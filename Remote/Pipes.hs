{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Remote.Pipes (Command, clear, loadTexture, newImage, render, update
                    ,runServer, runClient, sdlClient, sdlServer) where

import qualified Backend.SDLWrap as SDL
import Control.Monad.Catch
import Control.Monad.Except
import qualified Control.Monad.Free as F
import Control.Monad.Trans.Free
import Control.Monad.State.Strict
import Control.Monad.Trans
import qualified Data.Binary as B
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Identity
import Data.Picture
import Philed.Data.Rect
import Philed.Control.Monad
import Pipes
import Pipes.Binary
import Pipes.ByteString
import Pipes.Core
import Pipes.Safe
import qualified System.IO as IO

data Cmd tex img cmd = Clear cmd
                     | LoadTexture FilePath (tex -> cmd)
                     | NewImage tex (Rect Word) (img -> cmd)
                     | Render (Picture img) cmd
                     | Update cmd
                     deriving Functor

newtype Command tex img m a =
  Command { runCommand :: FreeT (Cmd tex img) m a }
  deriving (Applicative, Functor, Monad, MonadFree (Cmd tex img))

clear :: Monad m => Command tex img m ()
clear = liftF (Clear ())

loadTexture :: Monad m => FilePath -> Command tex img m tex
loadTexture file = join . liftF $ LoadTexture file return

newImage :: Monad m => tex -> Rect Word -> Command tex img m img
newImage tex rect = join . liftF $ NewImage tex rect return

render :: Monad m => Picture img -> Command tex img m ()
render pic = liftF (Render pic ())

update :: Monad m => Command tex img m ()
update = liftF (Update ())

data Response tex img = Img img
                      | Tex tex
                      | Bye

data C tex img = Clr | LoadTex FilePath | NewImg tex (Rect Word)
               | Rnder (Picture img) | Upd

data ResponseCmd tex img m =
  ResponseCmd [C tex img] (Maybe (Response tex img -> m (ResponseCmd tex img m)))

split :: Monad m => FreeT (Cmd tex img) m () -> m (ResponseCmd tex img m)
split cmd = runFreeT cmd >>= s
  where s (Pure ()) = return $ ResponseCmd [] Nothing
        s (Free (Clear cmd)) = fmap f (split cmd)
          where f (ResponseCmd init cs) = ResponseCmd (Clr : init) cs
        s (Free (LoadTexture file cmd)) =
          return $ ResponseCmd [LoadTex file] (Just f)
          where f (Tex tex) = split (cmd tex)
                f (Img img) = error "Expecting texture from server."
        s (Free (NewImage tex rect cmd)) =
          return $ ResponseCmd [NewImg tex rect] (Just f)
          where f (Tex tex) = error "Expecting image from server."
                f (Img img) = split (cmd img)
        s (Free (Render pic cmd)) = fmap f (split cmd)
          where f (ResponseCmd init cs) = ResponseCmd (Rnder pic : init) cs
        s (Free (Update cmd)) = fmap f (split cmd)
          where f (ResponseCmd init cs) = ResponseCmd (Upd : init) cs

-------------------------------------------------------------------------------------

instance (Binary tex, Binary img) => Binary (C tex img) where
  put Clr               = B.put (0::Word8)
  put (LoadTex path)    = B.put (1::Word8) >> B.put path
  put (NewImg tex rect) = B.put (2::Word8) >> B.put tex >> B.put rect
  put (Rnder pic)       = B.put (3::Word8) >> B.put pic
  put Upd               = B.put (4::Word8)
  get = do
    discriminator <- B.get :: B.Get Word8
    case discriminator of
     0 -> return Clr
     1 -> LoadTex <$> B.get
     2 -> NewImg  <$> B.get <*> B.get
     3 -> Rnder   <$> B.get
     4 -> return Upd

instance (Binary img, Binary tex) => Binary (Response img tex) where
  put (Tex tex) = B.put (0::Word8) >> B.put tex
  put (Img img) = B.put (1::Word8) >> B.put img
  put Bye       = B.put (2::Word8)
  get = do
    discriminator <- (B.get::B.Get Word8)
    case discriminator of
     0 -> Tex <$> B.get
     1 -> Img <$> B.get
     2 -> return Bye

-------------------------------------------------------------------------------------

sdlClient :: Monad m =>
             Command tex img m ()
             -> Client [C tex img] (Response tex img) m ()
sdlClient = join . lift . fmap sdlClient' . split . runCommand

sdlClient' :: Monad m =>
              ResponseCmd tex img m -> Client [C tex img] (Response tex img) m ()
sdlClient' (ResponseCmd inits Nothing) = do
  resp <- request inits
  case resp of
   Bye -> return ()
sdlClient' (ResponseCmd inits (Just cmd)) = do
  resp <- request inits
  lift (cmd resp) >>= sdlClient'

sdlServer :: (Monad m, MonadIO m, MonadError e m, SDL.FromSDLError e) =>
             [C SDL.Texture SDL.Image]
             -> Server [C SDL.Texture SDL.Image] (Response SDL.Texture SDL.Image)
                       (SDL.SDL e m) ()
sdlServer cmds = runCmds cmds >>= go
  where go Nothing     = return ()
        go (Just resp) = go <=< runCmds <=< respond $ resp
        runCmds []    = return Nothing
        runCmds [cmd] = do
          resp' <- lift . runCmd $ cmd
          case resp' of
           Nothing     -> return Nothing
           Just resp'' -> return (Just resp'')
        runCmds (cmd:cmds) = do
          resp' <- lift . runCmd $ cmd
          case resp' of
           Nothing -> runCmds cmds
           Just _  -> error "Not expecting a response."

-------------------------------------------------------------------------------------

runCmd :: (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
          C SDL.Texture SDL.Image
          -> SDL.SDL e m (Maybe (Response SDL.Texture SDL.Image))
runCmd Clr                = SDL.clear *> pure Nothing
runCmd (LoadTex filePath) = Just . Tex <$> SDL.loadTexture filePath
runCmd (NewImg tex rect)  = Just . Img <$> SDL.newImage tex rect
runCmd (Rnder pic)        = SDL.renderSDL pic *> pure Nothing
runCmd Upd                = SDL.update *> pure Nothing

-------------------------------------------------------------------------------------

encodeStrict :: Binary a => a -> BS.ByteString
encodeStrict x = BSL.toStrict (B.encode x)

decodeStrict :: Binary a => BS.ByteString -> a
decodeStrict bs = B.decode (BSL.fromStrict bs)

readBS :: (Binary a, MonadIO m) => IO.Handle -> m a
readBS hin = do
  payload <- decodeStrict <$> liftIO (BS.hGet hin 4)
  decodeStrict <$> liftIO (BS.hGet hin payload)

runClient :: (Binary a, Binary b, MonadIO m) =>
             IO.Handle -> IO.Handle -> Client a b m () -> Effect m ()
runClient hin hout client = do
  client //< \req -> do
    let bs = encodeStrict req
    liftIO (BS.hPut hout (encodeStrict (BS.length bs) `BS.append` bs))
    liftIO (IO.hFlush hout)
    readBS hin

runServer :: (Binary a, Binary b, MonadIO m) =>
             IO.Handle -> IO.Handle -> (b -> Server a b m ()) -> Effect m ()
runServer hout hin server = do
  req <- readBS hin
  runClient hout hin (reflect . server $ req)
