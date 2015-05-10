{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Remote.Pipes (Command, clear, loadTexture, render, update
                    ,runServer, runClient, sdlClient, sdlServer) where

import qualified Control.Concurrent as Concurrent
import qualified Backend.SDLWrap as SDL
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
import Data.Word
import Philed.Data.Rect
import Philed.Control.Monad
import Pipes
import Pipes.Binary
import Pipes.ByteString
import Pipes.Core
import Pipes.Safe
import qualified System.IO as IO

data Cmd tex cmd = Clear cmd
                 | LoadTexture FilePath (tex -> cmd)
                 | Render (Picture tex) cmd
                 | Update cmd
                 deriving Functor

newtype Command tex m a =
  Command { runCommand :: FreeT (Cmd tex) m a }
  deriving (Applicative, Functor, Monad, MonadFree (Cmd tex), MonadTrans
           ,MonadIO)

clear :: Monad m => Command tex m ()
clear = liftF (Clear ())

loadTexture :: Monad m => FilePath -> Command tex m tex
loadTexture file = join . liftF $ LoadTexture file return

render :: Monad m => Picture tex -> Command tex m ()
render pic = liftF (Render pic ())

update :: Monad m => Command tex m ()
update = liftF (Update ())

data Response tex = Tex tex
                  | Bye
                  deriving (Eq,Show)

data C tex = Clr | LoadTex FilePath | Rnder (Picture tex) | Upd
           deriving (Eq,Show)

data ResponseCmd tex m =
  ResponseCmd [C tex] (Maybe (Response tex -> m (ResponseCmd tex m)))

split :: Monad m => FreeT (Cmd tex) m () -> m (ResponseCmd tex m)
split cmd = runFreeT cmd >>= s
  where s (Pure ()) = return $ ResponseCmd [] Nothing
        s (Free (Clear cmd)) = fmap f (split cmd)
          where f (ResponseCmd init cs) = ResponseCmd (Clr : init) cs
        s (Free (LoadTexture file cmd)) =
          return $ ResponseCmd [LoadTex file] (Just f)
          where f (Tex tex) = split (cmd tex)
        s (Free (Render pic cmd)) = fmap f (split cmd)
          where f (ResponseCmd init cs) = ResponseCmd (Rnder pic : init) cs
        s (Free (Update cmd)) = fmap f (split cmd)
          where f (ResponseCmd init cs) = ResponseCmd (Upd : init) cs

-------------------------------------------------------------------------------------

instance Binary tex => Binary (C tex) where
  put Clr               = B.put (0::Word8)
  put (LoadTex path)    = B.put (1::Word8) >> B.put path
  put (Rnder pic)       = B.put (2::Word8) >> B.put pic
  put Upd               = B.put (3::Word8)
  get = do
    discriminator <- B.get :: B.Get Word8
    case discriminator of
     0 -> return Clr
     1 -> LoadTex <$> B.get
     2 -> Rnder   <$> B.get
     3 -> return Upd

instance Binary tex => Binary (Response tex) where
  put (Tex tex) = B.put (0::Word8) >> B.put tex
  put Bye       = B.put (1::Word8)
  get = do
    discriminator <- (B.get::B.Get Word8)
    case discriminator of
     0 -> Tex <$> B.get
     1 -> return Bye

-------------------------------------------------------------------------------------

sdlClient :: Monad m =>
             Command tex m ()
             -> Client [C tex] (Response tex) m ()
sdlClient = join . lift . fmap sdlClient' . split . runCommand

sdlClient' :: Monad m =>
              ResponseCmd tex m
              -> Client [C tex] (Response tex) m ()
sdlClient' (ResponseCmd inits Nothing) = do
  resp <- request inits
  case resp of
   Bye -> return ()
sdlClient' (ResponseCmd inits (Just cmd)) = do
  resp <- request inits
  lift (cmd resp) >>= sdlClient'

sdlServer :: (Monad m, MonadIO m, MonadError e m, SDL.FromSDLError e) =>
             [C SDL.Texture]
             -> Server [C SDL.Texture] (Response SDL.Texture)
                       (SDL.SDL e m) (Response SDL.Texture)
sdlServer cmds = runCmds cmds >>= go
  where go Nothing     = return Bye
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
          C SDL.Texture -> SDL.SDL e m (Maybe (Response SDL.Texture))
runCmd Clr                = SDL.clear *> pure Nothing
runCmd (LoadTex filePath) = Just . Tex <$> SDL.loadTexture filePath
runCmd (Rnder pic)        = SDL.renderSDL pic *> pure Nothing
runCmd Upd                = SDL.update *> pure Nothing

-------------------------------------------------------------------------------------

encodeStrict :: Binary a => a -> BS.ByteString
encodeStrict x = BSL.toStrict (B.encode x)

decodeStrict :: Binary a => BS.ByteString -> a
decodeStrict bs = B.decode (BSL.fromStrict bs)

waitM :: MonadIO m => m Bool -> m ()
waitM cond = do
  c <- cond
  if c then return () else
    do liftIO $ Concurrent.yield
       waitM cond

readBS :: (Show a, Binary a, MonadIO m) => IO.Handle -> m a
readBS hin = do
  waitM (liftIO . IO.hReady $ hin)
  liftIO . putStrLn $ "reading from " ++ show hin
  bs <- liftIO . BS.hGet hin $ 4
  let payload = decodeStrict bs
  liftIO . putStrLn $ "Payload size reported as " ++ show payload
  bs <- liftIO (BS.hGet hin (fromIntegral (payload::Word32)))
  liftIO . print $ "Read " ++ show bs
  let x = decodeStrict bs
  liftIO . print $ "Read " ++ show x
  return x
--  decodeStrict <$> liftIO (BS.hGet hin (fromIntegral (payload::Word32)))

writeBS :: (Binary a, MonadIO m, Show a, Eq a) => IO.Handle -> a -> m ()
writeBS hout x = liftIO $ do
  liftIO . putStrLn $ "writing " ++ show x
  liftIO . putStrLn $ "on " ++ show hout
  let bs = encodeStrict x
  let payload = encodeStrict (fromIntegral (BS.length bs) :: Word32)
  liftIO . putStrLn $ "payload size computed at " ++ show (BS.length bs)
  liftIO . putStrLn $ "Write " ++ show bs
  liftIO . print $ (decodeStrict bs == x)
  BS.hPut hout (payload `BS.append` bs)
  IO.hFlush hout

runClient :: (Binary a, Binary b, MonadIO m, Show a, Show b, Eq a, Eq b) =>
             IO.Handle -> IO.Handle -> Client a b m () -> Effect m ()
runClient hin hout client = client //< \req -> writeBS hout req >> readBS hin

runServer :: (Binary a, Binary b, MonadIO m, Show a, Show b, Eq a, Eq b) =>
              IO.Handle -> IO.Handle -> (a -> Server a b m b) -> Effect m ()
runServer hin hout server = do
  ((readBS hin >>= server) //> \resp -> writeBS hout resp >> readBS hin)
  >>= writeBS hout
