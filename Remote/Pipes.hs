{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Remote.Pipes (Command, clear, loadTexture, newImage, render, update
                    ,server, runClient) where

import qualified Data.Binary as B
import qualified Backend.SDLWrap as SDL
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans
import qualified Data.Binary as B
import Data.Picture
import Philed.Data.Rect
import Pipes
import Pipes.Binary
import Pipes.ByteString
import Pipes.Core
import Pipes.Safe

data Cmd img tex = Clear
                 | LoadTexture String
                 | NewImage tex (Rect Word)
                 | Render (Picture img)
                 | Update
                 deriving Functor

-------------------------------------------------------------------------------------

instance (Binary img, Binary tex) => Binary (Cmd img tex) where
  put Clear               = B.put (0::Word8)
  put (LoadTexture path)  = B.put (1::Word8) >> B.put path
  put (NewImage tex rect) = B.put (2::Word8) >> B.put tex >> B.put rect
  put (Render pic)        = B.put (3::Word8) >> B.put pic
  put Update              = B.put (4::Word8)
  get = do
    discriminator <- B.get :: B.Get Word8
    case discriminator of
     0 -> return Clear
     1 -> LoadTexture <$> B.get
     2 -> NewImage <$> B.get <*> B.get
     3 -> Render <$> B.get
     4 -> return Update

data Response img tex = None
                      | Img img
                      | Tex tex

instance (Binary img, Binary tex) => Binary (Response img tex) where
  put None      = B.put (0::Word8)
  put (Tex tex) = B.put (1::Word8) >> B.put tex
  put (Img img) = B.put (2::Word8) >> B.put img
  get = do
    discriminator <- (B.get::B.Get Word8)
    case discriminator of
     0 -> return None
     1 -> Tex <$> B.get
     2 -> Img <$> B.get

-------------------------------------------------------------------------------------

newtype Command img tex m a = Command (Pipe (Response img tex) (Cmd img tex) m a)
                            deriving (Functor, Applicative, Monad, MonadThrow,
                                      MonadCatch)

clear :: Monad m => Command img tex m ()
clear = Command . yield $ Clear

loadTexture :: MonadThrow m =>
               FilePath -> Command img tex m tex
loadTexture file = Command $ do
  yield (LoadTexture file)
  x <- await
  case x of
   Tex t -> return t
   _     -> throwM (userError "server should have returned a texture")

newImage :: MonadThrow m => tex -> Rect Word -> Command img tex m img
newImage texture rect = Command $ do
  yield (NewImage texture rect)
  x <- await
  case x of
   Img img -> return img
   _       -> throwM (userError "server should have returned a image")

render :: Monad m => Picture img -> Command img tex m ()
render picture = Command . yield $ Render picture

update :: Monad m => Command img tex m ()
update = Command . yield $ Update

-------------------------------------------------------------------------------------

runCmd :: (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
          Cmd SDL.Image SDL.Texture -> SDL.SDL e m (Response SDL.Image SDL.Texture)
runCmd Clear                  = SDL.clear *> pure None
runCmd (LoadTexture filePath) = Tex <$> SDL.loadTexture filePath
runCmd (NewImage tex rect)    = Img <$> SDL.newImage tex rect
runCmd (Render pic)           = SDL.renderSDL pic *> pure None
runCmd (Update)               = SDL.update *> pure None

runCmds :: (MonadError e m, MonadIO m, SDL.FromSDLError e)
           => Pipe (Cmd SDL.Image SDL.Texture) (Response SDL.Image SDL.Texture)
           (SDL.SDL e m) ()
runCmds = do
  cmd <- request ()
  yield <=< lift . runCmd $ cmd

-------------------------------------------------------------------------------------

runClient :: MonadThrow m =>
             Command SDL.Image SDL.Texture m () -> Pipe ByteString ByteString m ()
runClient (Command p) = decodes >-> p >-> encodes

server :: (MonadThrow m, MonadIO m, MonadError e m, SDL.FromSDLError e) =>
          Pipe ByteString ByteString (SDL.SDL e m) ()
server = decodes >-> runCmds >-> encodes

-------------------------------------------------------------------------------------

decodes :: (B.Binary a, MonadThrow m) => Pipe ByteString a m ()
decodes = feedback decoder
  where decoder p = do
          (x, leftover) <- lift . runStateT decode $ p
          case x of
           Right x  -> yield x >> decoder p
           Left err -> throwM (userError . deMessage $ err)

encodes :: (B.Binary a, Monad m) => Pipe a ByteString m ()
encodes = await >>= encode

feedback :: Monad m => (Producer a m () -> Producer b m ()) -> Pipe a b m ()
feedback f = go
  where
    go = do
      x <- await
      go2 (f $ yield x)
    go2 p = do
          y <- lift $ next p
          case y of
           Left ()             -> go
           Right (z, leftOver) -> do
             yield z
             go2 leftOver
