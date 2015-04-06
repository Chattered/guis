{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Remote.Pipes where

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
import Remote.Command

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

runClient :: (Binary img, Binary tex, MonadThrow m) =>
             Producer ByteString m () -> Consumer ByteString m ()
             -> Command tex img m () -> m ()
runClient source sink (Command p) =
  runEffect $ source >-> decodes >-> p >-> encodes >-> sink

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
