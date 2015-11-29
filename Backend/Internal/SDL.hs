module Backend.Internal.SDL where

import           Control.Exception (IOException)
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Data.Bits
import           Data.Picture
import           Philed.Data.Rect
import           Philed.Data.Vector
import           Data.Word
import qualified Foreign.C                  as C
import qualified Foreign                    as C
import qualified SDL.Raw.Basic      as SDL
import qualified SDL.Raw.Enum       as SDL
import qualified SDL.Raw.Error      as SDL
import qualified SDL.Raw.Filesystem as SDL
import qualified SDL.Raw.Types      as SDL
import qualified SDL.Raw.Video      as SDL
import           System.IO.Error (userError)

-------------------------------------------------------------------------------------

class FromSDLError e where
  fromSDLError :: String -> e

instance FromSDLError IOException where
  fromSDLError = userError

throwSDLError :: (MonadError e m, FromSDLError e) => String -> m a
throwSDLError = throwError . fromSDLError

handleSDLError :: (MonadIO m, MonadError e m, FromSDLError e) => m a
handleSDLError = do
  cErrMsg <- liftIO                   SDL.getError
  errMsg  <- liftIO . C.peekCString $ cErrMsg
  throwSDLError errMsg

safeSDL :: (MonadIO m, MonadError e m, FromSDLError e)
           => m (C.Ptr a) -> m (C.Ptr a)
safeSDL m = do
  x <- m
  if x /= C.nullPtr then return x else handleSDLError

safeSDL_ :: (MonadIO m, MonadError e m, FromSDLError e)
            => m C.CInt -> m ()
safeSDL_ m = do
  result <- m
  unless (result == 0) handleSDLError

-------------------------------------------------------------------------------------

init :: (MonadIO m, MonadError e m, FromSDLError e) => m ()
init = safeSDL_ (SDL.init SDL.SDL_INIT_VIDEO)

quit :: (MonadIO m, MonadError e m, FromSDLError e) => m ()
quit = SDL.quit

createWindow :: (MonadIO m, MonadError e m, FromSDLError e) =>
                Vec Word -> Word -> Word -> m (SDL.Window, SDL.Renderer)
createWindow bottomLeft w h = do
  windowName <- liftIO . C.newCString $ ""
  window     <- safeSDL $ SDL.createWindow windowName
                SDL.SDL_WINDOWPOS_UNDEFINED SDL.SDL_WINDOWPOS_UNDEFINED
                (fromIntegral w) (fromIntegral h)
                SDL.SDL_WINDOW_SHOWN
  renderer   <-
    safeSDL $ SDL.createRenderer window (-1) SDL.SDL_RENDERER_ACCELERATED
  return (window,renderer)
  where (x,y) = bottomLeft

destroyWindow :: MonadIO m => SDL.Window -> m ()
destroyWindow = SDL.destroyWindow

update :: (MonadIO m, MonadError e m, FromSDLError e) => SDL.Renderer -> m ()
update = SDL.renderPresent

clear :: (MonadIO m, MonadError e m, FromSDLError e) => SDL.Renderer -> m ()
clear = safeSDL_ . SDL.renderClear

-------------------------------------------------------------------------------------

loadTexture :: (MonadIO m, MonadError e m, FromSDLError e)
               => SDL.Renderer -> FilePath -> m SDL.Texture
loadTexture r =
  surfaceToTexture r <=< safeSDL . SDL.loadBMP <=< liftIO . C.newCString

surfaceToTexture :: (MonadIO m, MonadError e m, FromSDLError e)
                    => SDL.Renderer -> C.Ptr SDL.Surface -> m SDL.Texture
surfaceToTexture renderer surface =
  (safeSDL . SDL.createTextureFromSurface renderer $ surface)
    <* SDL.freeSurface surface

-------------------------------------------------------------------------------------
