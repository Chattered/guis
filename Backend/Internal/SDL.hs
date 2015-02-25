{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleContexts #-}
module Backend.Internal.SDL where

import ToBeDeprecated

import qualified Codec.Picture         as JC
import qualified Codec.Picture.Types   as JC
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Data.Bits
import           Data.Word
import qualified Foreign.C             as C
import qualified Foreign               as C
import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.UI.SDL.Enum  as SDL
import qualified Graphics.UI.SDL.Types as SDL
import qualified Philed.Data.Nat       as N

-------------------------------------------------------------------------------------

class FromSDLError e where
  fromSDLError :: String -> e

throwSDLError :: (MonadError e m, FromSDLError e) => String -> m a
throwSDLError = throwError . fromSDLError

handleSDLError :: (MonadIO m, MonadError e m, FromSDLError e) => m a
handleSDLError = do
  cErrMsg <- liftIO                 $ SDL.getError
  errMsg  <- liftIO . C.peekCString $ cErrMsg
  throwSDLError errMsg

safeSDL :: (MonadIO m, MonadError e m, FromSDLError e)
           => m (C.Ptr a) -> m (C.Ptr a)
safeSDL m = do
  x <- m
  if (x == C.nullPtr) then return x else handleSDLError

safeSDL_ :: (MonadIO m, MonadError e m, FromSDLError e)
            => m C.CInt -> m ()
safeSDL_ m = do
  result <- m
  if (result == 0) then return () else handleSDLError

-------------------------------------------------------------------------------------

createWindow :: (MonadIO m, MonadError e m, FromSDLError e) =>
                N.Nat C.CInt -> N.Nat C.CInt -> N.Nat C.CInt -> N.Nat C.CInt
                -> m (SDL.Window, SDL.Renderer)
createWindow x y w h = do
  windowName <- liftIO . C.newCString $ ""
  window     <- SDL.createWindow windowName
                (N.toIntegral x) (N.toIntegral y)
                (N.toIntegral w) (N.toIntegral h)
                SDL.SDL_WINDOW_SHOWN
  renderer   <- safeSDL (SDL.getRenderer window)
  return (window,renderer)

loadTexture :: (MonadError e m, MonadIO m, FromSDLError e) =>
               FilePath -> SDL.Renderer -> m SDL.Texture
loadTexture img renderer = do
  jcImg   <- loadImg img
  surface <- jcToSurface jcImg
  tex     <- safeSDL (SDL.createTextureFromSurface renderer surface)
  SDL.freeSurface surface
  return tex

-------------------------------------------------------------------------------------

rgba8ToWord32 :: JC.PixelRGBA8 -> Word32
rgba8ToWord32 (JC.PixelRGBA8 r g b a) =
  (fromIntegral r `shiftL` 24)
  .|. (fromIntegral b `shiftL` 16)
  .|. (fromIntegral g `shiftL` 8)
  .|. fromIntegral a

loadImg :: (MonadIO m, MonadError e m, FromSDLError e) =>
           FilePath -> m (JC.Image JC.PixelRGBA8)
loadImg path = do
  img <- liftIO (JC.readImage path)
  case img of
   Right (JC.ImageRGBA8 img) -> return img
   Left str                  -> throwSDLError str
   _                         -> throwSDLError "Not an RGBA8 image"

jcToSurface :: (MonadIO m, MonadError e m, FromSDLError e) =>
               JC.Image JC.PixelRGBA8 -> m (C.Ptr SDL.Surface)
jcToSurface img = do
  surfacePtr <- safeSDL
                (SDL.createRGBSurface
                 0
                 (fromIntegral . JC.imageWidth  $ img)
                 (fromIntegral . JC.imageHeight $ img) 32
                 (255 `shiftL` 24) (255 `shiftL` 16) (255 `shiftL` 8) 255)
  pixelsPtr <- C.castPtr <$> SDL.surfacePixels <$> liftIO (C.peek surfacePtr)
  safeSDL_ (SDL.lockSurface surfacePtr)
  _ <- liftIO $ JC.pixelFoldM (\ptr _ _ p -> do
                                  C.poke ptr (rgba8ToWord32 p)
                                  return $ ptr `C.plusPtr` 4)
    pixelsPtr img
  return surfacePtr

surfaceToTexture :: MonadIO m => SDL.Renderer -> C.Ptr SDL.Surface -> m SDL.Texture
surfaceToTexture renderer surface = do
  SDL.createTextureFromSurface renderer surface <* SDL.freeSurface surface

-------------------------------------------------------------------------------------
