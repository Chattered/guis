module Backend.Internal.SDL where

import           Control.Applicative
import           Control.Exception (IOException)
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Data.Binary
import           Data.Bits
import           Data.Function
import           Data.List (sortBy)
import           Data.Monoid
import           Data.Picture
import           Data.Word
import qualified Foreign.C                  as C
import qualified Foreign                    as C
import           GHC.Generics (Generic)
import qualified Graphics.UI.SDL.TTF     as TTF
import           Graphics.UI.SDL.TTF.FFI (TTFFont)
import           Philed.Data.Rect
import           Philed.Data.Vector
import qualified SDL.Raw.Basic      as SDL
import qualified SDL.Raw.Enum       as SDL
import qualified SDL.Raw.Error      as SDL
import qualified SDL.Raw.Filesystem as SDL
import qualified SDL.Raw.Types      as SDL
import qualified SDL.Raw.Video      as SDL
import           System.ByteOrder
import           System.IO.Error (userError)

-------------------------------------------------------------------------------------

newtype Colour = Colour SDL.Color deriving (Eq, Show)

rgbaColour :: Word8 -> Word8 -> Word8 -> Word8 -> Colour
rgbaColour r g b a = Colour (SDL.Color r g b a)

instance Ord Colour where
  compare (Colour (SDL.Color r g b a)) (Colour (SDL.Color r' g' b' a')) =
    compare r r' <> compare g g' <> compare b b' <> compare a a'

instance Binary Colour where
  get = Colour <$> (SDL.Color <$> get <*> get <*> get <*> get)
  put (Colour (SDL.Color r g b a)) = put r *> put g *> put b *> put a

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
  if x /= C.nullPtr then pure x else handleSDLError

safeSDL_ :: (MonadIO m, MonadError e m, FromSDLError e)
            => m C.CInt -> m ()
safeSDL_ m = do
  result <- m
  unless (result == 0) handleSDLError

-------------------------------------------------------------------------------------

init :: (MonadIO m, MonadError e m, FromSDLError e) => m ()
init = safeSDL_ (SDL.init SDL.SDL_INIT_VIDEO) >> safeSDL_ (liftIO TTF.init)

quit :: (MonadIO m, MonadError e m, FromSDLError e) => m ()
quit = (liftIO TTF.quit) >> SDL.quit

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
  pure (window,renderer)
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

loadFont:: (MonadIO m, MonadError e m, FromSDLError e) =>
           FilePath -> Int -> m (C.Ptr ())
loadFont file ptSize = do
  safeSDL (liftIO (TTF.openFont file ptSize))

stringTexture :: (MonadIO m, MonadError e m, FromSDLError e)
                 => SDL.Renderer -> TTFFont -> String -> Colour
                 -> m SDL.Texture
stringTexture r f str (Colour c) =
  safeSDL (liftIO (TTF.renderUTF8Solid f str c)) >>= surfaceToTexture r

surfaceToTexture :: (MonadIO m, MonadError e m, FromSDLError e)
                    => SDL.Renderer -> C.Ptr SDL.Surface -> m SDL.Texture
surfaceToTexture renderer surface =
  (safeSDL . SDL.createTextureFromSurface renderer $ surface)
    <* SDL.freeSurface surface

rmask :: Word32
gmask :: Word32
bmask :: Word32
amask :: Word32
(rmask,gmask,bmask,amask) =
  case byteOrder of
  BigEndian -> (0xFF000000,0x00FF0000,0x0000FF00,0x000000FF)
  LittleEndian -> (0x000000FF,0x0000FF00,0x00FF0000,0xFF000000)
  Mixed (b1,b2,b3,b4) ->
    let [r,g,b,a] =
          map (0xFF `shiftL`) . map fst . sortBy (compare `on` snd)
          $ zip [24,16,8,0] [b1,b2,b3,b4] in
    (r,g,b,a)

rectTexture :: MonadIO m => SDL.Renderer -> Vec Word -> Colour -> m SDL.Texture
rectTexture renderer (w,h) (Colour (SDL.Color r g b a)) = do
  sPtr <- SDL.createRGBSurface 0 (fromIntegral w) (fromIntegral h)
       8
       rmask gmask bmask amask
  s <- liftIO . C.peek $ sPtr
  c <- SDL.mapRGBA (SDL.surfaceFormat s) r g b a
  SDL.fillRect sPtr C.nullPtr c
  SDL.createTextureFromSurface renderer sPtr <* SDL.freeSurface sPtr
