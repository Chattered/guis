{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backend.SDLWrap (FromSDLError, Texture, Image, SDL
                       ,textureDimensions, loadTexture, newImage
                       ,renderImage, update
                       ,renderSDL, runSDL, clear) where

import           Backend.Internal.SDL     (FromSDLError)
import qualified Backend.Internal.SDLWrap as I
import           Control.Applicative hiding ((<$>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Binary
import           Data.Picture
import qualified Foreign.C.Types as C
import           Philed.Data.NNeg
import           Philed.Data.Rect
import           Philed.Data.Vector

newtype SDL e m a =
  SDL { unSDL :: I.SDL e m a }
  deriving (Applicative,Functor,Monad,MonadIO,MonadError e,MonadTrans)

newtype Texture = Texture { getTexture :: I.Texture } deriving Binary
newtype Image   = Img     { getImage   :: I.Image } deriving Binary

textureDimensions :: (MonadIO m, MonadError e m, FromSDLError e)
                     => Texture -> SDL e m (Word, Word)
textureDimensions = SDL . I.textureDimensions . getTexture

loadTexture :: (MonadError e m, MonadIO m, Monad m, FromSDLError e) =>
               FilePath -> SDL e m Texture
loadTexture = SDL . (Texture <$>) . I.loadTexture

newImage :: (MonadIO m, FromSDLError e)
            => Texture -> Rect Word -> SDL e m Image
newImage (Texture tex) = SDL . (Img <$>) . I.newImage tex

renderImage :: (Monad m, MonadIO m, MonadError e m, FromSDLError e)
               => Image -> Word -> Word -> SDL e m ()
renderImage (Img img) x y = SDL (I.renderImage img x y)

update :: (MonadIO m, MonadError e m, FromSDLError e) => SDL e m ()
update = SDL I.update

clear :: (MonadIO m, MonadError e m, FromSDLError e) => SDL e m ()
clear = SDL I.clear

runSDL :: (MonadIO m, MonadError e m, FromSDLError e) =>
          Vec Word -> Word -> Word -> SDL e m a -> m ()
runSDL bottomLeft w h (SDL sdl) = I.runSDL bottomLeft w h sdl

renderAt :: (Monad m, MonadIO m, MonadError e m, FromSDLError e)
            => Picture Image -> Double -> Double -> SDL e m ()
renderAt (Image img) x y = renderImage img (round x) (round y)
renderAt (Translate (x',y') p) x y = renderAt p (x' + x) (y' + y)

renderSDL :: (Monad m, MonadIO m, MonadError e m, FromSDLError e)
             => Picture Image -> SDL e m ()
renderSDL pic = renderAt pic 0 0
