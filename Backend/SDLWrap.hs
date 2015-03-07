{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backend.SDLWrap (textureDimensions, loadTexture, renderTexture, update
                       ,FromSDLError, Texture, SDL, runSDL) where

import           ToBeDeprecated
import           Backend.Internal.SDL     (FromSDLError)
import qualified Backend.Internal.SDLWrap as I
import           Control.Applicative hiding ((<$>))
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Foreign.C.Types as C
import           Philed.Data.NNeg
import           Philed.Data.Vector

newtype SDL e m a =
  SDL { unSDL :: I.SDL e m a }
  deriving (Applicative,Functor,Monad,MonadIO,MonadError e,MonadTrans)

newtype Texture = Texture { getTexture :: I.Image }

textureDimensions :: (MonadIO m, MonadError e m, FromSDLError e)
                     => Texture -> SDL e m (C.CInt, C.CInt)
textureDimensions = SDL . I.imageDimensions . getTexture

loadTexture :: (MonadError e m, MonadIO m, Monad m, FromSDLError e) =>
             FilePath -> SDL e m Texture
loadTexture = SDL . (Texture <$>) . I.loadImage

renderTexture :: (MonadIO m, MonadError e m, FromSDLError e) =>
                 Texture -> C.CInt -> C.CInt -> SDL e m ()
renderTexture tex x y = SDL (I.renderImage (getTexture tex) x y)

update :: (MonadIO m, MonadError e m, FromSDLError e) => SDL e m ()
update = SDL I.update

runSDL :: (MonadIO m, MonadError e m, FromSDLError e) =>
          Vec (NNeg C.CInt) -> NNeg C.CInt -> NNeg C.CInt -> SDL e m a -> m ()
runSDL bottomLeft w h (SDL sdl) = I.runSDL bottomLeft w h sdl
