{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backend.SDLWrap (FromSDLError, Texture, SDL
                       ,textureDimensions, loadTexture
                       ,renderImage, update
                       ,renderSDL, runSDL, clear) where

import           Backend.Internal.SDL     (FromSDLError)
import qualified Backend.Internal.SDLWrap as I
import           Control.Applicative hiding ((<$>))
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Binary
import           Data.Picture
import qualified Foreign.C.Types as C
import qualified Philed.Data.NNeg as N
import           Philed.Data.Rect
import           Philed.Data.Vector

newtype SDL e m a =
  SDL { unSDL :: I.SDL e m a }
  deriving (Applicative,Functor,Monad,MonadCatch,MonadMask,MonadThrow
           ,MonadIO,MonadError e,MonadTrans)

newtype Texture = Texture { getTexture :: I.Texture } deriving (Binary, Eq, Show)

textureDimensions :: (MonadIO m, MonadError e m, FromSDLError e)
                     => Texture -> SDL e m (Word, Word)
textureDimensions = SDL . I.textureDimensions . getTexture

loadTexture :: (MonadError e m, MonadIO m, Monad m, FromSDLError e) =>
               FilePath -> SDL e m Texture
loadTexture = SDL . (Texture <$>) . I.loadTexture

renderImage :: (Monad m, MonadIO m, MonadError e m, FromSDLError e)
               => Texture -> Rect Word -> Word -> Word -> SDL e m ()
renderImage (Texture tex) srcRect x y = SDL (I.renderImage tex srcRect x y)

update :: (MonadIO m, MonadError e m, FromSDLError e) => SDL e m ()
update = SDL I.update

clear :: (MonadIO m, MonadError e m, FromSDLError e) => SDL e m ()
clear = SDL I.clear

runSDL :: (MonadIO m, MonadError e m, FromSDLError e) =>
          Vec Word -> Word -> Word -> SDL e m a -> m ()
runSDL bottomLeft w h (SDL sdl) = I.runSDL bottomLeft w h sdl

renderSDL :: (Monad m, MonadIO m, MonadError e m, FromSDLError e)
             => Picture Texture -> SDL e m ()
renderSDL (Image tex) = do
  (w,h) <- textureDimensions tex
  renderImage tex (mkRect (0,0) (N.abs w) (N.abs h)) 0 0
