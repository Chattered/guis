{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backend.SDLWrap (Texture, SDL
                       ,ofColour
                       ,textureDimensions, loadTexture, loadString, loadRect
                       ,module Backend.Internal.SDL
                       ,render, update, runSDL, clear) where

import           Control.Monad.Catch
import           Control.Monad.Except
import           Data.Binary
import qualified Data.Picture as P
import qualified Philed.Data.NNeg as N
import           Philed.Data.Rect
import           Philed.Data.Vector
import           Backend.Internal.SDL     (FromSDLError, fromSDLError, loadFont
                                          ,rgbaColour, Colour)
import qualified Backend.Internal.SDLWrap as I

newtype SDL e m a =
  SDL { unSDL :: I.SDL e m a }
  deriving (Applicative,Functor,Monad,MonadCatch,MonadMask,MonadThrow
           ,MonadIO,MonadError e,MonadTrans)

newtype Texture = Texture { getTexture :: I.Texture } deriving (Binary, Eq, Show)

ofColour :: P.Colour -> Colour
ofColour P.Red     = rgbaColour 0xff 0x0 0x0 0xff
ofColour P.Green   = rgbaColour 0x0 0xff 0x0 0xff
ofColour P.Yellow  = rgbaColour 0xff 0xff 0x0 0xff
ofColour P.Blue    = rgbaColour 0x0 0x0 0xff 0xff
ofColour P.Magenta = rgbaColour 0xff 0x0 0xff 0xff
ofColour P.Cyan    = rgbaColour 0x0 0xff 0xff 0xff
ofColour P.White   = rgbaColour 0xff 0xff 0xff 0xff

textureDimensions :: (MonadIO m, MonadError e m, FromSDLError e)
                     => Texture -> SDL e m (Word, Word)
textureDimensions = SDL . I.textureDimensions . getTexture

loadTexture :: (MonadError e m, MonadIO m, Monad m, FromSDLError e) =>
               FilePath -> SDL e m Texture
loadTexture = SDL . (Texture <$>) . I.loadTexture

loadString :: (MonadError e m, MonadIO m, Monad m, FromSDLError e) =>
              FilePath -> Int -> String -> Colour -> SDL e m Texture
loadString fontFile ptSize str col =
  SDL (Texture <$> I.loadString fontFile ptSize str col)

loadRect :: (MonadIO m, MonadError e m, FromSDLError e) =>
            Vec Word -> Colour -> SDL e m Texture
loadRect wh col = SDL (Texture <$> I.loadRect wh col)

update :: (MonadIO m, MonadError e m, FromSDLError e) => SDL e m ()
update = SDL I.update

clear :: (MonadIO m, MonadError e m, FromSDLError e) => SDL e m ()
clear = SDL I.clear

runSDL :: (MonadIO m, MonadError e m, FromSDLError e) =>
          Vec Word -> Word -> Word -> SDL e m a -> m ()
runSDL bottomLeft w h (SDL sdl) = I.runSDL bottomLeft w h sdl

render :: (Monad m, MonadIO m, MonadError e m, FromSDLError e)
            => P.Picture Texture -> SDL e m ()
render = renderAt (Transform 0 (N.suc N.zero,N.suc N.zero) (0,0) False)

data Transform = Transform { rotation    :: Double
                           , scaling     :: (N.NNeg Double,N.NNeg Double)
                           , translation :: Vec Double
                           , reflectH    :: Bool
                           }

translate :: Vec Double -> Transform -> Transform
translate u (Transform rot (sx,sy) v reflectH) =
  Transform rot (sx,sy) (u +. v) reflectH

rotate :: Double -> Transform -> Transform
rotate rot (Transform rot' (sx,sy) (ux,uy) reflectH) =
  Transform (rot + rot') (sx,sy) (vx,vy) reflectH
  where vx =    cos rot * ux + sin rot * uy
        vy = -sin rot * ux + cos rot * uy

scale :: (N.NNeg Double, N.NNeg Double) -> Transform -> Transform
scale (sx,sy) (Transform rot (sx',sy') (x,y) reflectH) =
  Transform rot (sx `N.times` sx',sy `N.times` sy')
  (N.extract sx*x,N.extract sy*y) reflectH

reflect :: Transform -> Transform
reflect (Transform rot (sx,sy) (x,y) reflect) =
  Transform (-rot) (sx,sy) (x,-y) (not reflect)

renderAt :: (Monad m, MonadIO m, MonadError e m, FromSDLError e)
             => Transform -> P.Picture Texture -> SDL e m ()
renderAt (Transform rot (sx,sy) (x,y) reflectH) (P.Image tex) = do
  (width,height) <- textureDimensions tex
  dx <- f sx width
  dy <- f sy height
  liftIO . putStrLn $
    "Rendering texture (" ++ show width ++ "," ++ show height ++ ")"
    ++ " at " ++ "(" ++ show x ++ "," ++ show y ++ ")"
    ++ " with new dimensions " ++ "(" ++ show dx ++ "," ++ show dy ++ ")"
  SDL $ I.renderTexture
    (getTexture tex)
    (mkRect (0,0) (N.fromWord width) (N.fromWord height))
    (mkRect (round x,round y) dx dy)
    (0,0)
    rot
    reflectH
  where f s x = if n <= fromIntegral (maxBound :: Word)
                then return (N.fromWord . fromIntegral $ n)
                else throwError (fromSDLError "scaling too large")
          where n :: Integer
                n = round (N.extract s) * fromIntegral x
renderAt t (P.Translate v p)          = renderAt (translate v t) p
renderAt t (P.Scale (sx,sy) p)        = renderAt (scale (sx,sy) t) p
renderAt t (P.Rotate (P.Angle rot) p) = renderAt (rotate rot t) p
renderAt t (P.ReflectHorizontal p)    = renderAt (reflect t) p
