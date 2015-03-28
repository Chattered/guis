{-# LANGUAGE RankNTypes #-}
module Backend.SDLPlay where

import ToBeDeprecated
import Backend.SDLWrap
import Control.Applicative hiding ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (liftM)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Data.Picture
import qualified Foreign.C.Types as C
import Philed.Control.Monad.Error (loopM)
import Philed.Data.Vector
import Philed.Data.NNeg
import System.CPUTime (getCPUTime)

void :: Monad m => m a -> m ()
void x = liftM (const ()) x

renderAt :: (Monad m, MonadIO m, MonadError e m, FromSDLError e)
            => Picture Texture -> Double -> Double -> SDL e m ()
renderAt (Image tex) x y = renderTexture tex (round x) (round y)
renderAt (Translate (x',y') p) x y = renderAt p (x' + x) (y' + y)

renderSDL :: (Monad m, MonadIO m, MonadError e m, FromSDLError e)
          => Picture Texture -> SDL e m ()
renderSDL pic = renderAt pic 0 0

type Time    = Double
data Event   = NoEvent

now :: MonadIO m => m Double
now = (/ 10^10) <$> fromIntegral <$> liftIO getCPUTime

million :: Num a => a
million = 10^6

waitTill :: MonadIO m => Double -> m ()
waitTill x = if x <= 0 then return ()
             else liftIO . threadDelay . round $ x * million

frameFreq :: Double
frameFreq = 1/60

simulateFreq :: Double
simulateFreq = 1/60

playSDL :: (MonadError e m, FromSDLError f, MonadIO n, MonadError f n)
           => (forall a. m a -> n a)
           -> world
           -> (world -> Picture Texture)
           -> (Double -> Event -> world -> m world)
           -> SDL f n ()
playSDL nat initWorld render step =
  now >>= \startTime -> do

    renderSDL (render initWorld)

    void $ flip loopM (startTime,initWorld,0) $
      \(lastTime,world,i) -> do

      -- Get the current time
      playTime <- now
      let stepTime = playTime - lastTime

      -- If necessary, wait until we're ready for the next simulation update
      waitTill (simulateFreq - stepTime)

      nextWorld <- lift (nat (step simulateFreq NoEvent world))

      -- Are we due to render a frame?
      if (playTime + simulateFreq - stepTime  >= frameFreq)
        then clear >> renderSDL (render nextWorld) >> update
        else return ()

      -- Loop
      return (playTime,nextWorld,i+1)
