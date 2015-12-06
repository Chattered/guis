{-# LANGUAGE RankNTypes #-}
module Backend.SDLPlay where

import Control.Concurrent (threadDelay)
import Control.Monad (liftM, unless, when)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import qualified Foreign.C.Types as C
import Philed.Control.Monad.Error (spinM)
import Philed.Data.Vector
import Philed.Data.NNeg
import System.CPUTime (getCPUTime)

import Data.Picture
import Backend.SDLWrap

ignore :: Monad m => m a -> m ()
ignore = liftM (const ())

type Time    = Double
data Event   = NoEvent

now :: MonadIO m => m Double
now = (/ 10^10) <$> fromIntegral <$> liftIO getCPUTime

million :: Num a => a
million = 10^6

waitTill :: MonadIO m => Double -> m ()
waitTill x = unless (x <= 0) (liftIO . threadDelay . round $ x * million)

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
playSDL nat initWorld renderWorld step =
  now >>= \startTime -> do

    render (renderWorld initWorld)

    ignore $ flip spinM (startTime,initWorld,0) $
      \(lastTime,world,i) -> do

      -- Get the current time
      playTime <- now
      let stepTime = playTime - lastTime

      -- If necessary, wait until we're ready for the next simulation update
      waitTill (simulateFreq - stepTime)

      nextWorld <- lift (nat (step simulateFreq NoEvent world))

      -- Are we due to render a frame?
      when (playTime + simulateFreq - stepTime  >= frameFreq)
        (clear >> render (renderWorld nextWorld) >> update)

      -- Loop
      return (playTime,nextWorld,i+1)
