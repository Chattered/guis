module Backend.SDLRenderer where

import ToBeDeprecated
import Backend.SDLWrap
import Control.Applicative hiding ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (liftM)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Picture
import qualified Foreign.C.Types as C
import Philed.Control.Monad (loopM)
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
now = (/ 10000000000) <$> fromIntegral <$> liftIO getCPUTime

million :: Int
million = 10^6

waitTill :: MonadIO m => Double -> m ()
waitTill x = if x <= 0 then return ()
             else liftIO $ threadDelay (round x * million)

frameFreq :: Double
frameFreq = 1/60

simulateFreq :: Double
simulateFreq = 1/60

playSDL :: (Monad m, MonadIO m, MonadError e m, FromSDLError e)
           => Vec (NNeg C.CInt) -> NNeg C.CInt -> NNeg C.CInt
           -> world
           -> (world -> Picture Texture)
           -> (Double -> Event -> world -> world)
           -> m ()
playSDL bottomLeft w h initWorld render step =
  now >>= \startTime -> runSDL bottomLeft w h $ do

    renderSDL (render initWorld)

    void $ flip loopM (startTime,initWorld) $
      \(currentTime,world) -> do

      -- Get the current time
      playTime <- now
      let stepTime = playTime - currentTime

      -- If necessary, wait until we're ready for the next simulation update
      waitTill (stepTime - simulateFreq)

      let nextWorld = step simulateFreq NoEvent world

      -- Are we due to render a frame?
      if (stepTime >= frameFreq)
        then renderSDL (render nextWorld)
        else return ()

      -- Loop
      return (playTime,nextWorld)
