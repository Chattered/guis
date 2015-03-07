{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Main where

import Backend.Internal.SDL (fromSDLError)
import Backend.SDLPlay
import Backend.SDLWrap
import Control.Exception
import Control.Monad.Except
import Data.Picture
import System.IO.Error
import qualified Philed.Data.NNeg as N

quitOnLeft :: Either () a -> IO a
quitOnLeft (Left ()) = throwIO (userError "Play over")
quitOnLeft (Right x) = return x

play :: SDL IOException IO ()
play = do
  tex <- loadTexture "Assets/Foo.bmp"
  playSDL quitOnLeft () (const $ Image tex)
          (\dt _ () -> Left ())

main :: IO ()
main = runSDL (N.zero,N.zero) (N.abs 200) (N.abs 200) play
