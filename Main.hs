{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Main where

import Backend.SDLWrap (runSDL, Image, Texture)
import Control.Concurrent as Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Bits
import qualified Data.ByteString as BS
import Pipes
import Pipes.Binary hiding (get)
import Pipes.ByteString
import Pipes.Core
import Pipes.Prelude (drain)
import Pipes.Safe
import Prelude hiding (null)
import Remote.Pipes
import System.IO hiding (withFile)
import System.IO.Error
import System.Posix.Files

import Data.Picture
import Philed.Data.Rect
import Philed.Data.NNeg

withFile :: MonadSafe m => FilePath -> IOMode -> (Handle -> m r) -> m r
withFile file ioMode =
  bracket (liftIO $ openFile file ioMode) (liftIO . hClose)

runClient :: MonadSafe m => Command Image Texture m () -> m ()
runClient cmd = do
  withFile "/disk/scratch/remoteout" ReadMode $ \hin -> do
    withFile "/disk/scratch/remotein" WriteMode $ \hout ->
      runEffect $ fromHandle hin >-> client cmd >-> toHandle hout

waitM :: MonadIO m => m Bool -> m ()
waitM cond = do
  c <- cond
  if c then return () else
    do liftIO . threadDelay $ 100000
       liftIO $ Concurrent.yield
       waitM cond

main :: IO ()
main = do
  createNamedPipe "/disk/scratch/remotein"  (3 `shiftL` 7)
  createNamedPipe "/disk/scratch/remoteout" (3 `shiftL` 7)
  runSDL (0,0) 200 200 . forever . h . runSafeT $ do
    withFile "/disk/scratch/remotein" ReadMode $ \hin -> do
      waitM (liftIO . hReady $ hin)
      (withFile "/disk/scratch/remoteout" WriteMode $ \hout -> do
          h . runEffect . hoist liftBase $
            fromHandle hin >-> server >-> toHandle hout)
        `catch` (\err -> do
                  liftIO . print $ err
                  if isDoesNotExistError err then
                    do
                      liftIO . putStrLn $ "discarding output"
                      h . runEffect . hoist liftBase $
                        fromHandle hin >-> server >-> drain
                  else throwM err)
        where h x = x `catch` (\e -> do liftIO . print $ (e::SomeException)
                                        return ())
