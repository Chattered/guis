{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Main where

import Backend.SDLWrap (runSDL, Image, Texture)
import qualified Control.Concurrent as Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Morph
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

bracketHandle :: MonadSafe m => Handle -> m r -> m r
bracketHandle h b = bracket (return ()) (const . liftIO $ hClose h) (const b)

client :: MonadSafe m => Command Texture Image m () -> m ()
client cmd = do
  withFile "/disk/scratch/serverout" ReadMode $ \hin -> do
    withFile "/disk/scratch/serverin" WriteMode $ \hout -> do
      runEffect $ runClient hin hout (sdlClient cmd)

waitM :: MonadIO m => m Bool -> m ()
waitM cond = do
  c <- cond
  if c then return () else
    do liftIO . Concurrent.threadDelay $ 1000000
       liftIO $ Concurrent.yield
       waitM cond

main :: IO ()
main = do
  createNamedPipe "/disk/scratch/serverin"  (3 `shiftL` 7)
  createNamedPipe "/disk/scratch/serverout" (3 `shiftL` 7)
  runSDL (0,0) 200 200 . forever . h . runSafeT $ do
    withFile "/disk/scratch/serverin" ReadMode $ \hin -> do
      waitM (liftIO . hReady $ hin)
      withFile "/disk/scratch/serverout" WriteMode $ \hout -> do
        runEffect $ (hoist liftBase $ runServer hin hout sdlServer)
        liftIO $ putStrLn "bye"
  where h x = x `catch` (\e -> do liftIO . print $ (e::SomeException)
                                  return ())
