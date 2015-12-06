{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Main where

import qualified Control.Concurrent as Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Bits
import qualified Data.ByteString as BS
import Philed.Data.Rect
import Philed.Data.NNeg
import Pipes
import Pipes.Binary hiding (get)
import Pipes.ByteString
import Pipes.Core
import Pipes.Prelude (drain)
import Pipes.Safe
import Prelude hiding (null)
import System.IO hiding (withFile)
import System.IO.Error
import System.Posix.Files

import Backend.SDLWrap (clear, runSDL, Texture)
import Data.Picture
import Remote.TextTile

withFile :: MonadSafe m => FilePath -> IOMode -> (Handle -> m r) -> m r
withFile file ioMode =
  bracket (liftIO $ openFile file ioMode) (liftIO . hClose)

bracketHandle :: MonadSafe m => Handle -> m r -> m r
bracketHandle h b = bracket (return ()) (const . liftIO $ hClose h) (const b)

client :: MonadSafe m => Command Texture m () -> m ()
client cmd =
  withFile "serverout" ReadMode $ \hin ->
    withFile "serverin" WriteMode $ \hout ->
      runEffect $ runClient hin hout (textClient cmd)

waitM :: MonadIO m => m Bool -> m ()
waitM cond = do
  c <- cond
  unless c $ do liftIO . Concurrent.threadDelay $ 1000000
                liftIO   Concurrent.yield
                waitM cond

main :: IO ()
main = do
  createNamedPipe "serverin"  (3 `shiftL` 7)
  createNamedPipe "serverout" (3 `shiftL` 7)
  runSDL (0,0) 200 200 $ do
    clear
    forever . h . runSafeT $ do
      withFile "serverin" ReadMode $ \hin -> do
        waitM (liftIO . hReady $ hin)
        withFile "serverout" WriteMode $ \hout -> do
          runEffect $ hoist liftBase $ runServer hin hout textServer
          liftIO $ putStrLn "bye"
  where h x = x `catch` (\e -> do liftIO . print $ (e::SomeException)
                                  return ())
