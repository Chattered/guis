module Main where

import qualified Control.Concurrent as Concurrent
import Control.Monad
import Control.Monad.Except
import Control.Monad.Morph
import Data.Bits
import Pipes
import Pipes.Core
import Pipes.Safe
import Prelude hiding (null)
import System.IO hiding (withFile)
import System.Posix.Files
import Backend.SDLWrap (clear, runSDL, Texture)
import Data.TextTile

import Miner

import qualified Remote.Pipes as P
import qualified Backend.SDLWrap as SDL

withFile :: MonadSafe m => FilePath -> IOMode -> (Handle -> m r) -> m r
withFile file ioMode =
  bracket (liftIO $ openFile file ioMode) (liftIO . hClose)

bracketHandle :: MonadSafe m => Handle -> m r -> m r
bracketHandle h b = bracket (return ()) (const . liftIO $ hClose h) (const b)

client :: MonadSafe m => Command Texture m () -> m ()
client cmd =
  withFile "serverout" ReadMode $ \hin ->
    withFile "serverin" WriteMode $ \hout ->
      runEffect $ P.runClient hin hout (textClient cmd)

waitM :: MonadIO m => m Bool -> m ()
waitM cond = do
  c <- cond
  unless c $ do liftIO Concurrent.yield
                waitM cond

-- main :: IO ()
-- main = do
--   createNamedPipe "serverin"  (3 `shiftL` 7)
--   createNamedPipe "serverout" (3 `shiftL` 7)
--   runSDL (0,0) 640 480 $ do
--     clear
--     forever . h . runSafeT $ do
--       withFile "serverin" ReadMode $ \hin -> do
--         waitM (liftIO . hReady $ hin)
--         withFile "serverout" WriteMode $ \hout -> do
--           runEffect $ hoist liftBase $ P.runServer hin hout textServer
--           liftIO $ putStrLn "bye"
--   where h x = x `catch` (\e -> do liftIO . print $ (e::SomeException)
--                                   return ())
--         textServer = P.server (withEnv . flip runC)

textClient :: Monad m => Command tile m ()
              -> Client [C tile] (Maybe (Response tile)) m ()
textClient = join . lift . fmap P.client . textTileResponseCmd

main :: IO ()
main = SDL.runSDL (0,0) 320 240
       $ withEnv . flip runCmd $ scroll "Hello there with bells on the top"
