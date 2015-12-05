module Remote.Pipes (runServer, runClient, sdlClient, sdlServer
                    ,ResponseCmd(..)) where

import qualified Control.Concurrent as Concurrent
import qualified Data.Binary as B
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Control.Monad
import Pipes
import Pipes.Binary
import Pipes.Core
import qualified System.IO as IO

data ResponseCmd c r m = ResponseCmd [c] (Maybe (r -> m (ResponseCmd c r m)))

sdlClient :: Monad m => ResponseCmd c r m -> Client [c] (Maybe r) m ()
sdlClient (ResponseCmd cs Nothing) = do
  resp <- request cs
  case resp of
   Nothing -> return ()
sdlClient (ResponseCmd cs (Just c)) = do
  Just resp <- request cs
  lift (c resp) >>= sdlClient

sdlServer :: Monad m =>
             (c -> m (Maybe r)) -> [c] -> Server [c] r m ()
sdlServer runCmd cmds = runCmds cmds >>= go
  where go Nothing     = return ()
        go (Just resp) = go <=< runCmds <=< respond $ resp
        runCmds []    = return Nothing
        runCmds [cmd] = do
          resp' <- lift . runCmd $ cmd
          case resp' of
           Nothing     -> return Nothing
           Just resp'' -> return (Just resp'')
        runCmds (cmd:cmds) = do
          resp' <- lift . runCmd $ cmd
          case resp' of
           Nothing -> runCmds cmds
           Just _  -> error "Not expecting a response."

encodeStrict :: Binary a => a -> BS.ByteString
encodeStrict = BSL.toStrict . B.encode

decodeStrict :: Binary a => BS.ByteString -> a
decodeStrict = B.decode  . BSL.fromStrict

waitM :: MonadIO m => m Bool -> m ()
waitM cond = do
  c <- cond
  unless c $ do liftIO Concurrent.yield
                waitM cond

readBS :: (Show a, Binary a, MonadIO m) => IO.Handle -> m a
readBS hin = do
  waitM (liftIO . IO.hReady $ hin)
  liftIO . putStrLn $ "reading from " ++ show hin
  bs <- liftIO . BS.hGet hin $ 4
  let payload = decodeStrict bs
  liftIO . putStrLn $ "Payload size reported as " ++ show payload
  bs <- liftIO (BS.hGet hin (fromIntegral (payload::Word32)))
  liftIO . print $ "Read " ++ show bs
  let x = decodeStrict bs
  liftIO . print $ "Read " ++ show x
  return x

writeBS :: (Binary a, MonadIO m, Show a, Eq a) => IO.Handle -> a -> m ()
writeBS hout x = liftIO $ do
  liftIO . putStrLn $ "writing " ++ show x
  liftIO . putStrLn $ "on " ++ show hout
  let bs = encodeStrict x
  let payload = encodeStrict (fromIntegral (BS.length bs) :: Word32)
  liftIO . putStrLn $ "payload size computed at " ++ show (BS.length bs)
  liftIO . putStrLn $ "Write " ++ show bs
  liftIO . print $ (decodeStrict bs == x)
  BS.hPut hout (payload `BS.append` bs)
  IO.hFlush hout

runClient :: (Binary a, Binary b, MonadIO m, Show a, Show b, Eq a, Eq b) =>
             IO.Handle -> IO.Handle -> Client a b m () -> Effect m ()
runClient hin hout client = client //< \req -> writeBS hout req >> readBS hin

runServer :: (Binary a, Binary b, MonadIO m, Show a, Show b, Eq a, Eq b) =>
              IO.Handle -> IO.Handle -> (a -> Server a b m b) -> Effect m ()
runServer hin hout server =
  ((readBS hin >>= server) //> \resp -> writeBS hout resp >> readBS hin)
  >>= writeBS hout
