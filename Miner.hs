{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Miner where

import Control.Concurrent (threadDelay)
import Control.Applicative
import Control.Lens
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.IORef
import Data.Picture
import Philed.Data.NNeg
import Philed.Data.Vector

import Data.TextTile

fromJust :: Maybe a -> a
fromJust (Just x) = x

fromNum' :: Integer -> NNeg Integer
fromNum' = fromJust . fromNum

message :: MonadIO m => String -> Command tile m ()
message str = do
  glyphs <- mapM (newGlyph White) str
  mapM_ (\(g,n) -> lay g (fromNum' n) (fromNum' 0)) (zip glyphs [0..])

messageDelayed :: MonadIO m => String -> Command tile m ()
messageDelayed str = do
  glyphs <- mapM (newGlyph Red) str
  sequence_ [ lay g (fromNum' n) (fromNum' 1)
              >> sleep
              >> update
            | (g,n) <- zip glyphs [0..] ]

scroll :: MonadIO m => String -> Command tile m ()
scroll str = mapM (newGlyph Red) str >>= \gs -> mapM_ (scroll' gs) [0..20]
  where scroll' gs n = do
          remove (fromNum' n) (fromNum' 1)
          sequence [ remove (fromNum' m) (fromNum' 1)
                     >> lay g (fromNum' m) (fromNum' 1) | (g,m) <- zip gs [n+1..] ]
          sleep
          update

clearN :: MonadIO m => Integer -> Command tile m ()
clearN n = do
  sequence_ [ remove (fromNum' i) (fromNum' 1) | i <- [0..n] ]

data Direction = North | East | South | West deriving (Enum, Show)

vecOfDirection :: Direction -> Vec Integer
vecOfDirection North = (0,-1)
vecOfDirection East  = (1,0)
vecOfDirection South = (0,1)
vecOfDirection West  = (-1,0)

data GameState = GameState {
  _viewPortOrigin :: Vec Integer,
  _position :: Vec Integer
  }

makeLenses ''GameState

newtype MoveCommand tile m a =
  MoveCommand { unMoveCmd :: Command tile (ReaderT (IORef GameState, tile) m) a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance MonadIO m => MonadState GameState (MoveCommand tile m) where
  get   = MoveCommand (lift ask) >>= (liftIO . readIORef . fst)
  put x = MoveCommand (lift ask) >>= (liftIO . (`writeIORef` x) . fst)

move :: MonadIO m => Direction -> MoveCommand tile m ()
move dir = do
  hideMe
  p <- use position
  position .= p +. vecOfDirection dir
  drawMe

absolutePosition :: MonadIO m =>
                    MoveCommand tile m (Maybe (NNeg Integer, NNeg Integer))
absolutePosition = do
  (ax,ay) <- liftA2 (-.) (use position) (use viewPortOrigin)
  pure (liftA2 (,) (fromNum ax) (fromNum ay))

hideMe :: MonadIO m => MoveCommand tile m ()
hideMe = absolutePosition >>= maybe (pure ()) (MoveCommand . uncurry remove)

drawMe :: MonadIO m => MoveCommand tile m ()
drawMe = do
  (_, g) <- MoveCommand (lift ask)
  absolutePosition >>= maybe (pure ()) (MoveCommand . uncurry (lay g))

runMoveCommand :: MonadIO m => MoveCommand tile m a -> Command tile m a
runMoveCommand cmd = do
  stateRef <- liftIO (newIORef (GameState (-10,-20) (0,0)))
  g <- newGlyph Red '*'
  hoistCommand (\x -> runReaderT x (stateRef, g)) . unMoveCmd $ cmd

commit :: MonadIO m => MoveCommand tile m ()
commit = MoveCommand (sleep >> update)

haveFun :: MonadIO m => MoveCommand tile m ()
haveFun = sequence_ [ replicateM_ 10 (move d >> commit) | d <- cycle [North ..] ]
