{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.TextTile (Command, newGlyph, lay, remove, update, sleep, Env, withEnv
                     ,hoistCommand, C, runC, runCmd, Response
                     ,textTileResponseCmd) where

import           Control.Concurrent (threadDelay)
import           Control.Monad.Except
import           Control.Monad.Trans.Free
import           Data.Binary
import           GHC.Generics (Generic)
import           Philed.Data.NNeg
import           System.Environment (getEnv)

import qualified Backend.SDLWrap as SDL
import qualified Data.Picture as P
import qualified Remote.Pipes as P (ResponseCmd(..))

data Env = Env { w :: Word
               , h :: Word
               , eraser :: SDL.Texture
               , fontFile :: FilePath
               , fontSize :: Int
               }

data Cmd tile cmd = NewGlyph Char P.Colour (tile -> cmd)
                  | Lay tile (NNeg Integer) (NNeg Integer) cmd
                  | Remove (NNeg Integer) (NNeg Integer) cmd
                  | Update cmd
                  | Sleep cmd
                  deriving Functor

newtype Command tile m a =
  Command { runCommand :: FreeT (Cmd tile) m a }
  deriving (Applicative, Functor, Monad, MonadFree (Cmd tile), MonadTrans
           ,MonadIO)

newGlyph :: Monad m => P.Colour -> Char -> Command tile m tile
newGlyph col c = join . liftF $ NewGlyph c col return

lay :: Monad m => tile -> NNeg Integer -> NNeg Integer -> Command tile m ()
lay tile m n = liftF (Lay tile m n ())

remove :: Monad m => NNeg Integer -> NNeg Integer -> Command tile m ()
remove m n = liftF (Remove m n ())

update :: Monad m => Command tile m ()
update = liftF (Update ())

sleep :: Monad m => Command tile m ()
sleep = liftF (Sleep ())

data Response tile =
  Tile tile
  | Awake
  deriving (Eq,Generic,Show)

data C tile =
  NewG Char P.Colour
  | Lie tile (NNeg Integer) (NNeg Integer)
  | Rem (NNeg Integer) (NNeg Integer)
  | Upd
  | Slp
  deriving (Eq,Generic,Show)

split :: Monad m =>
         FreeT (Cmd tile) m () -> m (P.ResponseCmd (C tile) (Response tile) m)
split cmd = runFreeT cmd >>= s
  where s (Pure ()) = return $ P.ResponseCmd [] Nothing
        s (Free (NewGlyph c col cmd)) =
          return $ P.ResponseCmd [NewG c col] (Just f)
          where f (Tile tile) = split (cmd tile)
                f _           = error "BUG: Tile required."
        s (Free (Lay tile m n cmd)) = fmap f (split cmd)
          where f (P.ResponseCmd init cs) = P.ResponseCmd (Lie tile m n : init) cs
        s (Free (Remove m n cmd)) = fmap f (split cmd)
          where f (P.ResponseCmd init cs) = P.ResponseCmd (Rem m n : init) cs
        s (Free (Update cmd)) = fmap f (split cmd)
          where f (P.ResponseCmd init cs) = P.ResponseCmd (Upd : init) cs
        s (Free (Sleep cmd)) =
          return $ P.ResponseCmd [Slp] (Just f)
          where f Awake = split cmd
                f _     = error "BUG: Awake required"


withEnv :: (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
           (Env -> SDL.SDL e m r) -> SDL.SDL e m r
withEnv f = do
  fontDir <- liftIO (getEnv "fonts")
  let fontFile = (fontDir ++ "/share/fonts/truetype/DejaVuSansMono.ttf")
  let ptSize = 14
  dejavu <- SDL.loadFont fontFile ptSize
  x <- SDL.loadString fontFile ptSize "X" (SDL.rgbaColour 0xFF 0xFF 0xFF 0xFF)
  (w,h) <- SDL.textureDimensions x
  eraser <- SDL.loadRect (w,h) (SDL.rgbaColour 0x0 0x0 0x0 0xFF)
  f (Env w h eraser fontFile ptSize)

runCmd :: (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
          Env -> Command SDL.Texture m a -> SDL.SDL e m a
runCmd (Env w h eraser fontFile ptSize) = iterTM f . runCommand
  where f (NewGlyph c col k) =
          SDL.loadString fontFile ptSize [c] (SDL.ofColour col) >>= k
        f (Lay tile m n cmd) =
          SDL.render (P.Translate (vecOf m n) (P.Image tile)) *> cmd
        f (Remove m n cmd) =
          SDL.render (P.Translate (vecOf m n) (P.Image eraser)) *> cmd
        f (Update cmd) = SDL.update *> cmd
        f (Sleep cmd) = liftIO (threadDelay 500) *> cmd
        vecOf m n =
          ((fromIntegral . extract $ m) * fromIntegral w,
           (fromIntegral . extract $ n) * fromIntegral h)

runC :: (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
        Env -> C SDL.Texture -> SDL.SDL e m (Maybe (Response SDL.Texture))
runC (Env w h eraser fontFile ptSize) cmd = case cmd of
  NewG c col   ->
    Just . Tile <$> SDL.loadString fontFile ptSize [c] (SDL.ofColour col)
  Lie tile m n -> SDL.render (P.Translate (vecOf m n) (P.Image tile))
                  *> pure Nothing
  Rem m n      -> SDL.render (P.Translate (vecOf m n) (P.Image eraser))
                  *> pure Nothing
  Upd          -> SDL.update *> pure Nothing
  Slp          -> liftIO (threadDelay 500) *> pure (Just Awake)
  where vecOf m n =
          ((fromIntegral . extract $ m) * fromIntegral w,
           (fromIntegral . extract $ n) * fromIntegral h)

textTileResponseCmd :: Monad m =>
                       Command tile m ()
                       -> m (P.ResponseCmd (C tile) (Response tile) m)
textTileResponseCmd = split . runCommand

instance Binary tile => Binary (C tile)
instance Binary tile => Binary (Response tile)

hoistCommand :: (Monad m, Monad n) =>
                (forall a. m a -> n a) -> Command tile m a -> Command tile n a
hoistCommand f = Command . hoistFreeT f . runCommand
