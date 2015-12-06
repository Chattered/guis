{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Remote.TextTile where

import           Control.Monad.Except
import           Control.Monad.Trans.Free
import qualified Data.Binary as B
import           GHC.Generics (Generic)
import qualified Graphics.UI.SDL.TTF.FFI as SDL (TTFFont)
import           Philed.Data.NNeg
import           Pipes.Binary
import           Pipes.ByteString
import           Pipes.Core
import           System.Environment (getEnv)

import qualified Backend.SDLWrap as SDL
import qualified Data.Picture as P
import qualified Remote.Pipes as P

data Cmd tile cmd = NewGlyph Char SDL.Colour (tile -> cmd)
                  | Lay tile (NNeg Integer) (NNeg Integer) cmd
                  | Remove (NNeg Integer) (NNeg Integer) cmd
                  | Update cmd
                  deriving Functor

newtype Command tile m a =
  Command { runCommand :: FreeT (Cmd tile) m a }
  deriving (Applicative, Functor, Monad, MonadFree (Cmd tile), MonadTrans
           ,MonadIO)

newGlyph :: Monad m => Char -> SDL.Colour -> Command tile m tile
newGlyph c col = join . liftF $ NewGlyph c col return

lay :: Monad m => tile -> NNeg Integer -> NNeg Integer -> Command tile m ()
lay tile m n = liftF (Lay tile m n ())

remove :: Monad m => NNeg Integer -> NNeg Integer -> Command tile m ()
remove m n = liftF (Remove m n ())

update :: Monad m => Command tile m ()
update = liftF (Update ())

newtype Response tile = Tile tile deriving (Eq,Generic,Show)

data C tile =
  NewG Char SDL.Colour
  | Lie tile (NNeg Integer) (NNeg Integer)
  | Rem (NNeg Integer) (NNeg Integer)
  | Upd
  deriving (Eq,Generic,Show)

split :: Monad m =>
         FreeT (Cmd tile) m () -> m (P.ResponseCmd (C tile) (Response tile) m)
split cmd = runFreeT cmd >>= s
  where s (Pure ()) = return $ P.ResponseCmd [] Nothing
        s (Free (NewGlyph c col cmd)) =
          return $ P.ResponseCmd [NewG c col] (Just f)
          where f (Tile tile) = split (cmd tile)
        s (Free (Lay tile m n cmd)) = fmap f (split cmd)
          where f (P.ResponseCmd init cs) = P.ResponseCmd (Lie tile m n : init) cs
        s (Free (Remove m n cmd)) = fmap f (split cmd)
          where f (P.ResponseCmd init cs) = P.ResponseCmd (Rem m n : init) cs
        s (Free (Update cmd)) = fmap f (split cmd)
          where f (P.ResponseCmd init cs) = P.ResponseCmd (Upd : init) cs

sdlClient :: Monad m => Command tile m ()
             -> Client [C tile] (Maybe (Response tile)) m ()
sdlClient = join . lift . fmap P.sdlClient . split . runCommand

sdlServer ::
  (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
  [C SDL.Texture]
  -> Server [C SDL.Texture] (Response SDL.Texture) (SDL.SDL e m) ()
sdlServer cmds = do
  run <- lift $ do
    fontDir <- liftIO (getEnv "fonts")
    dejavu <- SDL.loadFont (fontDir ++ "/share/fonts/truetype/DejaVuSansMono.ttf") 14
    x <- SDL.loadString dejavu "X" (SDL.rgbaColour 0xFF 0xFF 0xFF 0xFF)
    (w,h) <- SDL.textureDimensions x
    eraser <- SDL.loadRect (w,h) (SDL.rgbaColour 0x0 0x0 0x0 0xFF)
    return (runCmd eraser dejavu)
  P.sdlServer run cmds

runCmd :: (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
         SDL.Texture -> SDL.TTFFont
         -> C SDL.Texture -> SDL.SDL e m (Maybe (Response SDL.Texture))
runCmd eraser font cmd = case cmd of
  NewG c col   -> Just . Tile <$> SDL.loadString font [c] col
  Lie tile m n -> SDL.render (P.Translate (vecOf m n) (P.Image tile)) *> pure Nothing
  Rem m n      -> SDL.render (P.Translate (vecOf m n) (P.Image eraser))
                  *> pure Nothing
  Upd          -> SDL.update *> pure Nothing
  where vecOf m n = (fromIntegral . extract $ m, fromIntegral . extract $ n)

instance Binary tile => Binary (C tile)
instance Binary tile => Binary (Response tile)
