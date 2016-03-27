{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- module Remote.World (Command, newGlyph, lay, remove, update, sleep
--                     ,P.runServer, P.runClient, textClient, textServer
--                     ,hoistCommand) where

import           Control.Concurrent (threadDelay)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Free
import           Data.Binary
import qualified Data.Bits as B
import           Data.Array.IO
import           Data.Word
import           GHC.Generics (Generic)
import qualified Graphics.UI.SDL.TTF.FFI as SDL (TTFFont)
import           Pipes.Binary
import           Pipes.ByteString
import           Pipes.Core
import           System.Environment (getEnv)

import qualified Backend.SDLWrap as SDL
import qualified Data.Picture as P
import qualified Remote.Pipes as P

data Square = E | R1 | R2 | R3 | H | V | D1 | D2
            deriving (Enum,Eq,Generic,Show)

type Width  = (Square, Square)
type Length = (Square, Square)
type Depth  = (Square, Word8)

squareToWord32 :: Square -> Word32
squareToWord32 = fromIntegral . fromEnum

widthToWord32 :: Width -> Word32
widthToWord32 (x,y) = (squareToWord32 x `B.shiftL` 6) B..|. squareToWord32 y

depthToWord32 :: Depth -> Word32
depthToWord32 (x,y) = (squareToWord32 x `B.shiftL` 3) B..|. fromIntegral y

coordToWord32 :: Width -> Length -> Depth -> Word32
coordToWord32 x y z =
  (widthToWord32 x `B.shiftL` 18)
  B..|. widthToWord32 y `B.shiftL` 6
  B..|. depthToWord32 z

data Cmd cmd = Get Width Length Depth (Word32 -> cmd)
             | Set Width Length Depth Word32 cmd
             | DoneTurn cmd
             deriving Functor

newtype Command m a =
  Command { runCommand :: FreeT Cmd m a }
  deriving (Applicative, Functor, Monad, MonadFree Cmd, MonadTrans, MonadIO)

get :: Monad m => Width -> Length -> Depth -> Command m Word32
get x y z = join . liftF $ Get x y z return

set :: Monad m => Width -> Length -> Depth -> Word32 -> Command m ()
set x y z content = liftF (Set x y z content ())

done :: Monad m => Command m ()
done = liftF (DoneTurn ())

data Response = Got Word32 | Awake deriving (Eq,Generic,Show)

data C =
  G Width Length Depth
  | S Width Length Depth Word32
  | D deriving (Eq,Generic,Show)

split :: Monad m =>
         FreeT Cmd m () -> m (P.ResponseCmd C Response m)
split cmd = runFreeT cmd >>= s
  where s (Pure ()) = return $ P.ResponseCmd [] Nothing
        s (Free (Get x y z cmd)) =
          return $ P.ResponseCmd [G x y z] (Just f)
          where f (Got content) = split (cmd content)
                f _             = error "BUG: Content required."
        s (Free (Set x y z content cmd)) = fmap f (split cmd)
          where f (P.ResponseCmd init cs) = P.ResponseCmd (S x y z content : init) cs
        s (Free (DoneTurn cmd)) =
          return $ P.ResponseCmd [D] (Just f)
          where f Awake = split cmd
                f _     = error "BUG: Awake required"

textClient :: Monad m => Command m ()
             -> Client [C] (Maybe Response) m ()
textClient = join . lift . fmap P.client . split . runCommand

-- textServer :: MonadIO m =>
--               [C] -> Server [C SDL.Texture] (Maybe Response) m ()
-- textServer cmds = do
--   run <- lift $ do
--     newArray (0, 2^30 - 1)

-- textServer ::
--   (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
--   [C SDL.Texture]
--   -> Server [C SDL.Texture] (Maybe (Response SDL.Texture)) (SDL.SDL e m) ()
-- textServer cmds = do
--   run <- lift $ do
--     fontDir <- liftIO (getEnv "fonts")
--     let fontFile = (fontDir ++ "/share/fonts/truetype/DejaVuSansMono.ttf")
--     let ptSize = 14
--     dejavu <- SDL.loadFont fontFile ptSize
--     x <- SDL.loadString fontFile ptSize "X" (SDL.rgbaColour 0xFF 0xFF 0xFF 0xFF)
--     (w,h) <- SDL.textureDimensions x
--     eraser <- SDL.loadRect (w,h) (SDL.rgbaColour 0x0 0x0 0x0 0xff)
--     return (runCmd w h eraser (fontFile,ptSize))
--   P.server run cmds

--runCmd :: MonadIO m => IOUArray Word32 Word32 -> C -> m (Maybe Response)
--runCmd arr cmd = case cmd of
--  Get x y z ->
--    Just <$> readArray arr 
--   NewG c col   ->
--     Just . Tile <$> SDL.loadString fontFile ptSize [c] (SDL.ofColour col)
--   Lie tile m n -> SDL.render (P.Translate (vecOf m n) (P.Image tile))
--                   *> pure Nothing
--   Rem m n      -> SDL.render (P.Translate (vecOf m n) (P.Image eraser))
--                   *> pure Nothing
--   Upd          -> SDL.update *> pure Nothing
--   Slp          -> liftIO (threadDelay 100000) *> pure (Just Awake)
--   where vecOf m n =
--           ((fromIntegral . extract $ m) * fromIntegral w,
--            (fromIntegral . extract $ n) * fromIntegral h)

instance Binary C
instance Binary Response
instance Binary Square

hoistCommand :: (Monad m, Monad n) =>
                (forall a. m a -> n a) -> Command m a -> Command n a
hoistCommand f = Command . hoistFreeT f . runCommand
