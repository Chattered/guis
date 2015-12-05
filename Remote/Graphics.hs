{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Remote.Graphics (Command, clear, loadTexture, render, update
                       ,P.runServer, P.runClient, sdlClient, sdlServer) where

import qualified Backend.SDLWrap as SDL
import           Control.Monad.Except
import           Control.Monad.Trans.Free
import qualified Data.Binary as B
import           Data.Picture
import           Pipes.Binary
import           Pipes.ByteString
import           Pipes.Core

import qualified Remote.Pipes as P

data Cmd tex cmd = Clear cmd
                 | LoadTexture FilePath (tex -> cmd)
                 | Render (Picture tex) cmd
                 | Update cmd
                 deriving Functor

newtype Command tex m a =
  Command { runCommand :: FreeT (Cmd tex) m a }
  deriving (Applicative, Functor, Monad, MonadFree (Cmd tex), MonadTrans
           ,MonadIO)

clear :: Monad m => Command tex m ()
clear = liftF (Clear ())

loadTexture :: Monad m => FilePath -> Command tex m tex
loadTexture file = join . liftF $ LoadTexture file return

render :: Monad m => Picture tex -> Command tex m ()
render pic = liftF (Render pic ())

update :: Monad m => Command tex m ()
update = liftF (Update ())

newtype Response tex = Tex tex deriving (Eq,Show)

data C tex = Clr | LoadTex FilePath | Rnder (Picture tex) | Upd
           deriving (Eq,Show)

split :: Monad m =>
         FreeT (Cmd tex) m () -> m (P.ResponseCmd (C tex) (Response tex) m)
split cmd = runFreeT cmd >>= s
  where s (Pure ()) = return $ P.ResponseCmd [] Nothing
        s (Free (Clear cmd)) = fmap f (split cmd)
          where f (P.ResponseCmd init cs) = P.ResponseCmd (Clr : init) cs
        s (Free (LoadTexture file cmd)) =
          return $ P.ResponseCmd [LoadTex file] (Just f)
          where f (Tex tex) = split (cmd tex)
        s (Free (Render pic cmd)) = fmap f (split cmd)
          where f (P.ResponseCmd init cs) = P.ResponseCmd (Rnder pic : init) cs
        s (Free (Update cmd)) = fmap f (split cmd)
          where f (P.ResponseCmd init cs) = P.ResponseCmd (Upd : init) cs

sdlClient :: Monad m => Command tex m ()
             -> Client [C tex] (Maybe (Response tex)) m ()
sdlClient = join . lift . fmap P.sdlClient . split . runCommand

sdlServer ::
  (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
  [C SDL.Texture]
  -> Server [C SDL.Texture] (Response SDL.Texture) (SDL.SDL e m) ()
sdlServer = P.sdlServer runCmd

runCmd :: (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
          C SDL.Texture -> SDL.SDL e m (Maybe (Response SDL.Texture))
runCmd Clr                = SDL.clear *> pure Nothing
runCmd (LoadTex filePath) = Just . Tex <$> SDL.loadTexture filePath
runCmd (Rnder pic)        = SDL.render pic *> pure Nothing
runCmd Upd                = SDL.update *> pure Nothing

-------------------------------------------------------------------------------------

instance Binary tex => Binary (C tex) where
  put Clr               = B.put (0::Word8)
  put (LoadTex path)    = B.put (1::Word8) >> B.put path
  put (Rnder pic)       = B.put (2::Word8) >> B.put pic
  put Upd               = B.put (3::Word8)
  get = do
    discriminator <- B.get :: B.Get Word8
    case discriminator of
     0 -> return Clr
     1 -> LoadTex <$> B.get
     2 -> Rnder   <$> B.get
     3 -> return Upd

instance Binary tex => Binary (Response tex) where
  put (Tex tex) = B.put (0::Word8) >> B.put tex
  get = do
    discriminator <- B.get::B.Get Word8
    case discriminator of
     0 -> Tex <$> B.get
