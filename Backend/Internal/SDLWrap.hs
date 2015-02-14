{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Backend.Internal.SDLWrap where

import           ToBeDeprecated

import           Control.Applicative hiding ((<$>))
import           Control.Exception (catchJust,throwIO,fromException
                                   ,Exception,IOException,SomeException)
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Control.Monad.State as S
import           Data.Maybe (isJust,fromJust)
import           Data.Monoid
import           Data.Typeable
import qualified Foreign as C
import qualified Foreign.C.Types as C
import qualified Graphics.UI.SDL.Video as SDL (queryTexture,renderCopy)
import qualified Graphics.UI.SDL.Types as SDL
import qualified Philed.Data.Nat as Nat
import           Philed.Control.Monad.Record
import qualified Philed.Data.Nat as N
import qualified Backend.Internal.SDL as SDL

data Image = Image (Nat.Nat Int) (C.Ptr SDL.Rect) deriving (Eq,Ord)
newtype TextureCache = TextureCache { getTextures :: [SDL.Texture] } deriving Monoid

newtype SDL e m a =
  SDL { unSDL :: RecordT TextureCache
                   (ReaderT (String -> e)
                    (S.StateT SDL.Renderer (ExceptT e m))) a }
  deriving (Applicative,Functor,Monad,MonadIO,MonadReader (String -> e)
           ,MonadRecord TextureCache,S.MonadState SDL.Renderer,MonadError e)

instance MonadTrans (SDL e) where
  lift = SDL . lift . lift . lift . lift

newtype SDLError = SDLError { sdlErrMsg :: String }
                 deriving (Show,Typeable)
instance Exception SDLError where

fromSDLError :: SomeException -> Maybe SDLError
fromSDLError = fromException

peek :: MonadIO m => C.Ptr a -> m a
peek = liftIO . peek

poke :: MonadIO m => C.Ptr a -> a -> m (C.Ptr a)
poke ptr = liftIO . poke ptr

malloc :: MonadIO m => m (C.Ptr a)
malloc = liftIO malloc

liftCont :: ContT r IO a -> ReaderT u (ExceptT e (ContT r IO)) a
liftCont (ContT c) = ReaderT (\r -> ExceptT $ ContT $ (\k -> c (k . Right)))

alloca :: C.Storable a => ReaderT u (ExceptT e (ContT r IO)) (C.Ptr a)
alloca = liftCont (ContT C.alloca)

liftError :: (Monad m, MonadError e m) => m (Either e a) -> SDL e m a
liftError x = lift (x >>= either throwError return)

sdlCont :: (Monad m, MonadError e m, MonadIO m)
           => ReaderT (String -> e) (ExceptT e (ContT (Either e a) IO)) a
           -> SDL e m a
sdlCont x = ask >>= flip f x
  where
    f r = liftError . liftIO . flip runContT return . runExceptT . (`runReaderT` r)

textureDimensions :: (Monad m, MonadIO m, MonadError e m)
                     => SDL.Texture -> SDL e m (C.CInt, C.CInt)
textureDimensions tex = sdlCont $ do
    -- Pointers to be filled by query
    (wPtr,hPtr) <- liftA2 (,) alloca alloca

    (SDL.queryTexture tex
     <$> alloca <*> alloca <*> (pure wPtr) <*> (pure hPtr))
     >>= SDL.safeSDL_

    liftA2 (,) (peek wPtr) (peek hPtr)

loadImage :: (MonadError e m, MonadIO m, Monad m) => FilePath -> SDL e m Image
loadImage file = do

  renderer <- S.get
  tex      <- SDL.loadTexture file renderer

  index    <- Nat.length <$> getTextures <$> get

  (w,h)    <- textureDimensions tex

  rect     <- malloc
  poke rect (SDL.Rect 0 0 w h)

  record (TextureCache [tex])
  return (Image index rect)

sdlTexture :: Monad m => Image -> SDL e m SDL.Texture
sdlTexture (Image index _) = fromJust <$> (`N.lookup` index) <$> getTextures <$> get

renderImage :: (MonadError e m, MonadReader (String -> e) m, MonadIO m, Monad m)
               => Image -> C.CInt -> C.CInt -> SDL e m ()
renderImage img@(Image _ rect) x y = do
  SDL.Rect _ _ w h <- peek rect

  renderer <- S.get
  tex <- sdlTexture img

  sdlCont $ do
    destRect <- alloca
    poke destRect (SDL.Rect x y w h)
    SDL.safeSDL_ (SDL.renderCopy renderer tex rect destRect)
