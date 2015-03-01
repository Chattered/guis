{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts
    , GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Backend.Internal.SDLWrap (textureDimensions, loadImage, renderImage
                                ,Image, imageDimensions, SDL, runSDL) where

import           ToBeDeprecated

import qualified Backend.Internal.SDL as SDL
import           Control.Applicative hiding ((<$>))
import           Control.Exception (catchJust,throwIO,fromException
                                   ,Exception,IOException,SomeException)
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Control.Monad.State as S
import           Control.Exception
import           Data.IORef
import           Data.Maybe (isJust,fromJust)
import           Data.Monoid
import           Data.Typeable
import qualified Foreign as C
import qualified Foreign.C.Types as C
import qualified Graphics.UI.SDL.Video as SDL (queryTexture,renderCopy)
import qualified Graphics.UI.SDL.Types as SDL
import           Philed.Control.Monad.Record
import qualified Philed.Data.NNeg as N
import           Philed.Data.Rect
import           Philed.Data.Vector

-------------------------------------------------------------------------------------

data Image = Image (N.NNeg Int) (C.Ptr SDL.Rect) deriving (Eq,Ord)
newtype TextureCache = TextureCache { getTextures :: [SDL.Texture] } deriving Monoid

data SDLState = SDLState { _renderer :: SDL.Renderer }
makeLenses ''SDLState

newtype SDL e m a =
  SDL { unSDL :: ReaderT (IORef (SDLState,TextureCache)) m a }
  deriving (Applicative,Functor,Monad,MonadIO,MonadError e,MonadTrans)

instance MonadIO m => S.MonadState SDLState (SDL e m) where
  get   = (^. _1) <$> (SDL ask >>= liftIO . readIORef)
  put x = SDL ask >>= liftIO . (`modifyIORef` (_1 .~ x))

instance MonadIO m => MonadRecord TextureCache (SDL e m) where
  get      = (^. _2) <$> (SDL ask >>= liftIO . readIORef)
  record x = SDL ask >>= liftIO . (`modifyIORef` (_2 %~ (`mappend` x)))

-------------------------------------------------------------------------------------

sdlTexture :: MonadIO m => Image -> SDL e m SDL.Texture
sdlTexture (Image index _) = fromJust <$> (`N.lookup` index) <$> getTextures <$> get

-------------------------------------------------------------------------------------

runSDL :: (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
          Vec (N.NNeg C.CInt) -> N.NNeg C.CInt -> N.NNeg C.CInt -> SDL e m a -> m a
runSDL bottomLeft w h sdl = do
  (_, renderer) <- SDL.createWindow bottomLeft w h
  ioRef <- liftIO $ newIORef (SDLState renderer, TextureCache [])
  runReaderT (unSDL sdl) ioRef

textureDimensions :: (Monad m, MonadIO m, MonadError e m, SDL.FromSDLError e)
                     => SDL.Texture -> SDL e m (C.CInt, C.CInt)
textureDimensions tex = sdlCont $ do
    -- Pointers to be filled by query
    (wPtr,hPtr) <- liftA2 (,) alloca alloca

    (SDL.queryTexture tex
     <$> alloca <*> alloca <*> (pure wPtr) <*> (pure hPtr))
     >>= SDL.safeSDL_

    liftA2 (,) (peek wPtr) (peek hPtr)

imageDimensions :: (Monad m, MonadIO m, MonadError e m, SDL.FromSDLError e)
                   => Image -> SDL e m (C.CInt, C.CInt)
imageDimensions img = sdlTexture img >>= textureDimensions

loadImage :: (MonadError e m, MonadIO m, Monad m, SDL.FromSDLError e)
             => FilePath -> SDL e m Image
loadImage file = do

  renderer <- use renderer
  tex      <- SDL.loadTexture file renderer

  index    <- N.length <$> getTextures <$> get

  (w,h)    <- textureDimensions tex

  rect     <- malloc
  poke rect (SDL.Rect 0 0 w h)

  record (TextureCache [tex])
  return (Image index rect)

renderImage :: (Monad m, MonadIO m, MonadError e m, SDL.FromSDLError e)
               => Image -> C.CInt -> C.CInt -> SDL e m ()
renderImage img@(Image _ rect) x y = do
  SDL.Rect _ _ w h <- peek rect

  renderer <- use renderer
  tex <- sdlTexture img

  sdlCont $ do
    destRect <- alloca
    poke destRect (SDL.Rect x y w h)
    SDL.safeSDL_ (SDL.renderCopy renderer tex rect destRect)

-------------------------------------------------------------------------------------

newtype SDLError = SDLError { sdlErrMsg :: String }
                 deriving (Show,Typeable)
instance Exception SDLError where

fromSDLError :: SomeException -> Maybe SDLError
fromSDLError = fromException

newtype SDLCont r a =
  SDLCont { unSDLCont :: ContT r IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance SDL.FromSDLError SDLError where
  fromSDLError = SDLError

runSDLCont :: SDLCont r a -> (a -> IO r) -> IO r
runSDLCont = runContT . unSDLCont

peek :: (MonadIO m, C.Storable a) => C.Ptr a -> m a
peek = liftIO . C.peek

poke :: (MonadIO m, C.Storable a) => C.Ptr a -> a -> m ()
poke ptr = liftIO . C.poke ptr

malloc :: (MonadIO m, C.Storable a) => m (C.Ptr a)
malloc = liftIO C.malloc

alloca :: C.Storable a => SDLCont r (C.Ptr a)
alloca = SDLCont $ ContT C.alloca

instance MonadError SDLError (SDLCont r) where
  throwError e = liftIO (throwIO e)
  catchError (SDLCont c) handler =
    SDLCont (ContT (\k -> catchJust fromSDLError (runContT c k)
                          (\e -> runSDLCont (handler e) k )))

sdlCont :: (Monad m, MonadError e m, MonadIO m, SDL.FromSDLError e)
           => SDLCont a a -> SDL e m a
sdlCont x = lift $ join $ liftIO $
            catchJust fromSDLError sdl
            (\sdlErr -> let errMsg = sdlErrMsg sdlErr
                        in return (throwError (SDL.fromSDLError errMsg)))
  where sdl = return <$> runSDLCont x return
