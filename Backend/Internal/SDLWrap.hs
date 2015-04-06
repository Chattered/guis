{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts, RankNTypes
    , GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Backend.Internal.SDLWrap (textureDimensions, loadTexture
                                ,newImage, renderImage
                                ,update
                                ,Texture, Image, SDL, runSDL, clear) where

import qualified Backend.Internal.SDL as SDL
import           Control.Applicative hiding ((<$>))
import           Control.Exception (IOException)
import           Control.Lens
import           Control.Monad.Catch hiding (catchIOError,finally)
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Control.Monad.State as S
import qualified Data.Binary as B
import           Data.IORef
import           Data.List (genericIndex, genericLength)
import qualified Data.Map as M
import           Data.Maybe (isJust,fromJust)
import           Data.Monoid
import qualified Foreign as C
import qualified Foreign.C.Types as C
import qualified Graphics.UI.SDL.Video as SDL (queryTexture,renderCopy)
import qualified Graphics.UI.SDL.Types as SDL
import           Philed.Control.Monad.Error
import           Philed.Control.Monad.Record
import qualified Philed.Data.NNeg as N
import           Philed.Data.Rect
import           Philed.Data.Vector
import qualified Pipes.Safe as PS
import           System.IO.Error

import qualified Graphics.UI.SDL.Video as Jam

-------------------------------------------------------------------------------------

data ImageSpec = ImageSpec { _imgTexture :: Texture
                           , _srcRect  :: C.Ptr SDL.Rect
                           } deriving Eq
data TextureSpec = TextureSpec { _texture :: SDL.Texture
                               , _textureWidth  :: Word
                               , _textureHeight :: Word
                               }
newtype Image   = Image (N.NNeg Integer) deriving B.Binary
newtype Texture = Texture Word deriving (B.Binary, Eq)
data Cache = Cache { _getTextures   :: [TextureSpec]
                   , _getImages     :: [ImageSpec]
                   , _knownTextures :: M.Map FilePath Texture
                   }
makeLenses ''ImageSpec
makeLenses ''TextureSpec
makeLenses ''Cache

instance Monoid Cache where
  mempty  = Cache mempty mempty mempty
  mappend (Cache texs imgs m) (Cache texs' imgs' m') =
    Cache (texs `mappend` texs') (imgs `mappend` imgs') (m `mappend` m')

data SDLState = SDLState { _renderer :: SDL.Renderer, _window :: SDL.Window }
makeLenses ''SDLState

newtype SDL e m a =
  SDL { unSDL :: ReaderT (IORef (SDLState,Cache)) m a }
  deriving (Applicative,Functor,Monad,MonadCatch,MonadMask,MonadThrow,
            MonadError e,MonadIO,MonadTrans)

instance MonadIO m => S.MonadState SDLState (SDL e m) where
  get   = (^. _1) <$> (SDL ask >>= liftIO . readIORef)
  put x = SDL ask >>= liftIO . (`modifyIORef` (_1 .~ x))

instance MonadIO m => MonadRecord Cache (SDL e m) where
  get      = (^. _2) <$> (SDL ask >>= liftIO . readIORef)
  record x = SDL ask >>= liftIO . (`modifyIORef` (_2 %~ (`mappend` x)))

-------------------------------------------------------------------------------------

acquire :: MonadRecord s m => Getter s a -> m a
acquire l = (^. l) <$> get

textureSpec :: MonadIO m => Texture -> SDL e m TextureSpec
textureSpec (Texture index) = (`genericIndex` index) <$> acquire getTextures

imageSpec :: MonadIO m => Image -> SDL e m ImageSpec
imageSpec (Image index) = fromJust . (`N.lookup` index) <$> acquire getImages

-------------------------------------------------------------------------------------

runSDL :: (MonadIO m, MonadError e m, SDL.FromSDLError e) =>
          Vec Word -> Word -> Word -> SDL e m a -> m ()
runSDL bottomLeft w h sdl = flip finally SDL.quit $ do
  (window, renderer) <- SDL.createWindow bottomLeft (fromIntegral w) (fromIntegral h)
  ioRef <- liftIO $ newIORef (SDLState renderer window, mempty)
  flip finally (SDL.destroyWindow window) $ runReaderT (unSDL sdl) ioRef

update :: (MonadIO m, MonadError e m, SDL.FromSDLError e) => SDL e m ()
update = SDL.update =<< use renderer

clear :: (MonadIO m, MonadError e m, SDL.FromSDLError e) => SDL e m ()
clear = SDL.clear =<< use renderer

texDimensions :: (MonadIO m, MonadError e m, SDL.FromSDLError e)
                 => SDL.Texture -> SDL e m (C.CInt, C.CInt)
texDimensions tex = sdlCont $ do
    -- Pointers to be filled by query
    (wPtr,hPtr) <- liftA2 (,) alloca alloca

    (SDL.queryTexture tex
     <$> alloca <*> alloca <*> pure wPtr <*> pure hPtr)
     >>= SDL.safeSDL_

    liftA2 (,) (peek wPtr) (peek hPtr)

loadTexture :: (MonadError e m, MonadIO m, SDL.FromSDLError e)
               => FilePath -> SDL e m Texture
loadTexture file = do
  renderer <- use renderer

  tex      <- SDL.loadTexture renderer file

  index    <- genericLength <$> acquire getTextures

  (w,h)    <- texDimensions tex

  record $ Cache [TextureSpec tex (fromIntegral w) (fromIntegral h)] mempty mempty
  return (Texture index)

textureDimensions :: MonadIO m => Texture -> SDL e m (Word, Word)
textureDimensions tex = do
  texSpec <- textureSpec tex
  return (texSpec ^. textureWidth, texSpec ^. textureHeight)

nFromIntegral :: Num a => N.NNeg Word -> a
nFromIntegral = fromIntegral . N.extract

newImage :: (MonadIO m, SDL.FromSDLError e)
            => Texture -> Rect Word -> SDL e m Image
newImage tex rect = do
  srcRect <- malloc
  let (x,y) = topLeft rect
  poke srcRect $ SDL.Rect (fromIntegral x) (fromIntegral y)
    (nFromIntegral . width $ rect) (nFromIntegral . height $ rect)

  index <- N.length <$> acquire getImages

  record $ Cache mempty [ImageSpec tex srcRect] mempty

  return (Image index)

renderImage :: (Monad m, MonadIO m, MonadError e m, SDL.FromSDLError e)
               => Image -> Word -> Word -> SDL e m ()
renderImage img x y = do
  imgSpec <- imageSpec img
  texSpec <- textureSpec $ imgSpec ^. imgTexture
  let rect =  imgSpec ^. srcRect

  SDL.Rect _ _ w h <- peek rect

  renderer <- use renderer

  sdlCont $ do
    destRect <- alloca
    poke destRect (SDL.Rect (fromIntegral x) (fromIntegral y) w h)
    SDL.safeSDL_ (SDL.renderCopy renderer (texSpec ^. texture) rect destRect)

-------------------------------------------------------------------------------------

newtype SDLCont r a =
  SDLCont { unSDLCont :: ContT r IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

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

instance MonadError IOException (SDLCont r) where
  throwError e = liftIO (ioError e)
  catchError (SDLCont c) handler =
    SDLCont (ContT (\k -> catchIOError (runContT c k)
                          (\e -> runSDLCont (handler e) k )))

sdlCont :: (Monad m, MonadError e m, MonadIO m, SDL.FromSDLError e)
           => SDLCont a a -> SDL e m a
sdlCont x = lift $ join $ liftIO $
            catchIOError sdl
            (\err -> let errMsg = ioeGetErrorString err
                     in return (throwError (SDL.fromSDLError errMsg)))
  where sdl = return <$> runSDLCont x return
