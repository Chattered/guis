{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts, RankNTypes
    , GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Backend.Internal.SDLWrap (textureDimensions, loadTexture
                                ,renderImage
                                ,update
                                ,Texture, SDL, runSDL, clear) where

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
import qualified Graphics.UI.SDL.Enum as SDL
import qualified Graphics.UI.SDL.Video as SDL (queryTexture,renderCopyEx)
import qualified Graphics.UI.SDL.Types as SDL
import           Philed.Control.Monad.Error
import           Philed.Control.Monad.Record
import qualified Philed.Data.NNeg as N
import           Philed.Data.Rect
import           Philed.Data.Vector
import qualified Pipes.Safe as PS
import           System.IO.Error

-------------------------------------------------------------------------------------

data TextureSpec = TextureSpec { _texture :: SDL.Texture
                               , _textureWidth  :: Word
                               , _textureHeight :: Word
                               }
newtype Texture = Texture Word deriving (B.Binary, Eq, Show)
data Cache = Cache { _getTextures   :: [TextureSpec]
                   , _knownTextures :: M.Map FilePath Texture
                   }
makeLenses ''TextureSpec
makeLenses ''Cache

instance Monoid Cache where
  mempty  = Cache mempty mempty
  mappend (Cache texs m) (Cache texs' m') =
    Cache (texs `mappend` texs') (m `mappend` m')

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
  renderer  <- use renderer

  preloaded <- M.lookup file <$> acquire knownTextures

  case preloaded of
   Just tex -> return tex
   Nothing  -> do
     tex       <- SDL.loadTexture renderer file

     index     <- genericLength <$> acquire getTextures

     (w,h)     <- texDimensions tex

     let texture = Texture index

     record $ Cache
       [TextureSpec tex (fromIntegral w) (fromIntegral h)]
       (M.singleton file texture)
     return texture

textureDimensions :: MonadIO m => Texture -> SDL e m (Word, Word)
textureDimensions tex = do
  texSpec <- textureSpec tex
  return (texSpec ^. textureWidth, texSpec ^. textureHeight)

nFromIntegral :: Num a => N.NNeg Word -> a
nFromIntegral = fromIntegral . N.extract

renderImage :: (Monad m, MonadIO m, MonadError e m, SDL.FromSDLError e)
               => Texture -> Rect Word -> Rect Word -> Vec C.CInt -> Double -> Bool
               -> SDL e m ()
renderImage tex srcRect destRect (cx,cy) angle flip = do
  texSpec <- textureSpec tex
  renderer <- use renderer

  sdlCont $ do
    sdlSrcRect <- alloca
    let fromTop = (texSpec ^. textureHeight) - top srcRect
    let srcW = fromIntegral . N.extract . width $ srcRect
    let srcH = fromIntegral . N.extract . height $ srcRect
    poke sdlSrcRect $ SDL.Rect
      (fromIntegral . left $ srcRect)
      (fromIntegral fromTop)
      srcW
      srcH
    sdlDestRect <- alloca
    let destW = fromIntegral . N.extract . width $ destRect
    let destH = fromIntegral . N.extract . height $ destRect
    poke sdlDestRect $ SDL.Rect
      (fromIntegral . left $ destRect)
      (fromIntegral fromTop)
      destW
      destH
    sdlPoint <- alloca
    poke sdlPoint $ SDL.Point cx cy
    SDL.safeSDL_ $ (SDL.renderCopyEx
      renderer
      (texSpec ^. texture)
      sdlSrcRect
      sdlDestRect
      (realToFrac angle)
      sdlPoint
      (if flip then SDL.SDL_FLIP_HORIZONTAL else SDL.SDL_FLIP_NONE))

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
