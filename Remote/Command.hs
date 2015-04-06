{-# LANGUAGE DeriveFunctor #-}
module Remote.Command where

import Data.Binary
import Data.ByteString
import Data.Picture
import Philed.Data.Rect

data Cmd img tex = Clear
                 | LoadTexture String
                 | NewImage tex (Rect Word)
                 | Render (Picture img)
                 | Update
                 deriving Functor

instance (Binary img, Binary tex) => Binary (Cmd img tex) where
  put Clear               = put (0::Word8)
  put (LoadTexture path)  = put (1::Word8) >> put path
  put (NewImage tex rect) = put (2::Word8) >> put tex >> put rect
  put (Render pic)        = put (3::Word8) >> put pic
  put Update              = put (4::Word8)
  get = do
    discriminator <- get :: Get Word8
    case discriminator of
     0 -> return Clear
     1 -> LoadTexture <$> get
     2 -> NewImage <$> get <*> get
     3 -> Render <$> get
     4 -> return Update

data Response img tex = None
                      | Img img
                      | Tex tex

instance (Binary img, Binary tex) => Binary (Response img tex) where
  put None      = put (0::Word8)
  put (Tex tex) = put (1::Word8) >> put tex
  put (Img img) = put (2::Word8) >> put img
  get = do
    discriminator <- (get::Get Word8)
    case discriminator of
     0 -> return None
     1 -> Tex <$> get
     2 -> Img <$> get
