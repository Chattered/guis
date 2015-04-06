{-# LANGUAGE DeriveFunctor #-}
module Data.Picture where

import Data.Binary

data Picture i = Image i
               | Translate (Double,Double) (Picture i)
                 deriving (Functor,Show)

instance Binary i => Binary (Picture i) where
  put (Image i)             = put (0::Word8) >> put i
  put (Translate (x,y) pic) = put (1::Word8) >> put x >> put y >> put pic
  get = do
    discriminator <- get :: Get Word8
    case discriminator of
     0 -> Image <$> get
     1 -> Translate <$> ((,) <$> get <*> get) <*> get
