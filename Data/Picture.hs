{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Data.Picture (Angle(..), Picture(..)) where

import Control.Monad
import Data.Binary
import Philed.Data.NNeg (NNeg)
import Philed.Data.Rect
import Philed.Data.Vector
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

newtype Angle = Angle Double deriving (Eq,Show)

data Picture i = Image i
               | Translate (Vec Double) (Picture i)
               | Scale (NNeg Double,NNeg Double) (Picture i)
               | Rotate Angle (Picture i)
               | ReflectHorizontal (Picture i)
               deriving (Eq,Functor,Show)

instance Binary i => Binary (Picture i) where
  put (Image i)             = put (0::Word8) >> put i
  put (Translate (x,y) pic) = put (1::Word8) >> put x >> put y >> put pic
  get = do
    discriminator <- get :: Get Word8
    case discriminator of
     0 -> Image <$> get
     1 -> Translate <$> ((,) <$> get <*> get) <*> get

instance Arbitrary i => Arbitrary (Picture i) where
  arbitrary = frequency [(1, liftM Image arbitrary)
                        ,(3, liftM2 Translate arbitrary arbitrary)]

instance CoArbitrary i => CoArbitrary (Picture i) where
  coarbitrary (Image img)           = variant (-1) . coarbitrary img
  coarbitrary (Translate (x,y) pic) = variant 0 . coarbitrary (x,y,pic)
