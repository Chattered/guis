{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Picture (Angle(..), Picture(..), Colour(..)) where

import Control.Monad
import Data.Binary
import GHC.Generics
import Philed.Data.NNeg (NNeg)
import Philed.Data.Vector
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

newtype Angle = Angle Double deriving (Arbitrary,Binary,CoArbitrary,Eq,Show)

data Picture i = Image i
               | Translate (Vec Double) (Picture i)
               | Scale (NNeg Double,NNeg Double) (Picture i)
               | Rotate Angle (Picture i)
               | ReflectHorizontal (Picture i)
               deriving (Eq,Functor,Generic,Show)

data Colour = Red | Green | Yellow | Blue | Magenta | Cyan | White
            deriving (Eq,Generic,Show)

instance Binary i => Binary (Picture i)

instance Binary Colour where

instance Arbitrary i => Arbitrary (Picture i) where
  arbitrary = frequency [(1, liftM Image arbitrary)
                        ,(3, liftM2 Translate arbitrary arbitrary)]

instance CoArbitrary i => CoArbitrary (Picture i) where
  coarbitrary (Image img)           = variant (-1) . coarbitrary img
  coarbitrary (Translate (x,y) pic) = variant 0 . coarbitrary (x,y,pic)
