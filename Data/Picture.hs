{-# LANGUAGE DeriveFunctor #-}
module Data.Picture where

data Picture i = Image i
               | Translate (Double,Double) (Picture i)
                 deriving (Functor,Show)
