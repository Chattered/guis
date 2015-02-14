module ToBeDeprecated where

import Control.Monad

(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM

(<*) :: Monad m => m a -> m () -> m a
(<*) = liftM2 const
