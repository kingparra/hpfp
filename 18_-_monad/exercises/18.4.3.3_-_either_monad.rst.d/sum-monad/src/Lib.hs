module Lib where


data Sum a b = First a | Second b deriving (Eq, Show)


instance Functor (Sum b) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a


instance Applicative (Sum b) where
  pure = Second
  a <*> f = undefined


instance Monad (Sum b) where
  return = pure
  (>>=) = undefined
