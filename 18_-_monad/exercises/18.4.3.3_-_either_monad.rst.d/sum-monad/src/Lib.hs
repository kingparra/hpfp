module Lib where


data Sum a b = First a | Second b deriving (Eq, Show)


instance Functor (Sum b) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a


instance Applicative (Sum b) where
  (First a)  <*>  _   =  First a
  _  <*>  (First a)   =  First a
  (Second f) <*>  r   =  fmap f r
  pure = Second


instance Monad (Sum b) where
  return = pure
  (>>=) = undefined
