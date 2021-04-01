module Lib where


data Sum a b = First a | Second b deriving (Eq, Show)


instance Functor (Sum b) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a


instance Applicative (Sum b) where
  (First a)  <*>  _   =  First a
  (Second f) <*>  r   =  fmap f r
  -- The condition where First is on the right
  -- would be a type error, since we can't access
  -- the first tyvar of Either, used by the First
  -- data constructor, so we don't have
  -- to worry about defining any logic for it.
  pure = Second


instance Monad (Sum b) where
  return = pure
  (Second b) >>= f  =  f b
  (First a)  >>= _  =  (First a)
