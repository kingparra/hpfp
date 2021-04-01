module Lib where
import Test.QuickCheck.Checkers ((=-=), EqProp(..), eq)
import Test.QuickCheck (Arbitrary(..))




-- Question 1
data Nope a = NopeDotJpg deriving (Eq, Show)


-- Notice that we're ommiting the type variable
-- a in order to satisfy the kind constraint of
-- (* -> *).
instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg


instance Applicative Nope where
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg
  pure x = NopeDotJpg


instance Monad Nope where
  NopeDotJpg >>= f = NopeDotJpg


instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg


instance Eq a => EqProp (Nope a) where
  (=-=) = eq




-- Question 2
data PhhhbbtttEither b a =
  PLeft a | PRight b
  deriving (Eq, Show)


instance Functor (PhhhbbtttEither a) where
  fmap f (PLeft x) = PLeft (f x)


instance Applicative (PhhhbbtttEither a) where
  (<*>) (PLeft f) (PLeft x) = PLeft (f x)
  pure x = PLeft x


instance Monad (PhhhbbtttEither a) where
  (>>=) (PLeft x) f = f x




-- Question 3
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor (Identity) where
  fmap f (Identity x) = Identity (f x)


instance Applicative (Identity) where
  (<*>) (Identity f) (Identity x) = Identity (f x)
  pure x = Identity x


instance Monad (Identity) where
  (>>=) (Identity x) f = f x




-- Question 4
data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


instance Applicative List where
  (<*>) (Cons f fs) (Cons x xs) = Cons (f x) (fs <*> xs)
  pure x = Cons x Nil
