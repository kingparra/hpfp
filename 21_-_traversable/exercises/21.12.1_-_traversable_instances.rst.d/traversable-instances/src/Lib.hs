module Lib where

import Data.Foldable
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



-- Question 1
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)


instance Functor Identity where
  fmap f (Identity a) = Identity $ (f a)


instance Foldable Identity where
  foldMap f (Identity a) = f a


instance Traversable Identity where
  traverse f (Identity a) = Identity <$> (f a)



-- Question 2
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)


instance Functor (Constant a) where
  fmap f (Constant x) = Constant x


instance Foldable (Constant a) where
  foldr f z (Constant x) = z


instance Traversable (Constant a) where
  traverse f (Constant x) = pure (Constant x)



-- Question 3
data Optional a = Nada | Yep a
  deriving (Eq, Ord, Show)


instance Functor Optional where
  fmap f Nada = Nada
  fmap f (Yep a) = Yep (f a)


instance Foldable Optional where
  foldr f z Nada = z
  foldr f z (Yep a) = f a z


instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep a) = fmap Yep (f a)



-- Question 4
data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show)


toMyList :: [a] -> List a
toMyList [] = Nil
toMyList (x:xs) = Cons x (toMyList xs)


instance Semigroup (List a) where
  Nil <> l = l
  l <> Nil = l
  (Cons x xs) <> ys = Cons x (xs <> ys)


instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)


instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


instance Foldable List where
  foldr f z Nil = z
  foldr f z (Cons x xs) = f x (foldr f z xs)


instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> (f x) <*> traverse f xs



-- -- Question 5
data Three a b c = Three a b c
  deriving (Eq, Show)


instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c


instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z


instance Traversable (Three a b) where
  sequenceA (Three a b c) = (Three a b) <$> c



-- Question 6
data Pair a b = Pair a b
  deriving (Eq, Show)


instance Functor (Pair b) where
  fmap f (Pair x y) = Pair x $ f y


instance Foldable (Pair b) where
  foldr f z (Pair x y) = f y z


instance Traversable (Pair b) where
  traverse f (Pair x y) = Pair x <$> (f y)



-- Question 7
data Big a b = Big a b b
  deriving (Eq, Show)


instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)


instance Foldable (Big a) where
  foldr f z (Big a b c) = f b (f c z)


instance Traversable (Big b) where
  -- (f b) and (f c) are combined, then (Big a) is mapped over the result
  traverse f (Big a b c) = (Big a) <$> f b <*> f c



-- Question 8
data Bigger a b = Bigger a b b b


instance Functor (Bigger b) where
  fmap f (Bigger a x y z) = Bigger a (f x) (f y) (f z)


instance Foldable (Bigger b) where
  foldr f z (Bigger _ a b c) = f a (f b (f c z))


instance Traversable (Bigger b) where
  traverse f (Bigger a x y z) = (Bigger a) <$> f x <*> f y <*> f z



-- Question 9
data S n a = S (n a) a deriving (Eq, Show)

instance Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)


instance Foldable (S n) where
  foldr f z (S n a) = f a (foldr f z n)

-- -- {-# LANGUAGE FlexibleContexts #-}
-- -- data S n a = S (n a) a deriving (Eq, Show)
--
--
-- -- instance ( Functor n
-- --          , Arbitrary (n a)
-- --          , Arbitrary a
-- --          ) => Arbitrary (S n a) where
-- --   arbitrary = S <$> arbitrary <*> arbitrary
--
--
-- -- instance ( Applicative n
-- --          , Testable (n Property)
-- --          , Eq a
-- --          , Eq (n a)
-- --          , EqProp a
-- --          ) => EqProp (S n a) where
-- --   (=-=) = eq
--
--
-- -- instance Traversable n => Traversable (S n) where
-- --   traverse = undefined
--
--
-- -- main = sample' (arbitrary :: Gen (S [] Int))
