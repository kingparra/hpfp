module Lib where
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


instance Arbitrary a => Arbitrary (Identity a) where
  -- arbitrary = pure (Identity 7)
  -- arbitrary = pure (Identity (choose (7,42))
  -- arbitrary = pure (Identity (elements [1,2,3]))
  arbitrary = arbitrary >>= pure . Identity


instance Eq a => EqProp (Identity a) where
  (=-=) = eq


-- Question 2
newtype Constant a b =
  Constant { getConstant :: a }


instance Functor (Constant b) where
  fmap f = undefined


instance Foldable (Constant b) where
  foldMap = undefined


instance Traversable (Constant b) where
  traverse = undefined



-- Question 3
data Optional a = Nada | Yep a


instance Functor Optional where
  fmap = undefined


instance Foldable Optional where
  foldr = undefined


instance Traversable Optional where
  traverse = undefined



-- Question 4
data List a = Nil | Cons a (List a)


instance Functor List where
  fmap = undefined


instance Foldable List where
  foldr = undefined


instance Traversable List where
  traverse = undefined



-- Question 5
data Three a b c = Three a b c


instance Functor (Three b c) where
  fmap = undefined


instance Foldable (Three b c) where
  foldr = undefined


instance Traversable (Three b c) where
  traverse = undefined



-- Question 6
data Pair a b = Pair a b


instance Functor (Pair b) where
  fmap = undefined


instance Foldable (Pair b) where
  foldr = undefined


instance Traversable (Pair b) where
  traverse = undefined



-- Question 7
data Big a b = Big a b b



instance Functor (Big b) where
  fmap = undefined


instance Foldable (Big b) where
  foldr = undefined


instance Traversable (Big b) where
  traverse = undefined



-- Question 8
data Bigger a b = Bigger a b b b


instance Functor (Bigger b) where
  fmap = undefined


instance Foldable (Bigger b) where
  foldr = undefined


instance Traversable (Bigger b) where
  traverse = undefined



-- -- Question 9
-- {-# LANGUAGE FlexibleContexts #-}
-- data S n a = S (n a) a deriving (Eq, Show)


-- instance ( Functor n
--          , Arbitrary (n a)
--          , Arbitrary a
--          ) => Arbitrary (S n a) where
--   arbitrary = S <$> arbitrary <*> arbitrary


-- instance ( Applicative n
--          , Testable (n Property)
--          , Eq a
--          , Eq (n a)
--          , EqProp a
--          ) => EqProp (S n a) where
--   (=-=) = eq


-- instance Traversable n => Traversable (S n) where
--   traverse = undefined


-- main = sample' (arbitrary :: Gen (S [] Int))
