{-# LANGUAGE FlexibleContexts #-}
module Lib where
import Test.QuickCheck
import Test.QuickCheck.Checkers



-- Question 1
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)


instance Traversable Identity where
  traverse = undefined



-- Question 2
newtype Constant a b =
  Constant { getConstant :: a }



-- Question 3
data Optional a = Nada | Yep a



-- Question 4
data List a = Nil | Cons a (List a)



-- Question 5
data Three a b c = Three a b c



-- Question 6
data Pair a b = Pair a b



-- Question 7
data Big a b = Big a b b



-- Question 8
data Bigger a b = Bigger a b b b



-- Question 9
data S n a = S (n a) a deriving (Eq, Show)


instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
         => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary


instance ( Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a)
         , EqProp a)
         => EqProp (S n a) where
 (=-=) = eq


instance Traversable n =>
  Traversable (S n) where
  traverse = undefined


main = sample' (arbitrary :: Gen (S [] Int))
