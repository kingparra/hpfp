module Lib where
import Test.QuickCheck

-- Question 1
data Trivial = Trivial deriving (Eq, Show)


instance Semigroup Trivial where
  _ <> _ = Trivial


instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)


instance Arbitrary Trivial where
  arbitrary = return Trivial :: Gen Trivial
