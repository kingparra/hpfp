module Lib where
import Test.QuickCheck


data Trivial = Trivial deriving (Eq, Show)


instance Semigroup Trivial where
  _ <> _ = Trivial


instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)


instance Arbitrary Trivial where
  -- What does Arbitrary do again?
  -- What about Gen?
  -- Why is there a "return" in the sample
  -- code here, and what does it do?
  arbitrary = oneof [return Trivial]

newtype Gen = Gen Trivial

