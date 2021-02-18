module Lib where
import Test.QuickCheck


data Trivial = Trivial deriving (Eq, Show)


instance Semigroup Trivial where
  _ <> _ = Trivial


instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

-- What is arbitrary, and what is it supposed to do?
-- It seems related to QuickCheck?
--
--  The class method arbitrary is the default generator
--  for type a. You can provide a default generator for
--  any other type by declaring an instance of class
--  Arbitrary that implements the arbitrary method. 
--

instance Arbitrary Trivial where
  -- Why is there a "return" in the sample
  -- code here, and what does it do?
  arbitrary = Gen Trivial

newtype Gen = Gen Trivial

