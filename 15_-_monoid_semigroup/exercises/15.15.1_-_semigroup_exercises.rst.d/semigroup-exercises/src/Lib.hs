module Lib where
import Test.QuickCheck
import Data.Monoid


-- Question 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial :: Gen Trivial


-- Question 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  -- arbitrary :: Arbitrary a => Gen (Identity a)
  -- arbitrary        = return $ Identity arbitrary
  -- Gen (Identity a) = Gen    ( Identity    ?     )
  -- .....................................   a
  -- ..................................... Gen a
  -- Arbitrary a => (Identity a) -> 
  arbitrary = do
    x <- arbitrary
    return (Identity x)


-- Question 3
data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Semigroup (Two a b) where
  (<>) (Two x y) (Two a b) = Two (x `mappend` a) (y `mappend` b)
