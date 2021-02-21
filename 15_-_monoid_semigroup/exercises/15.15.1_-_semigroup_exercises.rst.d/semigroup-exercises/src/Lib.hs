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
  --
  -- arbitrary :: Arbitrary a => Gen (Identity a)
  --
  -- arbitrary        = return $ Identity arbitrary
  -- Gen (Identity a) = Gen    ( Identity    ?     )
  -- .....................................   a
  -- ..................................... Gen a
  arbitrary = do
    x <- arbitrary
    return (Identity x)


-- Question 3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two x y) = Two (a <> x) (b <> y)


-- Question 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

instance (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Three a b c) where
  (<>) (Three a b c) (Three x y z) =
    Three (a <> x) (b <> y) (c <> z)


-- I'm skipping Q5


-- Question 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary :: Gen Bool
    return (BoolConj x)

instance Semigroup BoolConj where
  (<>) (BoolConj x) (BoolConj y) = (BoolConj (x && y))
