import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Pair a = Pair a a
  deriving (Eq, Show)


instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = return (Pair arbitrary arbitrary)

-- f <$> x <*> y <*> z
-- x = [1,2,3,4,5]
-- x^3 + y^3 + z^3 = 1

instance Monoid a => Monoid (Pair a) where
  mempty = (Pair mempty mempty)


instance Semigroup a => Semigroup (Pair a) where
  (<>) (Pair x y) (Pair a b) = (Pair (x <> a) (a <> b))


-- instance EqProp (Pair a) where
--   (=-=) = eq


-- main :: IO ()
-- main = quickBatch (monoid Pair)
