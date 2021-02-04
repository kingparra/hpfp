import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Lib (half)
import Data.List (sort)


-- Question 1
prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = half x * 2 == x


-- Question 2
-- For any list you apply ``sort`` to, this property should hold.
prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered xs =
  snd (foldr go (Nothing, True) xs)
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


-- Question 3
prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative x y z  =  x + (y + z) == (x + y) + z

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative x y    =  x + y == y + x


-- Question 4
prop_multAssociative :: Int -> Int -> Int -> Bool
prop_multAssociative x y z =  x * (y * z) == (x * y) * z

prop_multCommutative :: Int -> Int -> Bool
prop_multCommutative x y    =  x * y == y * x


-- Question 5
-- quot rem
prop_quotRem :: Int -> Int -> Bool
prop_quotRem x y  =  (quot x y)*y + (rem x y) == x

prop_divMod :: Int -> Int -> Bool
prop_divMod x y   =  (div x y)*y + (mod x y) == x


-- Question 6
prop_xpnAssociative :: Int -> Int -> Int -> Bool
prop_xpnAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_xpnCommutative :: Int -> Int -> Bool
prop_xpnCommutative x y = x ^ y == y ^ x

-- Question 7
prop_revRev :: String -> Bool
prop_revRev x = reverse (reverse x) == x


main :: IO ()
main = hspec $ do
  describe "Question 1" $ do
    context "division" $ do
      prop "half of x times 2 always equals x"
        prop_halfIdentity
  describe "Question 2" $ do
    context "list ordering" $ do
      prop "sort will totally order any list of Char"
        (\x -> prop_listOrdered $ sort (x :: [Char]))
      prop "sort will totally order any list of Int"
        (\x -> prop_listOrdered $ sort (x :: [Int]))
  describe "Question 3" $ do
    context "addition" $ do
      prop "addition is always associative"
        prop_plusAssociative
      prop "addition is always cummutative"
        prop_plusCommutative
  describe "Question 4" $ do
    context "multiplication" $ do
      prop "multiplication is always associative"
        prop_multAssociative
      prop "multiplication is always commutative"
        prop_multCommutative
  -- describe "Question 5" $ do
  --   context "quotRem and divMod" $ do
  --     prop "quotRem law"
  --       prop_quotRem
  --     prop "divMod law"
  --       prop_divMod
  describe "Question 6" $ do
    context "exponentiation" $ do
      prop "(^) is associative"
        prop_xpnAssociative
      prop "(^) is commutative"
        prop_xpnCommutative
  describe "Question 7" $ do
    context "reverse" $ do
      prop "x reversed twice is x"
        prop_revRev
