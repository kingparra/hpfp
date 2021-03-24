module Addition where
import Test.Hspec
import Test.QuickCheck



dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom =
  go 0 num denom
  where
    go count n d =
      if n < d
      then (count,n)
      else go (count+1) (n-d) d



-- from page 551
-- Play with these in the repl by using sample on them.
genBool :: Gen Bool
genBool = choose (False,True)

genBool' :: Gen Bool
genBool' = elements [False,True]

genOrdering :: Gen Ordering
genOrdering = elements [LT,EQ,GT]

genChar :: Gen Char
genChar = elements ['a'..'z']



-- More complex examples.
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a,b,c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a,b,c)



-- page 553
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))
            ]
-- ·∾ :type frequency
-- frequency :: [(Int, Gen a)] -> Gen a
-- ·∾ :doc frequency
-- Chooses one of the given generators, with a weighted
-- random distribution. The input list must be non-empty.



main :: IO ()
main = hspec $ do

  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "x+1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

  describe "Division" $ do
    it "15 divided by 3 is  5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
