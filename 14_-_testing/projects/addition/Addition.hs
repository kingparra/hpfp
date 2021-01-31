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
