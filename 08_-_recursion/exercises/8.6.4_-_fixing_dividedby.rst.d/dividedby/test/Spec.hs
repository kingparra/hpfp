import Test.Hspec (hspec, context, describe, it, shouldBe)
import Lib (dividedBy', DividedResult(..))

-- The main issue we want to address is that
-- dividedBy doesn't handle divisors of 0 or
-- less.
main :: IO ()
main = hspec $ do
  describe "dividedBy'" $ do
    it "10 2 -> Result 5" $ do
      dividedBy' 10 2 `shouldBe` Result 5
    it "10 (-2) -> Result (-5)" $ do
      dividedBy' 10 (-2) `shouldBe` Result (-5)
    it "(-10) (-2) -> Result 5" $ do
      dividedBy' (-10) (-2) `shouldBe` Result 5
    it "(-10) 2 -> Result (-5)" $ do
      dividedBy' (-10) 2 `shouldBe` Result (-5)
    it "8 0 -> DividedByZero" $ do
      dividedBy' 8 0 `shouldBe` DividedByZero
    it "0 8 -> 0" $ do
      dividedBy' 0 8 `shouldBe` Result 0
