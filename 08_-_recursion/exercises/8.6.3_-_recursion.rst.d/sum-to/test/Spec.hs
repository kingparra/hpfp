import Test.Hspec
import Lib (sumTo)

main = hspec $ do
  describe "sumTo" $ do
    it "sums 1..12" $ do
      sumTo 12 `shouldBe` sum [1..12]
    it "negative values for n enumarate from (-1),(-2)..n" $ do
      sumTo (-20) `shouldBe` sum [(-1),(-2)..(-20)]
    it "returns 0 for 0" $ do
      sumTo 0 `shouldBe` 0
