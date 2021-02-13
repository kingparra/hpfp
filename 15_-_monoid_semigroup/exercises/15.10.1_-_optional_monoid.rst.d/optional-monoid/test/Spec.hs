import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do
  describe "sample inputs" $ do
    it "x" $ do
      Only (Sum 1) `mappend` Only (Sum 1) `shouldBe` Only (Sum {getSum = 2})
    it "y" $ do
      Only (Product 4) `mappend` Only (Product 2) `shouldBe` Only (Product {getProduct = 8})
    it "z" $ do
      Only (Sum 1) `mappend` Nada `shouldBe` Only (Sum {getSum = 1})
    it "a" $ do
      Only [1] `mappend` Nada `shouldBe` Only [1]
    it "b" $ do
      Nada `mappend` Only (Sum 1) `shouldBe` Only (Sum {getSum = 1})
