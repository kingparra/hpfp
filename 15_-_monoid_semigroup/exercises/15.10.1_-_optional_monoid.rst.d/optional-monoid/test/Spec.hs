import Test.Hspec
import Test.QuickCheck
import Data.Monoid
import Lib

main :: IO ()
main = hspec $ do
  describe "compound types can be assembled by\
           \ performing operations on their\
           \ inhabitants" $ do
    it "Sums wrapped in Only can be combined" $ do
      Only (Sum 1) `mappend` Only (Sum 1)
        `shouldBe` Only (Sum {getSum = 2})
    it "Products wrapped in Only can be combined" $ do
      Only (Product 4) `mappend` Only (Product 2) `shouldBe` Only (Product {getProduct = 8})
    it "A Sum mappended to Nada resulst in that Sum" $ do
      Only (Sum 1) `mappend` Nada `shouldBe` Only (Sum {getSum = 1})
    it "right identity" $ do
      Only [1] `mappend` Nada `shouldBe` Only [1]
    it "left identity" $ do
      Nada `mappend` Only (Sum 1) `shouldBe` Only (Sum {getSum = 1})
