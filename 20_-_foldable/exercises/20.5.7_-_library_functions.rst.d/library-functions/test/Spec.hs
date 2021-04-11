import Data.Monoid ( Sum(..), Product(..) )
import qualified Lib
import Test.Hspec ( hspec , context , describe , it
                  , shouldBe , pending , pendingWith )
import Test.Hspec.QuickCheck (prop)


main = hspec $ do

  describe "Question 5" $ do
    it "empty structures return Nothing" $ do
      Lib.maximum [Just 8,Just 20] `shouldBe` Just (Just 20)
    it "lists return the maximum element" $ do
      Lib.maximum [20,19..1] `shouldBe` Just 20
      Lib.maximum [1..20] `shouldBe` Just 20
      Lib.maximum ['a'..'z'] `shouldBe` Just 'z'
  describe "Question 10" $ do
    it "works for the haddock examples of foldMap" $ do
      Lib.foldMap Sum [1,3,5] `shouldBe` Sum {getSum = 9}
      Lib.foldMap Product [1,3,5] `shouldBe` Product {getProduct = 15}
      Lib.foldMap (replicate 3) [1,2,3] `shouldBe` [1,1,1,2,2,2,3,3,3]
