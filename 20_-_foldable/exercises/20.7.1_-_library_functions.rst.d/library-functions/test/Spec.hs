import Data.Monoid ( Sum(..), Product(..) )
import qualified Lib
import Test.Hspec ( hspec , context , describe , it
                  , shouldBe , pending , pendingWith )
import Test.Hspec.QuickCheck (prop)


main = hspec $ do

  describe "Question 10" $ do
    it "works for the haddock examples of foldMap" $ do
      Lib.foldMap Sum [1,3,5] `shouldBe` Sum {getSum = 9}
      Lib.foldMap Product [1,3,5] `shouldBe` Product {getProduct = 15}
      Lib.foldMap (replicate 3) [1,2,3] `shouldBe` [1,1,1,2,2,2,3,3,3]
