import Test.Hspec
import Data.Typeable
import Lib

main = hspec $ do
  describe "Question 1" $ do
    it "added has type Maybe Integer" $ do
      (show (typeOf added))`shouldBe` "Maybe Integer" 
    it "added is Just 9" $ do
      added `shouldBe` (Just 9)

