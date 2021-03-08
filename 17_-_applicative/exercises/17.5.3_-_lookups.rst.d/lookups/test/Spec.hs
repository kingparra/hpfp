import Test.Hspec
import Data.Typeable
import Lib

main = hspec $ do
  describe "Question 1" $ do
    it "added has type Maybe Integer" $ do
      (show (typeOf added)) `shouldBe` "Maybe Integer" 
    it "added is Just 9" $ do
      added `shouldBe` (Just 9)

  describe "Question 2" $ do
    it "tupled has type Maybe (Integer, Integer)" $ do
      (show (typeOf tupled)) `shouldBe` "Maybe (Integer,Integer)"

  describe "Question 3" $ do
    it "maxed has type Maybe Int" $ do
      (show (typeOf maxed)) `shouldBe` "Maybe Int"

  describe "Question 4" $ do
    it "summed :: Maybe Integer" $ do
      (show (typeOf summed)) `shouldBe` "Maybe Integer"
