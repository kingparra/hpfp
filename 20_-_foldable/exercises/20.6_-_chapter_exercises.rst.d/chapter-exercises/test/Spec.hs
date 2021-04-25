import Data.Monoid
import Data.Foldable
import Test.Hspec
import Lib

main = hspec $ do

  describe "Question 1" $ do
    it "foldMapping id returns the original value" $ do
      foldMap id (Constant 3 :: Constant Int (Sum Int))
      `shouldBe` Sum { getSum = 0 }

  describe "Question 2" $ do
    it "Two b or not Two b, that is the question" $ do
      foldMap (+1) (Two 1 2 :: Two (Sum Int) (Sum Int))
      `shouldBe` Sum {getSum = 3}

  describe "Question 5" $ do
    it "foldMap does the thing" $ do
      foldMap (+1) (Four'
                      'z'
                      (Sum {getSum = 1})
                      (Sum {getSum = 2})
                      (Sum {getSum = 3}))
     `shouldBe` (Sum { getSum = 9 })
