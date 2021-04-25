import Test.Hspec
import Lib

main = hspec $ do
  describe "Question 1" $ do
    it "unit test 1" $ do
      True `shouldBe` True
