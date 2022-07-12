import Test.Hspec
import Lib

main = hspec $ do
  describe "Question 1" $ do
    it "unit test 1" $ do
      True `shouldBe` True
  describe "Question 2" $ do
    it "seekritFunc is avgWordLength" $ do
      seekritFunc "This damned sentence." `shouldBe`
        avgWordLength "This damned sentence."
