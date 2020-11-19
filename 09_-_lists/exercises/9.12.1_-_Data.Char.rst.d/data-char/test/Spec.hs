import Test.Hspec
import Lib
main = hspec $ do
  describe "Question 2" $ do
    it "onlyUpper filters out caps" $ do
      onlyUpper "HbEfLrLxO" `shouldBe` "HELLO"
  describe "Question 3" $ do
    it "capFirstLetter \"androids are people, too.\" -> \"Androids are people, too.\"" $ do
      capFirstLetter "androids are people, too." `shouldBe` "Androids are people, too."
  describe "Question 4" $ do
    it "capAll \"whatevercase\" -> \"WHATEVERCASE\"" $ do
      capAll "whatevercase" `shouldBe` "WHATEVERCASE"
  describe "Question 5" $ do
    it "firstCapCh \"this string!\" -> 'T'" $ do
      firstCapCh "this string!" `shouldBe` 'T'
  describe "Question 6" $ do
    it "firstCapChPointfree' \"this string!\" -> 'T'" $ do
      firstCapChPointfree "this string!" `shouldBe` 'T'
