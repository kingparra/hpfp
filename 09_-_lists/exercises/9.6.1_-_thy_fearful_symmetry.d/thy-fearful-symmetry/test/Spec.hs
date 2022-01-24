import Test.Hspec
import Test.QuickCheck
import Lib (myWords, myLines, mySplit)
import PoemLines (sentences, firstSen, secondSen, thirdSen, fourthSen)

main = hspec $ do

  describe "myWords" $ do
    it "will split on spaces" $ do
      myWords "sheryl wants fun" `shouldBe` ["sheryl","wants","fun"]
    it "won't split on other whitespace characters" $ do
      myWords "don't\nyou\tknow\vumbrellas are great" `shouldBe` ["don't\nyou\tknow\vumbrellas","are","great"]
    it "will respect empty lists" $ do
      myWords "" `shouldBe` []

  describe "myLines" $ do
    it "will split a string on newlines" $ do
      myLines sentences `shouldBe` lines sentences
    it "will produce the same output as lines" $ do
      property (\x -> myLines x == lines x)
      -- property $ verboseCheck (\x -> myLines x == lines x)
