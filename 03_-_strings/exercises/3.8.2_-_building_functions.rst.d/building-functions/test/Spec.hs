module Spec where
import Test.Hspec (hspec, describe, it, shouldBe)
import Test.QuickCheck
import Lib (a, b, c, thirdLetter)

main = hspec $ do

  describe "Questions 1 and 2" $ do
    it "a" $ do
      a "Curry is awesome" `shouldBe` "Curry is awesome!"
    it "b" $ do
      b "Curry is awesome" `shouldBe` "y"
    it "c" $ do
      c "Curry is awesome" `shouldBe` "awesome!"

  describe "Question 3" $ do
    it "will return the third character in a string" $ do
      thirdLetter "this" `shouldBe` 'i'
