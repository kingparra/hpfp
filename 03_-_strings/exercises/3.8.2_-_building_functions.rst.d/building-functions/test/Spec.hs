module Spec where
import Test.Hspec (hspec, describe, it, shouldBe)
import Test.QuickCheck
import Lib (a, b, c, thirdLetter)

main = hspec $ do

  describe "Question 2" $ do
    it "a" $ do
      a "Curry is awesome" `shouldBe` "Curry is awesome!"
    it "b" $ do
      b "Curry is awesome" `shouldBe` "y"
    it "c" $ do
      c "Curry is awesome" `shouldBe` "awesome!"

  describe "Question 3" $ do
    it "thirdLetter will return the third character in a string" $ do
      thirdLetter "this" `shouldBe` 'i'

  describe "Question 4" $ do
    it "letterIndex 8 will return 's', the 8th char of \"Curry is awesome\"" $ do
      letterIndex 8 `shouldBe` 's'

  describe "Question 5" $ do
    it "rvrs" $ do
      rvrs `shouldBe` "awesome is Curry"
