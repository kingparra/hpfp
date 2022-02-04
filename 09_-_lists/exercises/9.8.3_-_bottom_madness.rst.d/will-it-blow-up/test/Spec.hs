import Test.Hspec
import Lib
  ( one
  , two
  , three
  , four
  , five
  , six
  , seven
  , eight
  , nine
  )
import Control.Exception (evaluate)

{- To run this in GHCi

$ stack ghci will-it-blow-up:test:will-it-blow-up-test

This will make packages that the test component depends
on avaialbe for import within ghci.
-}


main = hspec $ do

  describe "Question 1" $ do
    it "will throw an exception after\
       \printing the first element" $ do
       -- Verify that the first element is (1^2).
       one !! 0 `shouldBe` 1
       -- The intent is to to test for an exception after
       -- the first element, but I don't know what I'm
       -- doing.
       mapM_ print (tail one) `shouldThrow` anyException

  describe "Question 2" $ do
    it "will return [1^2]" $ do
      two `shouldBe` [1^2]

  describe "Question 3" $ do
    it "will return and exception" $ do
      pendingWith "figure out how to test for this exception"

  describe "Question 4" $ do
    it "will return 3" $ do
      four `shouldBe` 3

  describe "Question 5" $ do
    it "will return and exception" $ do
      pendingWith "figure out how to test for this exception"

  describe "Question 6" $ do
    it "will return [2]" $ do
      six `shouldBe` [2]

  describe "Question 7" $ do
    it "will return and exception" $ do
      pendingWith "figure out how to test for this exception"

  describe "Question 8" $ do
    it "will return [1]" $ do
      eight `shouldBe` [1]

  describe "Question 9" $ do
    it "will return [1,3]" $ do
      nine `shouldBe` [1,3]

  describe "Question 10" $ do
    it "will return and exception" $ do
      pendingWith "figure out how to test for this exception"
