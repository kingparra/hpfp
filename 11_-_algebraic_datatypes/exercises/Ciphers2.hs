#!/usr/bin/env stack
-- stack --resolver lts-20.18 script --package hspec --package QuickCheck
import Test.Hspec
import Data.Char (toLower, toUpper, isUpper, isLower)
import Data.List (elemIndex, lookup)
import Data.Maybe (fromJust)


ce :: Int -> Char -> Char
ce n c
  | not $ c `elem` (['a'..'z'] ++ ['A'..'Z']) = c
  | c `elem` ['a'..'z'] = ['a'..'z'] !! ((fromJust $ elemIndex c ['a'..'z']) + n `mod` length ['a'..'z'])
  | c `elem` ['A'..'Z'] = ['A'..'Z'] !! ((fromJust $ elemIndex c ['A'..'Z']) + n `mod` length ['A'..'Z'])


unce :: Int -> Char -> Char
unce n c = error "not implemented"


vige :: String -> String -> String
vige p k = error "not implemented"


main = hspec $ do

  describe "ce" $ do

    it "won't shift whitespace chars" $ do
      ce 3 ' ' `shouldBe` ' '
      ce 3 '\t' `shouldBe` '\t'
      pendingWith "write a QuickCheck test that ranges over a list of whitespace chars"

    it "won't shift non-alpha chars" $ do
      ce 3 '.' `shouldBe` '.'
      ce 3 ',' `shouldBe` ','
      ce 3 '!' `shouldBe` '!'
      pendingWith "write a QuickCheck test that ranges over a list of non-alpha chars"

    it "preserves case when shifting" $ do
      ce 3 'T' `shouldBe` 'W'
      ce 3 't' `shouldBe` 'w'

  describe "vige" $ do

    context "edge cases for key" $ do

      it "does not encrypt p if k is empty" $ do
        vige "this random string" "" `shouldBe` "this random string"

      it "exception on non-alpha chars in k" $ do
        pendingWith "write a test that checks for exceptions"
        -- vige "test for non-alpha in k" "LEMON" `shouldThrow` anyException

      it "doesn't respect case in k" $ do
        vige "ATTACKATDAWN" "LEMON" `shouldBe` vige "ATTACKATDAWN" "lemon"

      it "doesn't cycle k on non-alpha chars" $ do
        vige "this is the plaintext" "key" `shouldBe` "dlgc mq dlc zpysrrobr"
