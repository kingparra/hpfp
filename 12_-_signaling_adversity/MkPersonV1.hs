#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-16.27 --package hspec

-- Run this with:
--
-- $ stack runghc MkPersonV1.hs
--
-- Snapshot info here: https://www.stackage.org/lts-16.27
--
module MkPersonV1 where
import Test.Hspec

-- First version of mkPerson, from page 464
type Name = String
type Age = Integer
data Person = Person Name Age deriving (Show, Eq)

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0  =  Just (Person name age)
  | otherwise               =  Nothing

-- This version of the mkPerson smart constructor returns an explicit signal
-- when we do not have a valid value, but still has some problems. What if we
-- want to know if it was the name, age or both that aws bad? See V2 of this
-- function for the answer.

main = hspec $ do
  describe "mkPerson" $ do
    context "adverse data should result in Nothing" $ do
      it "\"\" 160 ==> Nothing" $ do
        mkPerson "" 160 `shouldBe` Nothing
      it "\"blah\" 0 ==> Just (Person \"blah\" 0)" $ do
        mkPerson "blah" 0 `shouldBe` Just (Person "blah" 0)
