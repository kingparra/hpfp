#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-16.27 --package hspec

-- Run this with:
--
-- $ stack runghc MkPersonV2.hs
--
-- Snapshot info here: https://www.stackage.org/lts-16.27
--
module MkPersonV2 where
import Test.Hspec

-- page 467
type Name = String
type Age = Integer

data Person = Person Name Age deriving (Show, Eq)
data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

-- This version of mkPerson uses Either and the sum type PersonInvalid
-- to indicate what type of failure we may experience as result of an
-- invalid input.
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >= 0  =  Right (Person name age)
  | name == ""              =  Left NameEmpty
  | otherwise               =  Left AgeTooLow


-- These test are taken from ghci examples on page 468
main = hspec $ do
  describe "mkPerson" $ do
    it "\"Djali\" 5 ==> Right (Person \"Djali\" 5)" $ do
      mkPerson "Djali" 5 `shouldBe` Right (Person "Djali" 5)
    it "\"\" 10 ==> Left NameEmpty" $ do
      mkPerson "" 10 `shouldBe` Left NameEmpty
    it "\"Djali\" (-1) ==> Left AgeTooLow" $ do
      mkPerson "Djali" (-1) `shouldBe` Left AgeTooLow
    -- notice in this last example that when both the name and age are wrong,
    -- we're only going to see the result for the first failure case, not both.
    -- See V3 of this function for an example of a more expressive return type
    -- that can communicate multiple possible failure modes.
