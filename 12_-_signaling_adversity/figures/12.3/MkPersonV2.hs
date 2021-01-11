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
-- This still has some problems, as only the first failure mode encountered is
-- present in the result. See V3 of this function for an example of a more
-- expressive return type that can communicate multiple possible failure modes.


main = hspec $ do
  describe "mkPerson" $ do
    it "\"\" 160 ==> Nothing" $ do
      mkPerson "" 160 `shouldBe` Nothing
    it "\"blah\" 0 ==> Just (Person \"blah\" 0)" $ do
      mkPerson "blah" 0 `shouldBe` Just (Person "blah" 0)
