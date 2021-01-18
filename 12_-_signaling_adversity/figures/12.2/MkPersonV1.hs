#!/usr/bin/env stack
-- stack --resolver lts-16.28 --package hspec

module MkPersonV1 where
import Test.Hspec

type Name = String
type Age = Integer
data Person = Person Name Age deriving (Show, Eq)

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0  =  Just (Person name age)
  | otherwise               =  Nothing

main = hspec $ do
  describe "mkPerson" $ do
    context "adverse data" $ do
      it "\"\" 160 ==> Nothing" $ do
        mkPerson "" 160 `shouldBe` Nothing
      it "\"blah\" 0 ==> Just (Person \"blah\" 0)" $ do
        mkPerson "blah" 0 `shouldBe` Just (Person "blah" 0)
