#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-16.27 --package hspec

-- Run this with:
--
-- $ stack runghc MkPersonV3.hs
--
-- Snapshot info here: https://www.stackage.org/lts-16.27
--
module MkPersonV3 where
import Test.Hspec

-- page 469
type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a
data Person = Person Name Age deriving (Show, Eq)
data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

-- We're going to make separate checking functions and then combine the results.
--
-- Although more than one thing could be wrong with the age value, we'll keep
-- it simple and only check to make sure it's a positive Integer value.
ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True ->  Right age
  False ->  Left [AgeTooLow]

-- The implementation for our name checker is similarly niave. It only cares
-- that the string isn't empty. "💓" is a valid name, for example.
nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

-- Now we can use our type alias ValidatePerson in a new mkPerson implementation
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOkay) (Right ageOkay) = Right (Person nameOkay ageOkay)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge


main = hspec $ do
  -- page 467
  describe "nameOkay" $ do
    it "\"42\" ==> Right \"42\"" $ do
      nameOkay "42" `shouldBe` Right "42"
  -- page 469
  describe "mkPerson" $ do
    it "\"\" (-1) ==> Left [NameEmpty,AgeTooLow]" $ do
      mkPerson "" (-1) `shouldBe` Left [NameEmpty,AgeTooLow]
  -- That's more like it. Now we can tell the user what was incorrect in one
  -- go without them having to round-trip each mistake! Later in the book
  -- we'll be able to replace mkPerson and mkPerson' with the following:
  --
  -- mkPerson :: Name -> Age -> Validation [PersonInvalid] Person
  -- mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)
  --
  -- !!
