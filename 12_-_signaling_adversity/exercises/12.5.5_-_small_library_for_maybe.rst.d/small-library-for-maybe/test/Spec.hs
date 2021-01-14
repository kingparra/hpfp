import Test.Hspec
import Lib

main = hspec $ do


  describe "Question 1" $ do

    context "isJust" $ do
      it "Just 1 ==> True" $ do
        isJust (Just 1) `shouldBe` True
      it "Nothing ==> False" $ do
        isJust Nothing `shouldBe` False

    context "isNothing" $ do
      it "Just 1 ==> False" $ do
        isNothing (Just 1) `shouldBe` False
      it "Nothing ==> True" $ do
        isNothing Nothing `shouldBe` True


  describe "Question 2" $ do

    context  "mayybee" $ do
      it "0 (+1) Nothing ==> 0" $ do
        mayybee 0 (+1) Nothing `shouldBe` 0
      it "0 (+1) (Just 1) ==> 2" $ do
        mayybee 0 (+1) (Just 1) `shouldBe` 2


  describe "Question 3" $ do

    context "fromMaybe" $ do
      it "0 Nothing ==> 0" $ do
        fromMaybe 0 Nothing `shouldBe` 0
      it "0 (Just 1) ==> 1" $ do
        fromMaybe 0 (Just 1) `shouldBe` 1


  describe "Question 4" $ do

    context "listToMaybe" $ do
      it "[1,2,3] ==> Just 1" $ do
        listToMaybe [1,2,3] `shouldBe` Just 1
      it "[] ==> Nothing" $ do
        listToMaybe [] `shouldBe` Nothing

    context "maybeToList" $ do
      it "Just 1 ==> [1]" $ do
        maybeToList (Just 1) `shouldBe` [1]
      it "Nothing ==> []" $ do
        maybeToList Nothing `shouldBe` []


  describe "Question 5" $ do

    context "catMaybes" $ do
      it "[Just 1, Nothing, Just 2] ==> [1,2]" $ do
        catMaybes [Just 1, Nothing, Just 2] `shouldBe` [1,2]
      it "[Nothing,Nothing,Nothing] ==> []" $ do
        catMaybes [Nothing, Nothing, Nothing] `shouldBe` []


  describe "Question 6" $ do

    context "flipMaybe" $ do
      it "[Just 1, Just 2, Just 3] ==> Just [1,2,3]" $ do
        flipMaybe [Just 1, Just 2, Just 3] `shouldBe` Just [1,2,3]
      it "[Just 1, Nothing, Just 3] ==> Nothing" $ do
        flipMaybe [Just 1, Nothing, Just 3] `shouldBe` Nothing
