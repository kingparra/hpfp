import Test.Hspec
import Lib

main = hspec $ do


  context "zipping functions" $ do

    describe "x1, x2, and x3" $ do

      it "x1" $ do
        x1 `shouldBe` Just (6,9)
      it "x2" $ do
        x2 `shouldBe` Nothing
      it "x3 3" $ do
        x3 `shouldBe` (Just 9, Just 9)


  context "tests that were in main on page 869..870" $ do

    describe "based on sequenceA" $ do

      it "[Just 3, Just 2, Just 1] ==> Just [3,2,1]" $ do
        sequenceA [Just 3, Just 2, Just 1]
          `shouldBe` Just [3,2,1]
      it "[x, y] ==> a matrix of xs and ys" $ do
        sequenceA [x, y] `shouldBe`
          [ [1,4], [1,5], [1,6]
          , [2,4], [2,5], [2,6]
          , [3,4], [3,5], [3,6] ]
      it "[xs, ys] ==> Just [6,9]" $ do
        sequenceA [xs, ys] `shouldBe`
          Just [6,9]

    describe "other functions" $ do

      it "summed" $ do
        summed <$> ((,) <$> xs <*> ys) `shouldBe`
          Just 15
        fmap summed ((,) <$> xs <*> ys) `shouldBe`
          Nothing
      it "bolt" $ do
        bolt 7 `shouldBe` True
        fmap bolt z `shouldBe` [True, False, True]
