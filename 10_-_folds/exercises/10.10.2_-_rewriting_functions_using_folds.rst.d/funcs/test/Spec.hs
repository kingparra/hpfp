import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do

  -- Question 1
  describe "myOr" $ do
    it "[False,False,True] -> True" $ do
      myOr [False,False,True] `shouldBe` True
    it "[False,False] -> False" $ do
      myOr [False,False] `shouldBe` False
  -- Question 2
  describe "myAny" $ do
    it "evens and odds" $ do
      myAny even [1,3,5] `shouldBe` False
      myAny odd [1,3,5] `shouldBe` True
  describe "myReverse" $ do
    it "can reverse [1..20]" $ do
      myReverse [1..20] `shouldBe` [20,19..1]
  describe "squish" $ do
    it "will flatten [[1,2,3],[4,5,6]] to [1..6]" $ do
      squish [[1,2,3],[4,5,6]] `shouldBe` [1..6]
  describe "squishMap" $ do
    it "squishMap (+1) [[1,2,3],[4,5,6]] -> [2..7]" $ do
      squishMap (map (+1)) [[1,2,3],[4,5,6]] `shouldBe` [2,3,4,5,6,7]
  describe "squishAgain" $ do
    it "squishAgain [[1,2,3],[4,5,6]] -> [1..6]" $ do
      squishAgain [[1,2,3],[4,5,6]] `shouldBe` [1..6]
  describe "myMaximumBy" $ do
    it "[99,1,2,3] -> 99" $ do
      myMaximumBy compare [99,1,2,3] `shouldBe` 99
      -- why does this throw a type error?
      -- myMaximumBy compare [] `shouldThrow` anyErrorCall
  describe "myMaximumBy" $ do
    it "[99,1,2,3] -> 99" $ do
      myMaximum [99,1,2,3] `shouldBe` 99
  describe "myMinimum" $ do
    it "[99,1,2,3] -> 99" $ do
      myMinimum [99,1,2,3] `shouldBe` 1
