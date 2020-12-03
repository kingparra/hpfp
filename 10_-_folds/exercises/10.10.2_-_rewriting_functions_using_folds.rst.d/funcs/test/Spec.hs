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
    it "works on infinite lists if True is near the begining" $ do
      myOr ([False,False,True] ++ repeat False) `shouldBe` True

  -- Question 2
  describe "myAny" $ do
    it "myAny even [1,3,5] -> False" $ do
      myAny even [1,3,5] `shouldBe` False
    it "myAny odd [1,3,5] -> True" $ do
      myAny odd [1,3,5] `shouldBe` True

  -- Question 3
  describe "myElem (foldr implementation)" $ do
    it "myElem 'f' \"\" -> False" $ do
      myElem 'f' "" `shouldBe` False
    it "myElem 'f' \"baf\" -> True" $ do
      myElem 'f' "baf" `shouldBe` True
  describe "myElem' (myAny implementation)" $ do
    it "myElem' 'f' \"\" -> False" $ do
      myElem' 'f' "" `shouldBe` False
    it "myElem' 'f' \"baf\" -> True" $ do
      myElem' 'f' "baf" `shouldBe` True

  -- Question 4
  describe "myReverse (foldl' impelementation)" $ do
    it "can reverse [1..20]" $ do
      myReverse [1..20] `shouldBe` [20,19..1]
  describe "myReverse' (foldr implementation)" $ do
    it "last $ myReverse' [1,undefined,3,4,8] -> 1" $ do
      last (myReverse [1,undefined,3,4,8]) `shouldBe` 1

  -- Question 5
  describe "myMap" $ do
    it "myMap (+1) [1..4] -> [2..5]" $ do
     myMap (+1) [1..4] `shouldBe` [2..5]
    it "myMap (+1) [] -> []" $ do
     myMap (+1) [] `shouldBe` []

  -- Question 6, myFilter
  describe "myFilter" $ do
    it "myFilter (>3) [1,2,3,4] -> [4]" $ do
      myFilter (>3) [1,2,3,4] `shouldBe` [4]
    it "myFilter (<0) [1,2,3,4] -> []" $ do
      myFilter (<0) [1,2,3,4] `shouldBe` []

  -- Question 7
  describe "squish" $ do
    it "will flatten [[1,2,3],[4,5,6]] to [1..6]" $ do
      squish [[1,2,3],[4,5,6]] `shouldBe` [1..6]
    it "squish [[],[]] -> []" $ do
      squish ([[],[]] :: [[Int]]) `shouldBe` ([] :: [Int])

  -- Question 8
  describe "squishMap" $ do
    it "squishMap (\\x->[1,x,3]) 2 -> [1,2,3]" $ do
      squishMap (\x -> [1,x,3]) [2] `shouldBe` [1,2,3]
    it "squishMap (\\x -> \"WO \"++[x]++\" OT\") \"blah\" -> \
       \ \"WO b OT WO l OT WO a OT WO h OT\"" $ do
       let f = (\x -> "WO "++[x]++" OT ") in
          squishMap f "blah" `shouldBe` "WO b OT WO l OT WO a OT WO h OT "

  -- Question 9
  describe "squishAgain" $ do
    it "squishAgain [[1,2,3],[4,5,6]] -> [1..6]" $ do
      squishAgain [[1,2,3],[4,5,6]] `shouldBe` [1..6]

  -- Question 10
  describe "myMaximumBy" $ do
    it "[99,1,2,3] -> 99" $ do
      myMaximumBy compare [99,1,2,3] `shouldBe` 99
    it "[1..10] -> 10" $ do
      myMaximumBy compare [1..10] `shouldBe` 10
    it "works on values that are not adjacent:\n\
       \ myMaximumBy compare [1,0,99,2,500,3] -> 500" $ do
      myMaximumBy compare [1,0,99,2,500,3] `shouldBe` 500

  -- Question 11
  describe "myMinimumBy" $ do
    it "myMinimumBy compare [99,1,2,3] -> 99" $ do
      myMinimumBy compare [99,1,2,3] `shouldBe` 1
