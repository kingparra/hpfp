import Test.Hspec
import Test.Hspec.QuickCheck
import Lib

main = hspec $ do

  describe "Question 1: myOr" $ do
    it "simple inputs" $ do
      myOr [False,False,True] `shouldBe` True

  describe "Question 2: myAny" $ do
    it "simple inputs" $ do
      myAny even [1,3,5] `shouldBe` False
      myAny odd [1,3,5] `shouldBe` True

  describe "Question 3: myElem" $ do
    it "simple test cases" $ do
      myElem 1 [1..10] `shouldBe` True
      myElem 1 [2..10] `shouldBe` False

  describe "Question 4: myReverse" $ do
    it "simple test cases" $ do
      myReverse "blah" `shouldBe` "halb"
      myReverse [1..5] `shouldBe` [5,4..1]

  describe "Question 5: myMap" $ do
    it "simple test cases" $ do
      myMap (+3) [1..3] `shouldBe` map (+3) [1..3]
    prop "myMap :: Int -> [Int] ≡ map :: Int -> [Int]" $ do
      ((\n l -> myMap (*n) l `shouldBe` map (*n) l)
                :: Int -> [Int] -> Expectation)
      ((\n l -> myMap (+n) l `shouldBe` map (+n) l)
                :: Int -> [Int] -> Expectation)
