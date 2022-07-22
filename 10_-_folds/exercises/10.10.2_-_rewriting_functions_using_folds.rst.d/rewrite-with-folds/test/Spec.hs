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
    it "simple inputs" $ do
      myElem 1 [1..10] `shouldBe` True
      myElem 1 [2..10] `shouldBe` False

  describe "Question 4: myReverse" $ do
    it "simple inputs" $ do
      myReverse "blah" `shouldBe` "halb"
      myReverse [1..5] `shouldBe` [5,4..1]

  describe "Question 5: myMap" $ do
    it "simple inputs" $ do
      myMap (+3) [1..3] `shouldBe` map (+3) [1..3]
    prop "myMap :: Int -> [Int] ≡ map :: Int -> [Int]" $ do
      ((\n l -> myMap (*n) l `shouldBe` map (*n) l)
                :: Int -> [Int] -> Expectation)
      ((\n l -> myMap (+n) l `shouldBe` map (+n) l)
                :: Int -> [Int] -> Expectation)

  describe "Question 6: myFilter" $ do
    it "simple inputs" $ do
      myFilter (\x -> x == 'a') "thearwhat" `shouldBe` "aa"

  describe "Question 7: squish" $ do
    it "simple inputs" $ do
      squish ["this","and","that"] `shouldBe` "thisandthat"

  describe "Question 8: squishMap" $ do
    it "simple inputs" $ do
      squishMap (take 3) [[1..], [10..], [100..], [1000..]]
        `shouldBe` [1,2,3,10,11,12,100,101,102,1000,1001,1002]
      squishMap (take 3) [[1..]] `shouldBe` [1,2,3]

  describe "Question 9: squishMap" $ do
    it "simple inputs" $ do
      squishMap (take 3) [[1..], [10..], [100..], [1000..]]
        `shouldBe` [1,2,3,10,11,12,100,101,102,1000,1001,1002]
      squishMap (take 3) [[1..]] `shouldBe` [1,2,3]

  describe "Question 10: myMaximumBy" $ do
    it "simple inputs" $ do
      myMaximumBy (\_ _ -> GT) [1..10] `shouldBe` 1
      myMaximumBy (\_ _ -> LT) [1..10] `shouldBe` 10
      myMaximumBy compare [1..10]      `shouldBe` 10
      myMaximumBy (\x y -> compare (length x) (length y))
        ["Hello", "World", "!", "Longest", "bar"] `shouldBe` "Longest"

  describe "Question 11: myMinimumBy" $ do
    it "simple inputs" $ do
      myMinimumBy (\_ _ -> GT) [1..10] `shouldBe` 10
      myMinimumBy (\_ _ -> LT) [1..10] `shouldBe` 1
      myMinimumBy compare [1..10]      `shouldBe` 1
      myMinimumBy (\x y -> compare (length x) (length y))
        ["Hello", "World", "!", "Longest", "bar"] `shouldBe` "!"
