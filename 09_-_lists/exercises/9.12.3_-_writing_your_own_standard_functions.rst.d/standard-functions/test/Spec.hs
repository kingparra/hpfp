import Test.Hspec
  ( hspec
  , describe
  , it
  , shouldBe
  , shouldThrow
  , anyException
  )

import Test.Hspec.QuickCheck
  ( prop
  )

import Lib
  ( myAnd
  , myOr
  , myAny
  , myElem
  , myReverse
  , myRev
  , squish
  , squishMap
  , squishAgain
  , myMaximumBy
  , myMinimumBy
  , myMinimum
  , myMaximum
  )


main = hspec $ do

  describe "myAnd" $ do
    prop "will behave identically to and" $
      (\l -> and l `shouldBe` myAnd l)

  describe "myOr" $ do
    prop "will behave identically to or" $
      (\l -> or l `shouldBe` myOr l)

  describe "myAny" $ do
    it "will work for this fucking predicate function" $ do
      -- QuickCheck can't generate input functions.
      -- So prop .. (\pred list -> ... won't work.
      any (=='f') "fuck" `shouldBe` myAny (=='f') "fuck"

  describe "myElem" $ do
    prop "will behave identically to elem" $
      (\e l -> elem (e :: Char) (l :: [Char]) `shouldBe` myElem e l)

  describe "myReverse" $ do
    it "works for examples in the docs" $ do
      myReverse ([] :: [Int]) `shouldBe` []
      myReverse [42] `shouldBe` [42]
      myReverse [2,5,7] `shouldBe` [7,5,2]
      myReverse "Justice" `shouldBe` "ecitsuJ"
    prop "will behave identically to reverse" $
      (\l -> reverse (l :: [Char]) `shouldBe` myReverse l)

  describe "myRev (alternate implementation of reverse)" $ do
    it "works for examples in the docs" $ do
      myRev ([] :: [Int]) `shouldBe` []
      myRev [42] `shouldBe` [42]
      myRev [2,5,7] `shouldBe` [7,5,2]
      myRev "Justice" `shouldBe` "ecitsuJ"
    prop "will behave identically to reverse" $
      (\l -> reverse (l :: [Char]) `shouldBe` myRev l)

  describe "squish" $ do
    it "will concatenate lists of integers" $ do
      squish [[1,2,3],[4,5,6]] `shouldBe` [1,2,3,4,5,6]
    prop "will behave identically to concat" $
      (\l -> concat (l :: [[Char]]) `shouldBe` squish l)

  describe "squishMap" $ do
    it "can intersperse \"!\"" $ do
      squishMap (++"!") ["t","h","e"] `shouldBe` "t!h!e!"
    prop "will behave identically to concatMap" $
      (\l -> concatMap (++"e") (l :: [[Char]]) `shouldBe`
             squishMap (++"e") (l :: [[Char]]))

  describe "squishAgain" $ do
    it "[\"t\",\"h\",\"e\"] ==> \"the\"" $ do
      squishAgain ["t","h","e"] `shouldBe` "the"

  describe "myMaximumBy" $ do
    it "will work for example inputs" $ do
      -- The input list must be finite and nonempty.
      myMaximumBy compare [1,2,3] `shouldBe` 3
      myMaximumBy (\x y -> compare (length x) (length y)) ["Hello", "World", "!", "Longest", "bar"] `shouldBe` "Longest"
      myMaximumBy compare [1,53,9001,10] `shouldBe` 9001

  describe "myMinimumBy" $ do
    it "will work for example inputs" $ do
      -- The input list must be finite and nonempty.
      myMinimumBy compare [1,2,3] `shouldBe` 1
      myMinimumBy (\x y -> compare (length x) (length y)) ["Hello", "World", "!", "Longest", "bar"] `shouldBe` "!"
      myMinimumBy compare [1,53,9001,10] `shouldBe` 1

  describe "myMaximum" $ do
    it "will return 1 from [1,53,9001,10]" $ do
      myMaximum [1,53,9001,10] `shouldBe` 9001

  describe "myMinimum" $ do
    it "will return 9001 from [1,53,9001,10]" $ do
      myMinimum [1,53,9001,10] `shouldBe` 1
