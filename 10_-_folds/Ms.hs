#!/usr/bin/env stack
-- stack script --resolver lts-18.26 --package hspec
module Ms where
import Test.Hspec


main = hspec $ do
  describe "ms" $ do
    it "returns [z] for empty lists" $ do
      ms (+) 0 []          `shouldBe` [0]
    it "works on simple cases" $ do
      ms (+) 0 [1]         `shouldBe` [1,0]
      ms (+) 0 [1,2]       `shouldBe` [3,2,0]
      ms (+) 0 [1,2,3,4,5] `shouldBe` [15,14,12,9,5,0]


ms :: (a -> b -> b) -> b -> [a] -> [b]
ms f z l =
  case l of
    []     -> [z]
    (x:xs) -> foldr f z l : ms f z xs
