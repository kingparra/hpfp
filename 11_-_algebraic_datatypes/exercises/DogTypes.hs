#!/usr/bin/env stack
-- stack --resolver lts-20.18 script --package hspec --package QuickCheck
{-# LANGUAGE OverloadedStrings #-}
-- https://www.fpcomplete.com/haskell/tutorial/stack-script/
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


data BinaryTree =
  Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a  = Node             left a right
  | b < a   = Node (insert' b left) a right
  | b > a   = Node             left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node undefined undefined undefined


main :: IO ()
main = hspec $ do

  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)
    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "insert'" $ do
    it "inserts greater numbers to the right" $ do
      insert' 8 $ insert' 7 $ insert' 6 (Node Leaf 4 (Node Leaf 5 Leaf)) `shouldBe`
        Node Leaf 4 (Node Leaf 5 (Node Leaf 6 (Node Leaf 7 (Node Leaf 8 Leaf))))

  describe "mapTree" $ do
    it "mapTree (+1) works" $ do
      mapTree (+1) (Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)) `shouldBe`
        (Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf))

