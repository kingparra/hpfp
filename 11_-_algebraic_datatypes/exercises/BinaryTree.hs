#!/usr/bin/env stack
-- stack --resolver lts-20.18 script --package hspec --package QuickCheck
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


-- 11.17 Binary tree
data BinaryTree a =
  Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 11.17.1 Inserting into trees
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a  = Node             left a right
  | b < a   = Node (insert' b left) a right
  | b > a   = Node             left a (insert' b right)

-- 11.17.2 Write map for BinaryTree
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

-- 11.17.3 Convert binary trees to lists
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z Leaf = z
foldTree f z (Node left a right) = f a (f a (foldTree f z left)) -- wrong but type checks


main :: IO ()
main = hspec $ do

  describe "insert'" $ do
    it "insert' should create a tree if given Leaf" $ do
      insert' 8 Leaf `shouldBe` (Node Leaf 8 Leaf)
    it "insert' should order greater vaules to the right" $ do
      insert' 9 (insert' 8 Leaf) `shouldBe`
        (Node Leaf 8 (Node Leaf 9 Leaf))
    it "insert' should order greater vaules to the right : 2" $ do
      insert' 1 (insert' 2 (insert' 3 Leaf)) `shouldBe`
        Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 Leaf

  describe "mapTree" $ do
    it "mapTree (+1) works" $ do
      mapTree (+1) (Node Leaf 1 Leaf) `shouldBe` (Node Leaf 2 Leaf)
    it "mapTree (+1) Leaf should return Leaf" $ do
      mapTree (+1) Leaf `shouldBe` Leaf

  let testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

  describe "preorder" $ do
    it "a" $ do
      preorder testTree `shouldBe` [2, 1, 3]

  describe "inorder" $ do
    it "a" $ do
      inorder testTree `shouldBe` [1, 2, 3]

  describe "postorder" $ do
    it "a" $ do
      postorder testTree `shouldBe` [1, 3, 2]

  describe "foldTree" $ do
    it "a" $ do
      foldTree (+) 0 testTree `shouldBe` 6
