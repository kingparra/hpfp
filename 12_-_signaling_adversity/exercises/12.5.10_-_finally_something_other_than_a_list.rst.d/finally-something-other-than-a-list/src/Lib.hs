module Lib where


data BinaryTree a =
  Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


-- Question 1
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold = undefined


-- Question 2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = undefined
