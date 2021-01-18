module Lib where


data BinaryTree a =
  Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


-- From Ch11, 11.17 Binary Tree, 11.17.1 Inserting into trees, page 447
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b t = case t of
  Leaf         ->   Node Leaf b Leaf
  Node left a right
    | b == a   ->   Node      left          a        right
    |  b < a   ->   Node (insert' b left)   a        right
    |  b > a   ->   Node      left          a   (insert' b right)


-- From 11.17.2 Write map for BinaryTree, page 449
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)


-- An intermediate step, for my own understanding.
flattenTree :: BinaryTree a -> [a]
flattenTree       Leaf          = []
flattenTree (Node left a right) = [a] ++ (flattenTree left) ++ (flattenTree right)


-- To make interacting with ghci easier
testTree =  Node (Node (Node Leaf 2 Leaf)
                  1
                  (Node Leaf 2 Leaf))
            0
            (Node (Node Leaf 2 Leaf)
                  1
                  (Node Leaf 2 Leaf))


-- From 11.17.4 Write foldr for BinaryTree, page 452
-- any traversal order is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b t = foldr f b (flattenTree t)


-- Question 1
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
  Just (l, m, r) -> Node (unfold f l) m (unfold f r)
  Nothing -> Leaf


-- Question 2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold (\x -> if   x >= n
                then Nothing
                else Just (x+1, x, x+1)) 0
