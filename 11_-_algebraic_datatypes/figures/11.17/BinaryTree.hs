-- page 446
data BinaryTree a
  = Leaf
  | Node (BinaryTee a) a (BinaryTree a)
  deriving (Eq, Ord, Show)
