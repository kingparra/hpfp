-- page 447
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a  =  Node left a right
  | b < a   =  Node (insert' b left) a right
  | b > a   =  Node left a (insert' b right)
