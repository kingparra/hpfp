module Lib where

data BinaryTree a =
  Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)


foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree _ z Leaf = z
foldrTree f z (Node left a right) =
  f a (foldrTree f z' right)
  where z' = foldrTree f z left


instance Functor BinaryTree where
  fmap = mapTree


instance Foldable BinaryTree where
  foldr = foldrTree


-- instance Traversable BinaryTree where
--   -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse g (Leaf x) = Leaf <$> (g x)
--   traverse f (Node left a right) =
--     Node <$> (mapTree f left) (f a) (mapTree f right)
--   mapTree  f (Leaf x) = Leaf  $  (f x)
--   3Blue1Brown -- linear algebra

