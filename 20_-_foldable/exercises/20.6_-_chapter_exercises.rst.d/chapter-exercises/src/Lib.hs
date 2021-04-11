module Lib where




-- Question 1
data Constant a b = Constant b deriving (Eq, Show)


instance Foldable (Constant b) where
  foldMap _ _ = mempty




-- Question 2
data Two a b = Two a b deriving (Eq, Show)


instance Foldable (Two b) where
  foldMap f (Two _ b) = (f b)




-- Question 3
data Three a b c = Three a b c deriving (Eq, Show)


-- Why isn't this (Three c), instead of
-- (Three b c)? Or even (Three a b c)?
instance Foldable (Three b c) where
  foldMap f (Three _ _ c) = f c




-- Question 4
data Three' a b = Three' a b b deriving (Eq, Show)


instance Foldable (Three' b) where
  -- (x, y, z) ==> (_, y, z)
  foldr f acc (Three' _ y z) = (y `f` (z `f` acc))




-- Question 5
data Four' a b = Four' a b b b deriving (Eq, Show)


instance Foldable (Four' b) where
  foldr f acc (Four' _ x y z) = x `f` (y `f` (z `f` acc))




-- Question 6
-- Write a filter function for Foldable types using foldMap.
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a) )
        => (a -> Bool) -> t a -> f a
filterF pred t =
  foldMap (\x -> if pred x then pure x else mempty) t
