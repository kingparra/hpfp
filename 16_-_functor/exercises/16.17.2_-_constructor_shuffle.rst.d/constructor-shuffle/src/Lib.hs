module Lib where


-- Question 1
data Sum b a = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a)  =  First (f a)
  fmap f (Second b) =  Second b


-- Question 2
data Company a c b = DeepBlue a c | Something b 
  deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (Something b)  =  Something (f b)
  fmap _ (DeepBlue a c) =  DeepBlue a c


-- Question 3
data More b a = L a b a | R b a b 
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a')  =  L (f a) b (f a')
  fmap f (R b a b')  =  R b (f a) b'
