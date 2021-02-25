module MeaninglessTypeClasses where

-- 16.4 Let's talk about f baby
-- 16.4.1 Shining star come into view
-- page 627
class Sumthin a where
  s :: a -> a
  -- kind *

class Else where
  e :: b -> f (g a b c)
  --   b :: * ->
  --   f :: (* -> *)
  --   (g a b c) :: * -> * -> * -> *

class Biffy where
  slayer :: e a b -> (a -> c) -> (b -> d) -> e c d

-- page 629
-- The kind checker is going to fail on the next couple
-- of examples:
class Impish v where
  impossibleKind :: v -> v a

class AlsoImp v where
  nope :: v a -> v
