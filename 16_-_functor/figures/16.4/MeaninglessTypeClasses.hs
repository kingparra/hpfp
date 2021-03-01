{-# LANGUAGE MultiParamTypeClasses #-}
module MeaninglessTypeClasses where
-- 16.4 Let's talk about f baby
-- 16.4.1 Shining star come into view
-- page 627



-- Example 1
------------
class Sumthin a where
  s :: a -> a
  -- The argument and result type are both a. There's
  -- nothing else, so a has kind *.



-- Example 2
------------
class Else where
  e :: b -> f (g a b c)
  --
  --
  --   b :: * ->
  --
  --   1. This b, like a in the previous example, stands
  --      alone as the first arument to (->), so tit is
  --      kind*.
  --
  --
  --   f :: (* -> *)
  --
  --   2. Here f is the outermost type constructor for
  --      the second argument (the result type) of (->). It
  --      takes a single argument, the type (g a b c). Thus,
  --      f has kind * -> *.
  --
  --
  --   (g a b c) :: * -> * -> * -> *
  --
  --   3. And g is applied to three arguments: a, b, and c.
  --      That means it is kind * -> * -> * -> *, where:
  --
  --      g :: * -> * -> * -> *
  --
  --      -- a, b, and c are each kind *
  --
  --      g :: * -> * -> *  ->     *
  --      g    a    b    c     (g a b c)
  --



-- Example 3
------------
class Biffy where
  slayer :: e a b -> (a -> c) -> (b -> d) -> e c d



-- Example 4
------------
-- The kind checker is going to fail on the
-- next couple of examples.

-- Remember that the name of the variable
-- before the where in a type class definition
-- binds the occurences of that name throughout
-- the definition.

-- GHC will notice that our v sometimes has a
-- type argument and sometimes not; then throw
-- the following errors.

class Impish v where
  impossibleKind :: v -> v a

{-
 -
 -  MeaninglessTypeClasses.hs:52:26: error:
 -      • Expected kind ‘k0 -> *’, but ‘v’ has kind ‘*’
 -      • In the type signature: impossibleKind :: v -> v a
 -        In the class declaration for ‘Impish’
 -     |
 -  52 |   impossibleKind :: v -> v a
 -     |                          ^^^
 -
 -}

class AlsoImp v where
  nope :: v a -> v

{-
 -
 - MeaninglessTypeClasses.hs:80:18: error:
 -     • Expecting one more argument to ‘v’
 -       Expected a type, but ‘v’ has kind ‘k0 -> *’
 -     • In the type signature: nope :: v a -> v
 -       In the class declaration for ‘AlsoImp’
 -    |
 - 80 |   nope :: v a -> v
 -    |                  ^
 -
 -}
