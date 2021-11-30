module FunctorKind where
-- 16.4 Let's talk about f baby
-- 16.4.1 Shining star come into view
-- page 627, fig 2

class Functor f where
    fmap      :: (a -> b)  ->  f a  ->  f b
--  has kind:       *           *        *
