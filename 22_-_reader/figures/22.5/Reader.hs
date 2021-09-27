module Reader where



-- figure 1, page 854
newtype Reader r a =
  Reader { runReader :: r -> a }



-- figure 2, page 854


-- instance Functor (Reader r) where
--   fmap :: (a -> b) -> Reader r a -> Reader -> r b
--   fmap f (Reader ra) = Reader (\r -> f (ra r))


{-# ANN compose "Hlint: Ignore redundant lambda" #-}
-- same as (.)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)



-- figure 5, page 855
--
-- See it?
--
--   \r -> f (ra r)
--   \x -> f (g  x)
--



-- figure 6, page 855
instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader (f . ra)
