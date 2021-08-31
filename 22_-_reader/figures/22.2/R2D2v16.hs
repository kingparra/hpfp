#!/usr/bin/env stack
{- stack script
 --resolver lts-16.28
 --package hspec
-}
module R2D2 where
import Control.Applicative



-- page 844, figure 1
boop = (*2)
doop = (+10)


bip :: Integer -> Integer
bip = boop . doop



-- page 845, figure 2
bloop :: Integer -> Integer
bloop = fmap boop boop



-- page 845, figure 3
--
-- fmap boop doop x == (*2) ((+10) x)



-- page 846, figure 4
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop


duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop



-- page 846, figure 5
--
-- Prelude> bbop 3
-- 19
--



-- page 846, figure 6
--
-- That does something like this:
--
-- ((+) <$> (*2) <*> (+10)) 3
--
-- First the fmap
--
-- (*2) :: Num a => a -> a
-- (+)  :: Num a => a -> a -> a
-- (+) <$> (*2) :: Num a => a -> a -> a
--


-- page 846, figure 7
--
-- (+) . (*2) :: Num a => a -> a -> a
--



-- page 846, figure 8
--
-- Prelude> ((+) . (*2)) 5 3
-- 13
--
-- Prelude> ((+) <$> (*2)) 5 3
-- 13
--



-- page 847, figure 9
--
-- ((+) <$> (*2)) 5 3
--
-- Keeping in mind that (<$>) is the (.)
-- operator under the hood.
--
-- ((+) . (*2)) 5 3
--
-- Remember that function composition
-- desugars to a lambda like this:
-- (f . g) ≡ (\x -> f (g x)).
--
-- ((+) . (*2)) == (\x -> (+) (2 * x))
--



-- page 847, figure 10
--
-- ((+) . (*2)) 5 3
-- (\x -> (+) (2 * x)) 5 3
-- (\5 -> (+) (2 * 5)) 3
-- ((+) 10) 3
-- 13
--



-- page 847, figure 11
--
-- ((+) <$> (*2) <*> (+10)) 3
--
-- -- Wait, what? What happened to the first
-- -- argument?
--
-- ((+) <$> (*2) <*> (+10)) :: Num b => b -> b
--



-- page 847, figure 12
--
-- Prelude> :t (<*>)
-- (<*>) :: Applicative f =>
--   f (a -> b) -> f a -> f b
--



-- page 848, figure 13
--
-- appReader :: (a -> a -> b)
--           -> (a -> a)
--           -> (a -> b)
-- appReader = (<*>)
--



-- page 848, figure 14
--
-- ((+) <$> (*2) <*> (+10)) 3
-- (3*2) + (3+10)
-- 6 + 13
-- 19
--



-- page 848, figure 15
--
-- module Web.Shipping.Utils ((<||>)) where
--
-- import Control.Applicative (liftA2)
--
-- (<||>) :: (a -> Bool)
--        -> (a -> Bool)
--        -> a
--        -> Bool
-- (<||>) = liftA2 (||)
--



-- page 849, figure 16
--
boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

