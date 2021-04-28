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
