#!/usr/bin/env stack
{- stack script
 --resolver lts-16.28
 --package hspec
-}
module R2D2 where
import Control.Applicative



-- page 854, figure 1
boop = (*2)
doop = (+10)


bip :: Integer -> Integer
bip = boop . doop



-- page 854, figure 2
bloop :: Integer -> Integer
bloop = fmap boop boop



-- page 855, figure 3
--
-- fmap boop doop x == (*2) ((+10) x)
--
-- When this x comes along, it's the
-- first necessary argument (+10)
-- then the result for that is the
-- first necessary argument to (*2).



-- page 855, figure 4
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop


duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop



-- page 855, figure 5
--
--   Prelude> bbop 3
--   19
--
-- That does something like this:
--
--   ((+) <$> (*2) <*> (+10)) 3
--
-- First the fmap
--
--   (*2)         :: Num a => a -> a
--   (+)          :: Num a => a -> a -> a
--   (+) <$> (*2) :: Num a => a -> a -> a
--
-- Mapping a function awaiting two arguments
-- over a function awaiting one produces a 
-- two argument function.



-- page 856, figure 6
--
--   (+) . (*2) :: Num a => a -> a -> a
--
--   Prelude> ((+) . (*2)) 5 3
--   13
--
--   Prelude> ((+) <$> (*2)) 5 3
--   13
--
-- todo .. continue here ...
