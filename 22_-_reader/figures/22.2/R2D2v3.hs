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
