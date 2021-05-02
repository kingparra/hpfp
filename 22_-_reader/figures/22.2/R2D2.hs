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
bloop = fmap boop doop



-- page 846, figure 4
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop


duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop



-- page 849, figure 16
--
boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)
