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
