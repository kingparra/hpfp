module Lib where
import Data.Char


{-# ANN cap "HLint: ignore Eta reduce" #-}
cap :: [Char] -> [Char]
cap xs = map toUpper xs


{-# ANN rev "HLint: ignore Eta reduce" #-}
rev :: [Char] -> [Char]
rev xs = reverse xs


composed :: [Char] -> [Char]
composed = cap . rev


fmapped :: [Char] -> [Char]
fmapped = cap <$> rev


-- Use applicative to define these functions.
tupled :: [Char] -> ([Char], [Char])
tupled x = (cap x, rev x)


tupled' :: [Char] -> ([Char], [Char])
tupled' x = (rev x, cap x)


-- Write these using do syntax, then with (>>=).
tupledM :: [Char] -> ([Char], [Char])
tupledM x = (cap x, rev x)


tupled'M :: [Char] -> ([Char], [Char])
tupled'M x = (rev x, cap x)
