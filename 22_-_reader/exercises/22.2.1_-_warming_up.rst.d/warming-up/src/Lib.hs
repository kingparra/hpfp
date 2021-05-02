module Lib where
import Data.Char


{-# ANN cap "HLint: ignore Eta reduce" #-}
cap :: String -> String
cap xs = map toUpper xs


{-# ANN rev "HLint: ignore Eta reduce" #-}
rev :: String -> String
rev xs = reverse xs


composed :: String -> String
composed = cap . rev


fmapped :: String -> String
fmapped = cap <$> rev


-- Use applicative to define these functions.
tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev


tupled' :: String -> (String, String)
tupled' = (,) <$> rev <*> cap


-- Write these using do syntax, then with (>>=).
tupledM :: String -> (String, String)
tupledM x = (cap x, rev x)


tupled'M :: String -> (String, String)
tupled'M x = (rev x, cap x)
