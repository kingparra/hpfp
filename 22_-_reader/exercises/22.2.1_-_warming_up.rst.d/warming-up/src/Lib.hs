module Lib 
  ( cap
  , rev
  , composed
  , fmapped
  , tupled
  , tupled'
  , tupledM
  , tupledM'
  ) where
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


swap (x,y) = (y,x)


-- Use applicative to define these functions.
tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev


tupled' :: String -> (String, String)
tupled' = swap . tupled


-- Write these using do syntax, then with (>>=).
tupledM :: String -> (String, String)
tupledM = do { x <-cap; y <- rev; return (x,y) }


tupledM' :: String -> (String, String)
tupledM' = swap . tupledM
