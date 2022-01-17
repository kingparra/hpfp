module Lib where

import Data.Char (ord, chr, toLower, toUpper, isUpper)
import Data.Maybe (fromJust)
import Data.List (elemIndex)


alpha = "abcdefghijklmnopqrstuvwxyz"


shift :: Int -> Char -> Char
shift n c = 
  if toLower c `elem` alpha 
  then
    if isUpper c 
    then toUpper (alpha !! newIndex)
    else (alpha !! newIndex)
  else c
  where
    newIndex = (oldIndex + n) `mod` 26
    oldIndex = fromJust (elemIndex (toLower c) alpha)


unshift :: Int -> Char -> Char
unshift n c = shift (-n) c
