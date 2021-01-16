module Lib where

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s =
  let 
    numVowels = length $ filter (`elem` "aeiou") s
    numConsonants = length $ filter (`elem` "bcdfghjklmnpqrstvwxzy") s
  in
    if   numVowels > numConsonants 
    then Nothing
    else Just (Word' s)

    
