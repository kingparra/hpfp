module Lib where
import Data.Function ((&))
import Data.List (intercalate, words, elemIndices)


-- Question 1

-- helper function
notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

-- primary function
replaceThe :: String -> String
replaceThe s =
  words s &
  map notThe &
  map (\x -> case x of {Nothing -> Just "a"; Just y -> Just y}) &
  map (\(Just x) -> x) &
  intercalate " "

-- With Data.Text I could write ``replaceThe s = replace "the" "a" s``
-- instead, but that feels like cheating.


-- Question 2
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s =
  let
    idxOfThes = if length (words s) > 1
                then Just $ elemIndices "the" (init (words s))
                else Nothing
  in
    if idxOfThes == Nothing
    then
      0
    else
      map (\x -> (words s) !! (x+1)) ((\(Just x) -> x) idxOfThes) &
      filter (\x -> (head x) `elem` "aeiou") &
      length &
      toInteger


-- Question 3
countVowels :: String -> Integer
countVowels s = filter (\x -> x `elem` "aeiou") s & length & toInteger
