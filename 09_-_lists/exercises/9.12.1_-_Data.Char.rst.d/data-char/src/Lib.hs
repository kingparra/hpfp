module Lib where
import Data.Char

-- Question 1
-- I don't know how to query the type of this function
-- without GHCi, so here they are as a comment.
--
-- ·∾ :type isUpper
-- isUpper :: Char -> Bool
--
-- ·∾ :type toUpper
-- toUpper :: Char -> Char

-- Question 2
onlyUpper = filter isUpper

-- Question 3
capFirstLetter [] = []
capFirstLetter (x:xs) = toUpper x : xs

-- Question 4
{-# ANN capAll ("HLint: Ignore all" :: String) #-}
capAll [] = []
capAll (x:xs) = toUpper x : capAll xs

-- Question 5
firstCapCh [] = error "cannot convert empty string to char"
firstCapCh x = toUpper $ head x

-- Another version, using Maybe to represent the
-- possiblity of failure instead of execptions.
-- firstCapChMaybe :: String -> Maybe Char
-- firstCapChMaybe  [] = Nothing
-- firstCapChMaybe (x:_) = Just (toUpper x)

-- Question 6
firstCapChPointfree = toUpper . head
