module Lib where

myWords :: String -> [String]
myWords = mySplit ' '

myLines :: String -> [String]
myLines = mySplit '\n'

mySplit :: Char -> String -> [String]
mySplit _  ""  = []
mySplit ch str =
  takeWhile (/= ch) str : mySplit ch (drop 1 $ dropWhile (/= ch) str)
