#!/usr/bin/env stack
-- stack --resolver lts-20.18 script --package hspec --package text
import Data.List (intersperse)
import Data.Char (toUpper)
import Test.Hspec


-- 1. Write a function that capitalizes a word.
capWord l@(x:xs) = toUpper x : xs

-- 2. Write a function that capitalizes sentences in a paragraph.
-- Recognize when a new sentence has begun by checking for periods.
splitPara p
  | p == "" = []
  | otherwise = 
     [takeWhile (/='.') p ++ "."] ++ splitPara (drop 2 $ dropWhile (/='.') p)

capPara p = concat $ intersperse " " $ map capWord $ splitPara p


main :: IO ()
main = hspec $ do
  describe "capitalizeParagraph" $ do
    it "Capitalizes da paragraph, dude!" $ do
      capPara "blah. woot ha." `shouldBe` "Blah. Woot ha."
