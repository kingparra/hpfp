#!/usr/bin/env stack
-- stack --resolver lts-20.18 script --package hspec
import Data.List.Split (splitOn)
import Data.List (intersperse)
import Data.Char (toUpper)
import Test.Hspec


-- 1. Write a function that capitalizes a word.
capitalizeWord l@(x:xs) = toUpper x : xs


-- 2. Write a function that capitalizes sentences in a paragraph.
-- Recognize when a new sentence has begun by checking for periods.
capitalizeParagraph p =
  let sentences = map (++".") (splitOn ". " p)
  in concat $ intersperse " " $ map (\(x:xs) -> toUpper x : xs) sentences


main :: IO ()
main = hspec $ do
  describe "capitalizeParagraph" $ do
    it "returns the first element of a list" $ do
      capitalizeParagraph "blah. woot ha" `shouldBe` "Blah. Woot ha."
