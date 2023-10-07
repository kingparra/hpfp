#!/usr/bin/env stack
-- stack --resolver lts-20.18 script --package hspec --package split
import Test.Hspec
import Data.Char (toUpper)
import Data.List.Split (endBy)
import Data.List (intersperse)


-- example function from the book
doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp l@(x:_) = x : l


-- 1. This should return true iff all the values in the first list appear in the second list, though they need not be contiguous.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True -- an empty list is a subsequence of anything
isSubseqOf _ [] = False -- anything other than an empty list cannot be a subsequence of an empty list
-- isSubseqOf xs ys = all (`elem` ys) xs
isSubseqOf l@(x:xs) (y:ys)
  | x == y    = isSubseqOf xs ys
  | otherwise = isSubseqOf l ys


-- 2. Split a sentence into words, then tuple each one with its capitalized form.
capitalizeWords :: String -> [(String,String)]
capitalizeWords l =
  zip (words l) (map capitalize (words l)) 
  where 
    capitalize l@(x:xs) = toUpper x : xs


main :: IO ()
main = hspec $ do

  describe "doubleUp" $ do

    it "works for the provided examples" $ do
      doubleUp ([] :: Num a => [a]) `shouldBe` []
      doubleUp [1] `shouldBe` [1,1]
      doubleUp [1,2] `shouldBe` [1,1,2]
      doubleUp [1,2,3] `shouldBe` [1,1,2,3]

  describe "isSubseqOf" $ do

    it "contiguous substrings match" $ do
      isSubseqOf "blah" "blahwoot"  `shouldBe`  True
      isSubseqOf "blah" "wootblah"  `shouldBe`  True

    it "uncontiguous substrings also match" $ do
      isSubseqOf "blah" "wboloath"  `shouldBe`  True
      isSubseqOf "blah" "blawhoot"  `shouldBe`  True

    it "every element must match" $ do
      isSubseqOf "blah" "wootbla"   `shouldBe`  False

  describe "capitalizeWords" $ do
    
    it "works for this particular test case :D" $ do
      capitalizeWords "hello world" `shouldBe`
        [("hello","Hello"),("world","World")]
