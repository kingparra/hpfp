#!/usr/bin/env stack
-- stack --resolver lts-20.18 script --package hspec --package QuickCheck
import Test.Hspec
import Test.QuickCheck
import Data.Char (toLower, toUpper, isUpper, isLower, isAlpha, isSpace)
import Data.List (elemIndex, lookup)
import Data.Maybe (fromJust)
import Control.Exception (evaluate)


indexOf :: Char -> Int
indexOf c = case elemIndex (toLower c) ['a'..'z'] of
              Just x -> x
              Nothing -> 0

alpha = ['a'..'z']; ualpha = ['A'..'Z']

ceasar :: Int -> Char -> Char
ceasar n c
  | not $ c `elem` (alpha ++ ualpha) = c
  | c `elem` alpha   =   alpha !! ((indexOf c + n) `mod` length alpha)
  | c `elem` ualpha  =  ualpha !! ((indexOf c + n) `mod` length ualpha)

unceasar :: Int -> Char -> Char
unceasar n c
  | not $ c `elem` (alpha ++ ualpha) = c
  | c `elem` alpha   =   alpha !! ((indexOf c - n) `mod` length alpha)
  | c `elem` ualpha  =  ualpha !! ((indexOf c - n) `mod` length ualpha)

calc :: Char -> Char -> Int
calc c k
  | c `elem` alpha  = (indexOf k - indexOf c) `mod` length alpha
  | c `elem` ualpha = (indexOf k - indexOf c) `mod` length ualpha
  | otherwise       = 0 -- set shift to 0 for non-alpha chars

vige :: String -> String -> String
vige p "" = p
vige p k =
  map (\(x,y) -> ceasar (calc 'a' y) x) $ couple p (cycle (map toLower k))
  where
    couple :: [Char] -> [Char] -> [(Char,Char)]
    couple [] _ = []
    couple _ [] = []
    couple (x:xs) (y:ys) =
      if isAlpha x
      then (x,y) : couple xs ys
      else (x,x) : couple xs (y:ys)


unvige :: String -> String -> String
unvige p "" = p
unvige p k =
  map (\(x,y) -> 
    unceasar (calc 'a' y) x) $ couple p (cycle (map toLower k))
  where
    couple :: [Char] -> [Char] -> [(Char,Char)]
    couple [] _ = []
    couple _ [] = []
    couple (x:xs) (y:ys) =
      if isAlpha x
      then (x,y) : couple xs ys
      else (x,x) : couple xs (y:ys)



main = hspec $ do

  describe "calc" $ do
    it "should produce these shifts" $ do
      calc 'a' 'b' `shouldBe` 1
      calc 'a' 'g' `shouldBe` 6

  describe "ceasar" $ do
    it "won't shift example whitespace chars" $ do
      let spaces = concat [ "\t\n\v\f\r \160\5760\8192\8193\8194\8195"
                          , "\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288"
                          ]
      ceasar 3 ' ' `shouldBe` ' '
      ceasar 3 '\t' `shouldBe` '\t'
      map (\x -> ceasar 3 x) spaces `shouldBe` spaces
    it "won't shift example non-alpha chars" $ do
      ceasar 3 '.' `shouldBe` '.'
      ceasar 3 ',' `shouldBe` ','
      ceasar 3 '!' `shouldBe` '!'
    it "won't shift generated non-alpha chars" $ do
      property $ \c -> not (isAlpha c) ==> ceasar 3 c `shouldBe` c
    it "preserves case when shifting" $ do
      ceasar 3 'T' `shouldBe` 'W'
      ceasar 3 't' `shouldBe` 'w'

  describe "unceasar" $ do
    it "will reverse a ceasared string" $ do
      map (unceasar 3) "Wklv vwulqj" `shouldBe` "This string"

  describe "vige" $ do

    context "happy path" $ do

      it "works for lowercase alpha only strings" $ do
        vige "anewdayrises" "completevictory" `shouldBe` "cbqloervdagl"
      it "works for uppercase alpha only strings" $ do
        vige "ANEWDAYRISES" "COMPLETEVICTORY" `shouldBe` "CBQLOERVDAGL"

    context "edge cases for key" $ do

      it "cycles k when it's shorter than p" $ do
        vige "tobeyourselfisallthatyoucando" "freedom" `shouldBe` "yffibcgwjipiwefcpxkofdfygdbpt"
      it "doesn't cycle k on non-alpha chars" $ do
        vige "this is the plaintext" "key" `shouldBe` "dlgc mq dlc zpysrrobr"
      it "does not encrypt p if k is empty" $ do
        vige "this random string" "" `shouldBe` "this random string"
      it "exception on non-alpha chars in k" $ do
        evaluate (vige "test for non-alpha in k" "f#@!") `shouldThrow` anyException
      it "doesn't respect case in k" $ do
        vige "ATTACKATDAWN" "LEMON" `shouldBe` vige "ATTACKATDAWN" "lemon"

  describe "unvige" $ do

    it "decodes simple input" $ do
      unvige "dlgc xchx" "key" `shouldBe` "this text"
      unvige (vige "this text, too" "key") "key"
