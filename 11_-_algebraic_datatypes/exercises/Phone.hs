#!/usr/bin/env stack
-- stack --resolver lts-20.18 script --package hspec --package QuickCheck
import Test.Hspec

-- 1. Create a data structure which captures the phone
-- layout below.
--
-- +-------+--------+-------+
-- | 1     | 2 ABC  | 3 DEF |
-- +-------+--------+-------+
-- | 4 GHI | 5 JKL  | 6 MNO |
-- +-------+--------+-------+
-- | * ^   | 0 + _  | # . , |
-- +-------+--------+-------+
-- 
-- 0 is the space bar. * capitalizes the current letter.
-- To represent the digit itself you press it once 
-- more than the letter it represents.
data DaPhone =
  Key1    | Key2 | Key3 |
  Key4    | Key5 | Key6 |
  KeyStar | Key0 | KeyPound
  deriving (Eq, Ord, Show)


-- 2. Convert convo (from the test case below) into the
-- key presses required to express it.
--
-- validButtons = "1234567890*#"
data Digit = Char

type Presses = Int


reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = undefined


cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined


-- 3. How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = undefined


-- 4. What is the most popular letter for each message?
mostPopularLetter :: String -> Char
mostPopularLetter = undefined


-- 5. What is the most popular letter overall? What is
-- the overall most popular word?
coolestLtr :: [String] -> Char
coolestLtr = undefined


coolestWord :: [String] -> String
coolestWord = undefined


main :: IO ()
main = hspec $ do
  describe "taps" $ do
    it "encodes test convo to key presses" $ do
      let convo = ["Wanna play 20 questions"
                  , "Ya"
                  , "U 1st haha"
                  , "Lol OK. Have u ever tasted alcohol"
                  , "Lol ya"
                  , "Wow ur cool haha. Ur turn"
                  , "OK. Do u think I am pretty Lol"
                  , "Lol ya"
                  , "Just making sure rofl ur turn"]
      taps convo `shouldBe` undefined :: [(Digit, Presses)]
