module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


main :: IO ()
main = do
  putStrLn "hello world"


type WordList = [String]


allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)


minWordLength :: Int
minWordLength = 5


maxWordLength :: Int
maxWordLength = 9


gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l >= minWordLength && l < maxWordLength


randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex


randomWord' :: IO String
randomWord' = gameWords >>= randomWord


data Puzzle =
  Puzzle String [Maybe Char] [Char]
  --     word   discovered   guessed
  --      [1]        [2]      [3]
  --
  --  1. The word we're trying to guess.
  --
  --  2. Placeholder for all letters in word. Letters we've
  --     discovered by guessing correctly are represented as
  --     Just c, and undiscovered letters are represented as
  --     Nothing.
  --
  --  3. All the letters we've guessed so far, both correct
  --     and incorrect.


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c


instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed


freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word [Nothing | _ <- word] []


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word


alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discovered guessed) c =
  -- Always add the guessed character to our list of gusses in Puzzle.
  --                          vvvvvvvvvvvvv
  Puzzle word newlyDiscovered (c : guessed)
  where
    newlyDiscovered = zipWith (zipper c) word discovered
    -- Compare the character c with the current character
    -- from the word, named targetCh, and the current
    -- character from the already discovered characters,
    -- named discoveredMaybeCh. If it matches the character
    -- from the target word, we return it wrapped in Just.
    -- Otherwise we return the already discovered Maybe
    -- Char.
    zipper :: Char -> Char -> Maybe Char -> Maybe Char
    zipper c targetCh discoveredMaybeCh =
      if c == targetCh
      then Just targetCh
      else discoveredMaybeCh
