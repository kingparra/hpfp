module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO (hSetBuffering, stdout, BufferMode(..))


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle


newtype WordList =
  WordList [String]
  deriving (Eq, Show)


allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)


minWordLength :: Int
minWordLength = 5


maxWordLength :: Int
maxWordLength = 9


gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l >= minWordLength && l < maxWordLength


randomWord :: WordList -> IO String
randomWord (WordList wl) = do
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


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn ("Your guess was: " ++ [guess])
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True,_) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False,_) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)


-- Stops the game after 7 guesses, regardless of
-- whether they're correct, even if the 7th guess
-- is the final letter to the word. Oops. This
-- function needs some improvement.
gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) =
  if (length guessed) > 7
  then do
    putStrLn ("You lose!\nThe word was: " ++ word)
    exitSuccess
  else return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ discovered _) =
  if all isJust discovered
  then do { putStrLn "You win!"; exitSuccess }
  else return ()


runGame :: Puzzle -> IO ()
runGame puzzle =
  forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn ("Current puzzle is " ++ show puzzle)
    putStr "Guess a lettter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "You guess must be a single character"
