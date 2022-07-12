module Lib where


-- Question 1
stops  = ['p','b','t','d','k','g']
vowels = ['a','e','i','o','u']

-- 1a
sts = [(s,v,s') | s <- stops, v <- vowels, s' <- stops]

-- 1b
stsp = [(s,v,s') | s <- stops, v <- vowels, s' <- stops, s == 'p']

-- 1c
nouns = ["house","car","banana"]
verbs = ["build","drive","peel"]

nvn = [(n,v,n') | n <- nouns, v <- verbs, n' <- nouns]


-- Question 2
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

avgWordLength x = word_lengths `div` word_count
  where
    all_words    = words x
    word_lengths = sum . map length $ all_words
    word_count   = length all_words


-- Question 3
avgWordLengthFrac x = word_lengths / word_count
  where
    all_words    = words x
    word_lengths = fromIntegral $ sum . map length $ all_words
    word_count   = fromIntegral $ length all_words
