module Lib where

eftBool :: Bool -> Bool -> [Bool]
eftBool True  False  =  []
eftBool True  True   =  [True]
eftBool False True   =  [False,True]
eftBool False False  =  [False]

eft :: (Ord t, Enum t) => t -> t -> [t]
eft from to | from > to   =  []
            | from == to  =  [from]
            | from < to   =  from : eft (succ from) to

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to = eft from to

eftInt :: Int -> Int -> [Int]
eftInt from to = eft from to

eftChar :: Char -> Char -> [Char]
eftChar from to = eft from to
