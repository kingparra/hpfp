module Lib where

one    =  [x^y | x <- [1..5], y <- [2, undefined]]

two    =  take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]

three  =  sum [1, undefined, 3]

four   =  length [1, 2, undefined]

five   =  length $ [1, 2, 3] ++ undefined

six    =  take 1 $ filter even [1, 2, 3, undefined]

seven  =  take 1 $ filter even [1, 3, undefined]

eight  =  take 1 $ filter odd [1, 3, undefined]

nine   =  take 2 $ filter odd [1, 3, undefined]

ten    =  take 3 $ filter odd [1, 3, undefined]
