module Four where

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWz x = fst (yToWz (xToY x))
