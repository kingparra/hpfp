module Lib where

mySqr  = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- Question 1
tuples = [(x,y) | x <- mySqr, y <- myCube]

-- Question 2
tuplesLT50 = [(x,y) | x <- mySqr, y <- myCube, x < 50 && y < 50]

-- Question 3
tuplesLT50Count = length tuplesLT50
