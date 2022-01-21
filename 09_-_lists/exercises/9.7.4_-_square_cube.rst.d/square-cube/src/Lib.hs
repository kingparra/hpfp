module Lib where


mySqr = [ x^2 | x <- [1..10]]
myCube = [y^3 | y <- [1..5]]


-- Question 1
one = [ (x,y) | x <- mySqr, y <- myCube]


-- Question 2
two = [ (x,y) | (x,y) <- one, x < 50 && y < 50 ]


-- Question 3
three = length two
