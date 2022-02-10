module Lib where


mySqr = [ x^2 | x <- [1..10]]


one = [ x
      | x <- mySqr
      , x `rem` 2 == 0
      ]


two = [ (x,y)
      | x <- mySqr
      , y <- mySqr
      , x < 50
      , y > 50
      ]


three =
  take 5 [ (x,y)
         | x <- mySqr
         , y <- mySqr
         , x < 50
         , y > 50
         ]
