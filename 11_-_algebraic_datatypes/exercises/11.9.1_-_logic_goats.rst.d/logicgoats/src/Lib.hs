{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Lib where

newtype Cows  = Cows Int deriving (Eq, Show)
newtype Goats = Goats Int deriving (Eq, Show, TooMany)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42


-- Question 1
newtype LivestockNumNamePair =
  LivestockNumNamePair (Int, String)
  deriving (Eq, Show)

-- Using a newtype
instance TooMany LivestockNumNamePair where
  tooMany n = case n of
    LivestockNumNamePair (a,_) -> a > 42

instance TooMany (Int,String) where
  tooMany (n,_) = n > 42
-- End question 1


-- Question 2
newtype GoatsPair =
  GoatsPair (Int,Int)
  deriving (Eq, Ord, Show)

instance TooMany GoatsPair where
  tooMany (GoatsPair (x,y)) = (x+y) > 42

instance TooMany (Int,Int) where
  tooMany (n,m) = (n+m) > 42
-- End question 2


-- Question 3

instance (Num a, TooMany a) => TooMany (a,a) where
  tooMany (x,y) = tooMany x || tooMany y
-- ·∾ tooMany (3 :: Int, 8 :: Int)

-- <interactive>:4:1: error:
--     • Overlapping instances for TooMany (Int, Int)
--         arising from a use of ‘tooMany’
--       Matching instances:
--         instance (Num a, TooMany a) => TooMany (a, a)
--           -- Defined at /home/chris/Projects/hpfp/11_-_algebraic_datatypes/
--           exercises/11.9.1_-_logic_goats.rst.d/logicgoats/src/Lib.hs:44:10
--         instance TooMany (Int, Int)
--           -- Defined at /home/chris/Projects/hpfp/11_-_algebraic_datatypes/
--           exercises/11.9.1_-_logic_goats.rst.d/logicgoats/src/Lib.hs:37: 10
--     • In the expression: tooMany (3 :: Int, 8 :: Int)
--       In an equation for ‘it’: it = tooMany (3 :: Int, 8 :: Int)
-- ·∾ tooMany (3 :: Int, "hi")
-- False
-- End question 3
