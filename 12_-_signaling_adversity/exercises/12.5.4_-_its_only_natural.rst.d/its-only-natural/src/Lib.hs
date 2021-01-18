module Lib where

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x = case signum x of 
  (-1) -> Nothing
  0    -> Just Zero
  1    -> Just (go x)
  where 
    go :: Integer -> Nat
    go 0 = Zero
    go n = Succ (go (n-1))
