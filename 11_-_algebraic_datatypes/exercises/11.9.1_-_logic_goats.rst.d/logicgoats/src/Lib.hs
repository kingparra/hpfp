module Lib where

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- 11.9 newtype
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

-- page 408
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- page 410
instance TooMany a where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)
--                              ^^^^^^^
--                        the magic happens here!
