{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- 11.9 newtype
--
-- page 407
-- Semantically different, but representationally the same. This prevents us
-- from mixing them up without incurring any cost in memory for a new type
-- definition.
--
-- newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

{-
 - Prelude> tooManyGoats (Goats 43)
 - True
 -
 - Prelude> tooManyGoats (Cows 43)
 - Couldn't match expected type
 -   Goats with actual type Cows
 - In the first argument of
 -   toomanyGoats, namely (Cows 42)
 - In the expression: tooManyGoats (Cows 43)
 -}

-- page 408
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- How is this different? Try it out in GHCi.
-- instance TooMany Goats where
--   tooMany (Goats n) = n > 43

-- page 409
-- This will allow you to derive instances for your newtype
-- from the definition of the underlying type. It's not the
-- default behaviour and must be enabled.
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- page 410
--
-- instance TooMany a where
--   tooMany n = n > 42
--
newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)
--                              ^^^^^^^
--                        the magic happens here!
