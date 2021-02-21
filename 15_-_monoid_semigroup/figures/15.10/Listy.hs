-- 15.10.4 The problem of orphan instances
-- page 602
module Listy where

newtype Listy a = Listy [a] deriving (Eq, Show)

-- Duplicate of instance in ListyInstances.hs
instance Monoid (Listy a) where
  mempty = Listy []

instance Semigroup (Listy a) where
  (<>) (Listy l) (Listy l') = Listy (l `mappend` l')

-- [Listy changed]
--
-- Listy.hs:8:10: error:
--     Duplicate instance declarations:
--       instance [safe] Monoid (Listy a) -- Defined at Listy.hs:8:10
--       instance Monoid (Listy a) -- Defined at ListyInstances.hs:8:10
--   |
-- 8 | instance Monoid (Listy a) where
--   |          ^^^^^^^^^^^^^^^^
--
-- Listy.hs:11:10: error:
--     Duplicate instance declarations:
--       instance [safe] Semigroup (Listy a) -- Defined at Listy.hs:11:10
--       instance Semigroup (Listy a) -- Defined at ListyInstances.hs:11:10
--    |
-- 11 | instance Semigroup (Listy a) where
--    |          ^^^^^^^^^^^^^^^^^^^
