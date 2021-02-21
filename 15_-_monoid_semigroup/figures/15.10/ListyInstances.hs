-- 15.10.4 The problem of orphan instances
-- page 602
module ListyInstances where

import Data.Monoid
import Listy

instance Monoid (Listy a) where
  mempty = Listy []

instance Semigroup (Listy a) where
  (<>) (Listy l) (Listy l') = Listy (l `mappend` l')
