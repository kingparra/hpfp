module Lib where
import Data.Monoid


data Optional a = Nada | Only a deriving (Eq, Show)


instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend = (<>)

-- (<>) must be defined for Semigroup or we'll get
-- a compiler error that GHC can't infer how (<>)
-- will behave from the definition of mappend in
-- the Monoid instance.
instance Monoid a => Semigroup (Optional a) where
  (<>)   Nada     Nada    =   Nada
  (<>)   Nada   (Only b)  =  (Only b)
  (<>) (Only a)   Nada    =  (Only a) 
  (<>) (Only a) (Only b)  =  (Only (a `mappend` b))
