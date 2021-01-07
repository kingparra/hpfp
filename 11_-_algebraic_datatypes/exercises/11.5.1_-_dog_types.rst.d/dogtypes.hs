-- page 393
data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

-- page 395
myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData
-- no witness to the contrary ^

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- This won't work because 10 cannot be recondiled with the type variable being
-- bound to String. (Commented out so this module will still compile.)
{-
badDoge :: DogueDeBordeaux String
badDoge = DogueDeBordeaux 10
-}


-- page 396
data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

-- Type constructor awaiting an argument:
{-
Doggies
-}
-- This needs to be applied to become a concrete type.
