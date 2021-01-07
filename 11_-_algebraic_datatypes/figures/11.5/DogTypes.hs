-- 11.5 Data constructors and values

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

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

badDoge :: DogueDeBordeaux String
badDoge = DogueDeBordeaux 10

-- page 396
data Doggies = Husky a | Mastiff a deriving (Eq, Show)
{-
 - -- type constructor awaiting an argument
 - Doggies
 -
 - :k Doggies
 - Doggies :: * -> *
 -
 - :t Husky
 - Husky :: a -> Doggies a
 -}
