-- 11.13 Constructing and deconstructing values
-- page 421
data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b } deriving (Eq, Show)


-- page 422
-- 11.13.1 Sum and Product
newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
data Farmhouse' = Product' NumCow NumPig deriving (Eq, Show)


-- page 423
newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

-- Product can take another Product as an argument, which allows nesting.
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

-- We can perform a similar trick with Sum.
type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)


-- page 424
data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)
-- Alternately...
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)


-- page 425
-- 11.13.2 Constructing values
trivialValue :: GuessWhat
trivialValue = Chickenbutt


-- page 426
-- MkId :: a -> Id a
idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool
type Name = String

person :: Product Name Awesome
person = Product "Simon" True

-- See "data Sum = ..." above

data Twitter = Twitter deriving (Eq, Show)

data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter


-- page 427
data SocialNetwork = Twitter | AskFm deriving (Eq, Show)


-- TODO add figures for page 428 and on. Add ghci output to previous figures.
