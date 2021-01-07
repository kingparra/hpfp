-- 11.11 Product types
-- page 414
--
-- ( , ) :: a -> b -> (a, b)
--
data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)
--
data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)
-- This product type has a cardinality of nine. We can figure this out by multiplying
-- the cardinality of QuantumBoth by itself. Here's the longhand version...
--
-- 1. MkTwoQs QuantumTrue QuantumTrue
-- 2. MkTwoQs QuantumTrue QuantumFalse
-- 3. MkTwoQs QuantumTrue QuantumBoth
-- 4. MkTwoQs QuantumFalse QuantumTrue
-- 5. MkTwoQs QuantumFalse QuantumFalse
-- 6. MkTwoQs QuantumFalse QuantumBoth
-- 7. MkTwoQs QuantumBoth QuantumTrue
-- 8. MkTwoQs QuantumBoth QuantumFalse
-- 9. MkTwoQs QuantumBoth QuantumBoth
--

-- page 415
type TwoQs' = (QuantumBool, QuantumBool)

-- 11.11.1 Record syntax
data Person MkPerson String Int deriving (Eq, Show)

-- page 416
-- sample data
jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

-- v typo as it appears in the book
namae :: Person -> String
namae (MkPerson s _) = s

data Person = { name :: String, age :: Int } deriving (Eq, Show)

{-
 - Prelude> :t name
 - name :: Person -> String
 -
 - Prelude> :t age
 - age :: Person -> Int
 -}

{-
 - Prelude> Person "Papu" 5
 - Person {name = "Papu", age = 5}
 -
-- page 417
 -
 - Prelude> papu = Person "Papu" 5
 -
 - Prelude> age papu
 - 5
 -
 - Prelude> name papu
 - "Papu"
 -}
