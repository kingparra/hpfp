module Lib
  ( DatabaseItem(..)
  , theDatabase
  , filterDbDate
  , filterDbNumber
  , mostRecent
  , sumDb
  , avgDb
  )
where
import Data.Time


data DatabaseItem =
  DbString String | DbNumber Integer | DbDate UTCTime
  deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]


-- Question 1
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate l =
  map fromDbDate $ filter isDbDate l
  where
    fromDbDate (DbDate date) = date
    isDbDate e = case e of
      DbDate (UTCTime _ _) -> True
      _                    -> False


-- Question 2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber l =
  map fromDbNumber $ filter isDbNumber l
  where
    fromDbNumber (DbNumber num) = num
    isDbNumber e = case e of
      DbNumber _ -> True
      _          -> False


-- Question 3
mostRecent :: [DatabaseItem] -> UTCTime
-- What if the list of DatabaseItems is empty?
-- Should I change this to work on NonEmpty lists,
-- or return a Maybe UTCTime, instead?
mostRecent [] = error "empty list"
mostRecent xs = maximum . filterDbDate $ xs


-- Question 4
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber



-- Question 5
avgDb :: [DatabaseItem] -> Double
avgDb db =
  dbSum / dbLen
  where
    dbSum = fromIntegral (sumDb db)
    dbLen = fromIntegral (length $ filterDbNumber db)
