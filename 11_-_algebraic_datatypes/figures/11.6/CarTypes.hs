-- 11.6 What's a type and what's data
-- page 397
data Price = Price Integer deriving (Eq, Show)

-- page 398
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

-- page 399
data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

-- 11.6.1 Exercises: Vehicles
-- page 400
myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir
