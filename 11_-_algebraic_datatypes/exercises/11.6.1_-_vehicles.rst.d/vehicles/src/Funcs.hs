module Funcs where
import Types

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (Plane _) = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane (Car _ _) = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu (Plane _) = error "this function only works on cars"

getManuOrNothing :: Vehicle -> Maybe Manufacturer
getManuOrNothing (Car m _) = Just m
getManuOrNothing (Plane _) = Nothing

-- question 5
isCar' :: Vehicle' -> Bool
isCar' (Car' _ _) = True
isCar' (Plane' _ _) = False

isPlane' :: Vehicle' -> Bool
isPlane' (Plane' _ _) = True
isPlane' (Car' _ _) = False

areCars' :: [Vehicle'] -> [Bool]
areCars' = map isCar'

getManu' :: Vehicle' -> Manufacturer
getManu' (Car' m _) = m
getManu' (Plane' _ _) = error "this function only works on cars"
