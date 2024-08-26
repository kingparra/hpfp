data Cow = Cow {
    name :: String
  , age  :: Int
  , weight :: Int
  } deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str


noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing


weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let { w = weight c; n = name c } in
  if n == "Bess" && w > 499
  then Nothing
  else Just c


mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weight
              weightCheck (Cow nammy agey weighty)


-- ·∾ mkSphericalCow "Bess" 5 499
-- Just (Cow {name = "Bess", age = 5, weight = 499})
-- ·∾ mkSphericalCow "Bess" 5 500
-- Nothing

mkSphericalCow' name' age' weight' = do
  nammy   <- noEmpty name'
  agey    <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

-- ·∾ mkSphericalCow' "Bess" 5 500
-- Nothing
-- ·∾ 
-- ·∾ mkSphericalCow' "Bess" 5 499
-- Just (Cow {name = "Bess", age = 5, weight = 499})
--

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  \nammy ->
    noNegative age' >>=
    \agey ->
     noNegative weight' >>=
     \weighty ->
      weightCheck (Cow nammy agey weighty)

