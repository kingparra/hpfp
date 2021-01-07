-- 11.7 Data constructor arities
-- page 401
-- nullary
data Example0 = Example0 deriving (Eq, Show)

-- unary
data Example1 = Example1 Int deriving (Eq, Show)

-- product of Int and String
data Example2 = Example2 Int String deriving (Eq, Show)

-- page 401
{-
 - Prelude> Exmaple0
 - Example0
 -
 - Prelude> Example1 10
 -
-- page 402
 -
 - 10
 -
 - Prelude> Example1 10 == Example1 42
 - False
 -
 - Prelude> nc = Example2 1 "NC"
 - Prelude> Example2 10 "FlappyBat" == nc
 - False
 -}

data MyType = MyVal Int deriving (Eq, Show)

{-
 - Prelude> :type myVal
 - MyVal :: Int -> MyType
 -
 - Prelude> MyVal 10
 - MyVal 10
 -
 - Prelude> MyVal 10 == MyVal 10
 - True
 -
 - -- page 403
 -
 - Prelude> MyVal 10 == MyVal 9
 - False
 -
 -}
