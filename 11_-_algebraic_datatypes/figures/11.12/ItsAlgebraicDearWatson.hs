-- 11.12 Normal form

-- page 417
--
-- All the existing algebraic rules for products and sums apply in type
-- systems, and that includes the distributive property.
--
-- 2 * (3 + 4)
--   2 * (7)
--     14
--
-- 2 * 3 + 2 * 4
--   (6) + (8)
--      14
--
-- a * (b + c) ≡ (a * b) + (a * c)
--

-- page 418
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

-- type AuthorName = String
-- data Author = Author (AuthorName, BookType)

-- page 419
type AuthorName = String
data Author
  = Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

data Expr
  = Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr
--
-- begin fig 2
--
-- A stricter interpretation of normal form or "sum of products" would
-- require representing products with tuples and sums with Either.
--
type Number =  Int
type Add    = (Expr, Expr)
type Minus  =  Expr
type Mult   = (Expr, Expr)
type Divide = (Expr, Expr)

type Expr =
  Either Number
    (Either Add
      (Either Minus
        (Either Mult Divide)))
--
-- This representation finds applications in problems where one is writing
-- functions or folds over the representations of datatypes, such as with
-- generics and metaprogramming.
--
-- end fig 2
