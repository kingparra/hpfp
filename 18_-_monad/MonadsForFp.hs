#!/usr/bin/env stack
-- stack --resolver lts-20.18 script
{-# LANGUAGE OverloadedStrings #-}
import Data.Array

-- 2.1 Variation zero: The basic evaluator
-- ---------------------------------------
-- -- First, let's build an evaluator, in the simplest possible way.
--
--data Term = Con Int | Div Term Term deriving (Eq, Ord, Show)
----
-- eval :: Term -> Int
-- eval (Con a) = a
-- eval (Div t u) = eval t `div` eval u

-- answer, error' :: Term
-- answer  = (Div  (Div (Con 1972) (Con 2))  (Con 23))
-- error'  = (Div  (Con 1)  (Con 0))


-- 2.2 Variation one: Exceptions
-- -----------------------------
-- -- Say we want to reutn an error message
--
-- data M a = Raise Exception | Return a deriving (Eq, Ord, Show)
--
-- type Exception = String
-- eval :: Term -> M Int
-- eval (Con a)   = Return a
-- eval (Div t u) = case eval t of
--                    Raise e -> Raise e
--                    Return a ->
--                      case eval u of
--                        Raise e -> Raise e
--                        Return b ->
--                           if b == 0
--                             then Raise "divide by zero"
--                             else Return (a ÷ b)


-- 2.3 Variation two: State
-- ------------------------
-- -- Say we want to count how many operations occur in
-- -- the evaluation process.
--
-- type State = Int
-- type M a   = State -> (a, State)
--
-- eval :: Term -> M Int
-- eval (Con a)   x = (a, x)
-- eval (Div t u) x = let (a, y) = eval t x in
--                    let (b, z) = eval u y in
--                    (a `div` b, z + 1)
--
-- >>> eval answer 0
-- (42,2) -- two steps to reduce answer


-- 2.4 Vairation three: Output
-- ---------------------------
-- -- Say we want to display a trace of execution.
-- type M a = (Output, a)
-- type Output = String

-- eval :: Term -> M Int
-- eval (Con a)   = (line (Con a) a, a)
-- eval (Div t u) = let (x, a) = eval t in
--                  let (y, b) = eval u in
--                  (x ++ y ++ line (Div t u) (a `div` b), a `div` b)

-- line :: Term ->  Int -> Output
-- line t a = "eval(" ++ show t ++ ") <= " ++ show a ++ "\n"

-- ·∾ eval (Con 1972)
-- ("eval(Con 1972) <= 1972\n",1972)
--
-- ·∾ eval (Con 2)
-- ("eval(Con 2) <= 2\n",2)
--
-- ·∾ eval (Div (Con 1972) (Con 2))
-- ("eval(Con 1972) <= 1972\neval(Con 2) <= 2\neval(Div (Con 1972) (Con 2)) <= 986\n",986)
--
-- ·∾ eval (Con 23)
-- ("eval(Con 23) <= 23\n",23)
--
-- ·∾ eval (Div     (Div  (Con 1972) (Con 2))  (Con 23)    )
-- ( "eval(Con 1972) <= 1972\neval(Con 2) <= 2\neval(Div (Con 1972) (Con 2)) <= 986\n
--   \eval(Con 23) <= 23\neval(Div (Div (Con 1972) (Con 2)) (Con 23)) <= 42\n"
-- , 42
-- )
--
-- Say we want to modify the the previous program to
-- print the trace in reverse.
--
-- Just replace the term ‘(x ++ y ++ line (Div t u) (a `div` b),  a `div` b)’
-- with                  ‘(line (Div t u) (a `div` b) ++ x ++ y,  a `div` b)’.
--


-- 2.5 A monadic evaluator
-- -----------------------
-- -- Say we want to abstract out the common pattern of all of these evaluators.
--


-- 4 State
-- -------
--
-- 4.1 Arrays
-- ----------
-- newarray :: Val -> Arr
-- index    :: Ix  -> Arr -> Val
-- update   :: Ix  -> Val -> Arr -> Arr

-- type State = Array
-- type Ix    = Id
-- type Val   = Int

-- data Id    = Id deriving Show
-- data Term  = Var Id         | Con Int       | Add Term Term
-- data Comm  = Asgn Id Term   | Seq Comm Comm | If Term Comm Comm
-- data Prog  = Prog Comm Term

-- eval :: Term -> State -> Int
-- eval (Var i)   x = x ! i
-- eval (Con a)   x = a
-- eval (Add t u) x = eval t x + eval u x

-- exec :: Comm => State -> State
-- exec (Asgn i t) x  = (!) (eval t x) x
-- exec (Seq c d)  x  = exec d (exec c e)
-- exec (If t c d) x  = if eval t x == 0 then exec c x else exec d x

-- elab :: Prog -> Int
-- elab (Prog c t) = eval t (exec c (array


-- 4.2 Array transformers
-- ----------------------
-- type M a = State -> (a, State)
-- type State = Arr

-- unit :: a -> M a
-- unit a = \x -> (a, x)

-- (>>=) :: M a -> (a -> M b) -> M b
-- m >>= k = \x -> let (a,y) = mx in
--                 let (b, z) = k a y in
--                 (b, z)
