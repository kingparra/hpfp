module Lib where


-- Question 1
data Quant a b = Finance | Desk a | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f Finance   = Finance
  fmap f (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)


-- Question 2
data K a b = K a
{- justsomeguy
 
     Is it impossible to define an instance of Functor
     for the type ''data K a b = K a''?

     I keep on getting type errors if I try something
     like ''instance Functor (K a) where { fmap f (K
     a) = K (f a) }''.

   koz_

     justsomeguy: That's just Const. So it is very
     possible.

     The problem you're having is that 'f' has a
     different type to what you're thinking. It's
     not a -> c, it's _b_ -> c.

     In Const, the second type parameter is phantom.
     So you can change it to whatever whenever.

     So you just ignore the function and rebuild the
     Const with the same value 'inside' it always had.

     You can think of 'Const a b' as 'an a pretending
     to be a b'.  So we can have it pretend to be a
     c, and you only pass the function so fmap is happy.  -}
instance Functor (K a) where
  fmap f (K a) = K a
-- (K a) b
-- fmap 
-- Prelude> :t K
-- K :: a -> K a b
-- Prelude> :kind K
-- K :: * -> * -> *

-- Question 3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
--                   ^     ^
--                   |     |
--                        (\k a b -> b a) b' a'
--                            [ a := b' ]
--                        (\k b -> b b') a'
--                            [ b := a' ]
--                        (\k -> a' b')
--                            a' b'
newtype K' a b = K' a

-- This should remind you of an instance you've written before.
-- instance Functor (Flip K' a) where
-- (Flip (K' a))
-- (((Fip K') a) b)
-- fmap :: (x->y) -> (Flip K' a) x -> (Flip K' a) y 
--  fmap f (Flip (K' b)) = Flip (K' (f b))


-- -- Question 4
-- data EvilGoateeConst a b = GoatyConst b
-- -- You thought you'd escaped the goats
-- -- by now didn't you? Nope.


-- -- Question 5
-- data LiftItOut f a = LiftItOut (f a)


-- -- Question 6
-- data Parrapa f g a = DaWrappa (f a) (g a)


-- -- Question 7
-- data IgnoreOne f g a b =
--  IgnoringSomething (f a) (g b)

-- -- Question 8
-- data Notorious g o a t =
--  Notorious (g o) (g a) (g t)


-- -- Question 9
-- data List a = Nil | Cons a (List a)


-- Question 10
data GoatLord a 
   = NoGoat
   | OneGoat a
   | MoreGoats (GoatLord a)
               (GoatLord a)
               (GoatLord a)
-- A VERITABLE HYDRA OF GOATS

{-
            *
       /    |     \
   NoGoat   *    (OneGoad "Larry")
          / | \
        No  *  O "Page"
           ...
-}
instance Functor (GoatLord) where
  fmap f (NoGoat) = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) =
   (MoreGoats (fmap f a) (fmap f b) (fmap f c))
   
-- Question 11
data TalkToMe a 
   = Halt 
   | Print String a 
--   Print [Char] a
   | Read (String -> a)

instance Functor TalkToMe where
-- fmap :: (a->b) -> f a -> f b 
  fmap f Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (f . g)
  -- [(String, a)]