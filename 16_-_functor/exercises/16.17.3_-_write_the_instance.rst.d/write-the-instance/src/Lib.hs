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
-- justsomeguy
--
--   Is it impossible to define an instance of Functor
--   for the type ''data K a b = K a''?
--
--   I keep on getting type errors if I try something
--   like ''instance Functor (K a) where { fmap f (K
--   a) = K (f a) }''.
--
-- koz_
--
--   justsomeguy: That's just Const. So it is very
--   possible.
--
--   The problem you're having is that 'f' has a
--   different type to what you're thinking. It's
--   not a -> c, it's _b_ -> c.
--
--   In Const, the second type parameter is phantom.
--   So you can change it to whatever whenever.
--
--   So you just ignore the function and rebuild the
--   Const with the same value 'inside' it always had.
--
--   You can think of 'Const a b' as 'an a pretending
--   to be a b'.  So we can have it pretend to be a
--   c, and you only pass the function so fmap is happy.
--
-- So the question becomes, why does f take a "b" as
-- input rather than an "a"?
--
-- It turns out that instance declarations operate on the
-- outermost (or rightmost) type argument first. This is
-- due to the outermost reduction strategy that Haskell
-- employes.
--
-- More here:
--
--   https://www.youtube.com/watch?v=QBQ9_9R7o8I
--   &list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
--   &index=31
--
-- One property of the church-rosser theorem is:
--
-- If there is some reduction strategy that terminates,
-- the outermost reduction strategy will *always* terminate.
--
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



-- Question 4
data EvilGoateeConst a b = GoatyConst b deriving (Show)
-- You thought you'd escaped the goats
-- by now didn't you? Nope.

instance Functor (EvilGoateeConst b) where
  fmap f (GoatyConst b) = GoatyConst (f b)



-- Question 5
data LiftItOut f a = LiftItOut (f a)

-- This compiles, but does it make sense?
instance Functor a => Functor (LiftItOut a) where
  fmap f (LiftItOut g) = LiftItOut (fmap f g)



-- Question 6
data Parrapa f g a = DaWrappa (f a) (g a) deriving (Show,Eq)
-- f and g are type constructors that wrap their argument, a
-- Show is needed for viewing in ghci, Eq is needed for the test case

instance (Functor g, Functor h) => Functor (Parrapa g h) where
  fmap f (DaWrappa g h) = DaWrappa (fmap f g) (fmap f h)



-- Question 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g b) where
  fmap f (IgnoringSomething x y) =
    IgnoringSomething x (fmap f y)



-- Question 8
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor (Notorious g o t) where
  fmap = undefined


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
        /   |    \
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
