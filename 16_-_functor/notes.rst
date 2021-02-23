*********************
 Chapter 16: Functor
*********************
https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Functor.html
https://wiki.haskell.org/Typeclassopedia#Functor

.. topic:: Short summary

   **What the hell is a functor?**

   It's a typeclass that lets you map over shit! Sometimes this
   shit is a data structure, such as a list or tree, and
   sometimes the shit is a computational context, like the data
   constructor of a wrapper type such as ``Just`` or ``Either``
   or ``IO``.

   In Haskell there is no way to enforce the laws of a type
   classes algebra. (Instead we rely on programmer discipline
   and test cases.) So ``Functor`` is simply a type class and
   what it actually does depends on the implementation of the
   instance declaration for the type in question.

   One example of a somewhat counterintuitive ``Functor``
   instance is the one for tuples, which maps over the second
   element, but not the first. Apparently, there is a
   tradition of treating tuples like name:value pairs, so the
   instance follow that tradition by not touching the first
   element.

   For example::

     >>> fmap (+1) [1..4]
     [2,3,4,5]

     >>> fmap (+1) (Just 8)
     Just 9

   More examples::

     >>> fmap show (Just 1)  --  (a   -> b)      -> f a       -> f b
     Just "1"                --  (Int -> String) -> Maybe Int -> Maybe String

     >>> fmap show Nothing   --  (a   -> b)      -> f a       -> f b
     Nothing                 --  (Int -> String) -> Maybe Int -> Maybe String

     >>> fmap show [1,2,3]   --  (a   -> b)      -> f a       -> f b
     ["1", "2", "3"]         --  (Int -> String) -> [Int]     -> [String]

     >>> fmap show []        --  (a   -> b)      -> f a       -> f b
     []                      --  (Int -> String) -> [Int]     -> [String]


   **Description**

   A type f is a Functor if it provides a function
   fmap which, given any types a and b lets you apply
   any function from (a -> b) to turn an f a into an
   f b, preserving the structure of f.


   **Laws**

   * Identity:    ``fmap id == id``
   * Composition: ``fmap (f . g) == fmap f . fmap g``
   * Structure preservation

   Note, that the second law follows from the free
   theorem of the type fmap and the first law, so you
   need only check that the former condition holds.


   **Class methods**

   * ``fmap :: (a -> b) -> f a -> f b``
   * ``(<$) :: a -> f b -> f a``
   * ``{-# MINIMAL fmap #-}``


   **Instances**

   * ``(Either a)``
   * ``[]``
   * ``Maybe``
   * ``IO``
   * ``((->) r)``
   * ``((,) a)``

   **Where is it from?**

   base and Data.Functor


.. epigraph::

   Lifting is the "cheat mode" of type tetris.

   -- Michael Neale

What is lifting, you ask?  https://wiki.haskell.org/Lifting



16.1 Functor
------------
In the 1930s the logician Rudolf Carnap coined the term
functor to describe grammatical function words that act as
logical operations over sentences or phrases.

Functors are combinators: they take a phrase as input and
return that phrase without some logical operation applied
to the whole.

This chapter will include:

* the return of the higher-kinded types;
* fmaps galore, and not only on lists;
* no more digressions about dusty logicians;
* words about type classes and constructor classes;
* puns based on George Clinton music, probably.


16.2 What's a functor?
----------------------
::

  --       input type constructor
  --         must take one type
  --         argument to become
  --           a concrete type
  --            vvvvvvvvvvvvv
  class Functor (f :: * -> *) where

    -- The only essential class method, fmap
    --
    --             takes an a
    --            inside   some
    --             container f
    --                  v
    fmap :: (a -> b) -> f a -> f b
    --      ^^^^^^^^           ^
    --     function to         |
    --   perform  on the   the enclosing type
    --    enclosed type  constructor can *change*
    --                    into any other tycon
    --                    that has an instance
    --                    of the Functor class.

    -- This operator is essentially ``fmap . const``.
    --
    (<$) :: a -> f b -> f a

    {-# MINIMAL fmap #-}
