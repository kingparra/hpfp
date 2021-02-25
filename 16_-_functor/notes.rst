*********************
 Chapter 16: Functor
*********************
https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Functor.html
https://wiki.haskell.org/Typeclassopedia#Functor

.. topic:: Short summary

   **What the hell is a functor?**

   It's an algebra that lets you map over shit! Sometimes
   this is a data structure, like a list or tree, and
   sometimes this is computational context, such as
   ``Just`` or ``Either`` or ``IO``.

   In Haskell there is no way to enforce the laws of a type
   classes algebra. (Instead we rely on programmer
   discipline.) So ``Functor`` is simply a type class and
   what it actually does depends on the implementation of
   the instance declaration for the type in question.

   One example of a somewhat counterintuitive ``Functor``
   instance is the one for pairs, which alters the second
   element of the tuple, but not the first. Apparently,
   there is a tradition of treating tuples as name:value
   pairs, and the instance follows that tradition by not
   touching the first element.

   Unlike some other algebras, it can be proven that each
   type has at most one functor. (Via the free theorem of
   the type of fmap, whatever the hell that means.)

   GHC can even derive ``Functor`` for you, if you turn on
   the ``DeriveFunctor`` language pragma.

   **Examples**

   ::

     >>> fmap (+1) [1..4]
     [2,3,4,5]

     >>> fmap (+1) (Just 8)
     Just 9

     >>> fmap show (Just 1)  --  (a   -> b)      -> f a       -> f b
     Just "1"                --  (Int -> String) -> Maybe Int -> Maybe String

     >>> fmap show Nothing   --  (a   -> b)      -> f a       -> f b
     Nothing                 --  (Int -> String) -> Maybe Int -> Maybe String

     >>> fmap show [1,2,3]   --  (a   -> b)      -> f a       -> f b
     ["1", "2", "3"]         --  (Int -> String) -> [Int]     -> [String]

     >>> fmap show []        --  (a   -> b)      -> f a       -> f b
     []                      --  (Int -> String) -> [Int]     -> [String]

   **Laws**

   * Identity:    ``fmap id`` :math:`=` ``id``
   * Composition: ``fmap (f . g)`` :math:`=` ``fmap f . fmap g``
   * Structure preservation

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

   **Where is it defined?**

   * ``Prelude``
   * ``Data.Functor``
   * ``Control.Monad``


16.1 Functor
------------
Rudolf Carnap, a logician in the 1930s, coined the term
functor to describe grammatical function words that operate
over sentences or phrases.

Understanding ``Functor`` and ``Applicative`` is important
to a deep understanding of ``Monad``.

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
  --
  --                             may be a differnt
  --               takes an a   value than a
  --              inside some      /
  --               container f    /
  --                    v        v
    fmap :: (a -> b) -> f a -> f b
  --        ^^^^^^^^           ^
  --       function to         |
  --     perform  on the   this is the smae f!
  --      enclosed type
  --
    (<$) :: a -> f b -> f a -- Essentially ``fmap . const``
    {-# MINIMAL fmap #-}


16.4 Let's talk about f baby
----------------------------

16.4.5 A shining star for you to see what your f can truly be
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Notice a similarity between ``$`` and ``<$>`` (infix
``fmap``)::

  (<$>) :: Functor f => (a -> b) -> f a -> f b
    $   ::              (a -> b) ->   a ->   b


16.17 Chapter Exercises
-----------------------

.. include:: exercises/16.17.1_-_valid_functor.rst

.. include:: exercises/16.17.2_-_constructor_shuffle.rst

.. include:: exercises/16.17.3_-_write_the_instance.rst
