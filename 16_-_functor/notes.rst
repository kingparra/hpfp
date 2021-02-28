*********************
 Chapter 16: Functor
*********************
https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Functor.html
https://wiki.haskell.org/Typeclassopedia#Functor

.. topic:: Short summary

   **What the hell is a functor?**

   It's an algebra that lets you map over things!
   Sometimes this is a data structure, like a list or
   tree, and sometimes this is computational context,
   such as ``Just`` or ``Either`` or ``IO``.

   In Haskell there is no way to enforce the laws of a
   type classes algebra. (Instead we rely on programmer
   discipline.) So ``Functor`` is simply a type class and
   what it actually does depends on the implementation of
   the instance declaration for the type in question.

   One example of a somewhat counterintuitive ``Functor``
   instance is the one for pairs, which alters the second
   element of the tuple, but not the first. Apparently,
   there is a tradition of treating tuples as name:value
   pairs, and the instance follows that tradition by not
   touching the first element.

   This is also a consequence of functors *kind constraint*
   of ``(f :: * -> *)``, which means that any type
   constructor that can be an instance of ``Functor`` must
   take exactly one argument. This, again, is an
   implementation detail. Conceptually, n-ary functors
   exist. To define them in the general case for any number
   of arguments and type constructors in Haskell would
   require dependent types, though.

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
A functor is a way to apply a function to the inhabitants
of some structure that we don't want to alter.

.. The book says "Other means of implementing [Functor] are
.. possible [rather than using type classes], but this is
.. the most convenient way to do so." What *are* those other
.. methods?

.. include:: figures/16.2/SomeFunctor.hs
   :code:


16.4 Let's talk about f baby
----------------------------
We know that the ``f`` in our ``Functor`` definition must
be kind ``* -> *`` for a couple of reasons, which we will
first describe and then demonstrate:

1. Each argument (and result) in the type signature must
   be fully applied. Each argument must have the kind
   ``*``.
2. The type ``f`` was applied to a single argument in two
   different places, ``f a`` and ``f b``. Since ``f a``
   and ``f b`` must each have the kind ``*``, ``f`` by
   itself must be kind ``* -> *``.

16.4.1 Shining star come into view
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
**Every argument to the type constructor of ``->`` must
be of kind ``*``.** ::

  ·∾ :kind (->)
  (->) :: * -> * -> *

Given this knowledge, we can know something about
``Functor`` from the type of ``fmap``::

  class Functor f where
      fmap      :: (a -> b)  ->  f a  ->  f b
  --  has kind:       *           *        *

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
