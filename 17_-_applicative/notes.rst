*************************
 Chapter 17: Applicative
*************************

.. topic:: Summary

   **First, some examples**

   ::

     -- Recall that <$> is infix fmap

     ·∾ (,) <$> [1,2] <*> [3,4]
     [(1,3),(1,4),(2,3),(2,4)]

     -- This time (,) is contained in a list

     ·∾ [(,)] <*> [1,2] <*> [3,4]
     [(1,3),(1,4),(2,3),(2,4)]

     ·∾ (Just (++)) <*> (Just "hello ") <*> (Just "there")
     Just "hello there"

     ·∾ import Control.Applicative (liftA2)

     ·∾ liftA2 (,) [1,2] [3,4]
     [(1,3),(1,4),(2,3),(2,4)]

   **Class methods**

   Applicative has two essential class methods, ``(<*>)``
   (pronounced "apply"), and ``pure``.

   **Laws**

   * **Identity**:      ``pure id <*> v`` :math:`=` ``v``
   * **Composition**:   ``pure (.) <*> u <*> v <*> w`` :math:`=` ``u <*> (v <*> w)``
   * **Homomorphism**:  ``pure f <*> pure x`` :math:`=` ``pure (f x)``
   * **Interchange**:   ``u <*> pure y`` :math:`=` ``pure ($ y) <*> u``

   **Where is it defined?**

   ``Applicative`` is defined in the ``base``
   package. It's exported by ``Prelude`` and
   ``Control.Applicative`` (which has a few extra
   utility functions).

   * `Docs for Control.Applicative
     <https://hackage.haskell.org/
     package/base-4.10.1.0/docs/
     Control-Applicative.html
     #t:Applicative>`_

17.1 Applicative
----------------
Applicatives are monoidal functors. The applicative
type class allows for function application lifted
over structure (like functor). But with applicative
the function we're applying is also embedded in some
structure.

In this chapter, we will:

* define and explore the ``Applicative`` type class
  and its core operations;
* demonstrate why applicatives are monoidal functors;
* make the usual chitchat about laws and instances;
* do a lot of lifting;
* give you some ``Validation``.


17.2 Defining Applicative
-------------------------
The first thing you'll notice here is that the type
argument to ``Applicative`` is also a functor.

.. include:: figures/17.1/ghci_info_applicative.txt
   :code:

The two core operations of ``Applicative`` are
``pure`` and ``<*>`` (pronounced "apply").

``pure`` lifts some value into an applicative
context.

The ``<*>`` operator is described as "sequential
application". It applies monoids contained in one
functor to the contents of another functor (as seen
above).

Along with these core functions, the
``Control.Applicative`` module provides some other
convenient functions:: ``liftA``, ``liftA2``, and
``liftA3``.

::

  liftA :: Applicative f
        => (a -> b) -> f a -> f b

  liftA2 :: Applicative f
         => (a -> b -> c) -> f a -> f b -> f b

  liftA2 :: Applicative f
         => (a -> b -> c -> d) -> f a -> f b -> f b -> f d


17.3 Functor vs. Applicative
----------------------------
Let's review the difference between ``fmap`` and
``<*>``::

  (<$>) :: Functor f      =>    (a -> b) -> f a -> f b
  (<*>) :: Applicative f  =>  f (a -> b) -> f a -> f b

The difference is we now have an ``f`` in front of
our function ``(a -> b)``.  The increase in power it
introduces is profound.

For one thing, any ``Applicative`` also has a ``Functor``
and not merely by definition -- you can define a functor
in terms of a provided ``Applicative`` instance.

``fmap f x`` :math:`=` ``pure f <*> x``

Here's an example::

  ·∾ import Control.Applicative

  ·∾ fmap (+1) [1,2,3]
  [2,3,4]

  ·∾ pure (+1) <*> [1,2,3]
  [2,3,4]

Keeping in mind that pure has type ``Applicative f => a
-> f a``, we can think of it as a means of embedding a
value of any type in the structure we're working with::

  ·∾ pure 1 :: [Int]
  [1]
  ·∾ :type it
  it :: [Int]

  ·∾ pure 1 :: Maybe Int
  Just 1
  ·∾ :type it
  it :: Maybe Int

  ·∾ pure 1 :: Either a Int
  Right 1
  ·∾ :type it
  it :: Either a Int

  ·∾ pure 1 :: ([a], Int)
  ([],1)
  ·∾ :type it
  it :: ([a], Int)

The left type is handled differently from the right in
the final two examples for the same reason as here::

  ·∾ fmap (+1) (4,5)
  (4,6)

The left type is part of the structure, and the structure
is not transformed by the function application.


17.4 Applicative functors are monoidal functors
-----------------------------------------------
First let us notice something::

  ($)   ::   (a -> b) ->   a ->   b
  (<$>) ::   (a -> b) -> f a -> f b
  (<*>) :: f (a -> b) -> f a -> f b

In the type signature above, ``mappend`` is our function
from ``(a -> b)``.

::

  ·∾ (Just (*2)) <*> (Just 2)
  Just 4

  ·∾ (Just (*2)) <*> Nothing
  Nothing

  ·∾ Nothing <*> (Just 2)
  Nothing

  ·∾ Nothing <*> Nothing
  Nothing

With ``Maybe``, the ordinary functor is mapping
over the possibility of a value's nonexistence.
With the ``Applicative``, now the function also
might not be provided.

17.4.1 Show me the monoids
^^^^^^^^^^^^^^^^^^^^^^^^^^
Recall that the ``Functor`` instance for the two-tuple
ignores the first value inside the tuple::

  ·∾ fmap (+1) ("blah",0)
  ("blah",1)

  ·∾ :info (,)
  data (,) a b = (,) a b  -- Defined in ‘GHC.Tuple’
  instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
  instance (Monoid a, Monoid b) => Monoid (a, b) -- Defined in ‘GHC.Base’

Alright, that makes sense so far. But what about this? ::

  ·∾ ("Woo", (+1)) <*> (" Hoo!",0)
  ("Woo Hoo!",1)

The first elements of the pair were combined without
explicitly supplying a function. What has happened, here?

No explanation for you. What about *this*? ::

  ·∾ import Data.Monoid

  ·∾ (Sum 2, (+1)) <*> (Sum 0, 0)
  (Sum {getSum = 2},1)

  ·∾ (Sum 2,(+1)) <*> (Sum 0,0)
  (Sum {getSum = 2},1)

  ·∾ (Product 3,(+9)) <*> (Product 2,8)
  (Product {getProduct = 6},17)

  ·∾ (All True,(+1)) <*> (All False,0)
  (All {getAll = False},1)

17.4.2 Tuple Monoid and Applicative side by side
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Squint if you can't see it::

  instance (Monoid a, Monoid b) => Monoid (a,b) where
    mempty                  = (mempty, mempty)
    (a,b) `mappend` (a',b') = (a `mappend` a', b `mappend` b')

  instance Monoid a => Applicative ((,) a) where
    pure x          = (mempty, x)
    (u,f) <*> (v,x) = (u `mappend` v, f x)

17.4.3 Maybe Monoid and Applicative
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
While applicatives are monoidal functors, be careful
about making assumptions based on this.

For one thing, ``Monoid`` and ``Applicative`` instances
aren't required or guaranteed to have the same monoid of
structure, and the functorial part may change the way it
behaves.


17.5 Applicative in use
-----------------------

17.5.1 List Applicative
^^^^^^^^^^^^^^^^^^^^^^^
With the list applicative, we are mapping a plurality of
functions over a plurality of values.

::

  ·∾ [(+1),(*2)] <*> [2,4]
  [3,5,4,8]

  ·∾ -- [(+1),(*2)] <*> [2,4]  ≡  [3,5,4,8]  ≡  [(+1)2,(+1)4,(*2)2,(*2)4]

  ·∾ ([(+1),(*2)] <*> [2,4]) == [3,5,4,8] &&
       [(+1)2,(+1)4,(*2)2,(*2)4] == [3,5,4,8]
  True

  ·∾ (,) <$> [1,2] <*> [3,4]
  [(1,3),(1,4),(2,3),(2,4)]

  ·∾ import Control.Applicative

  ·∾ liftA2 (+) [1,2] [3,5]
  [4,6,5,7]

  ·∾ max <$> [1,2] <*> [1,4]
  [1,4,2,4]

  ·∾ liftA2 max [1,2] [1,4]
  [1,4,2,4]

If you're familiar with Cartesian products, this probably
looks a lot like one, but with functions.

More examples! ::

  ·∾ :load figures/17.5/LookupTables.hs
  [1 of 1] Compiling LookupTables     ( figures/17.5/LookupTables.hs, interpreted )
  Ok, one module loaded.

  ·∾ f 3
  Just "hello"

  ·∾ g 8
  Just "chris"

  ·∾ (++) <$> f 3 <*> g 7
  Just "hellosup?"

  ·∾ (+) <$> h 5 <*> m 1
  Just 9007

  ·∾ (+) <$> h 5 <*> m 6
  Nothing

  ·∾ liftA2 (++) (g 9) (f 4)
  Just "alohajulie"

  ·∾ liftA2 (^) (h 5) (m 4)
  Just 60466176

  ·∾ liftA2 (*) (h 5) (m 4)
  Just 60

  ·∾ liftA2 (*) (h 1) (m 1)
  Nothing

  ·∾ -- Your applicative context can also sometimes be IO:
  ·∾ (++) <$> getLine <*> getLine
  one
  two
  "onetwo"

  ·∾ (,) <$> getLine <*> getLine
  one
  two
  ("one","two")
