*******************
 Chapter 18: Monad
*******************


18.1 Monad
----------
In this chapter, we:

* define Monad, its operations and laws;
* look at several examples of monads in practice;
* write the Monad instances for various types;
* address some misinformation about monads.

.. For a quick overview, check out this video:
.. "Haskell for imperative programmers",
.. "#17 Monad"
.. https://www.youtube.com/watch?v=IBB7JpbClo8
.. &list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
.. &index=17


18.2 Sorry -- a monad is not a burrito
--------------------------------------
A monad is an applicative functor with
some unique features that make it a bit
more powerful than either alone.

You can think of monad as another way of
applying functions over structure, with
a couple of additional features.

First let's take a look at the type class
definition:

  .. include:: figures/18.2/monad_typeclass_definition.rst

18.2.1 Applicative m
^^^^^^^^^^^^^^^^^^^^
You can derive applicative and functor in
terms of monad, just as you can derive
functor in terms of applicative.

What does this mean? It means that, for
example, that you can write ``fmap`` using
monadic operations and it works:

  ``fmap f xs``  :math:`=`  ``xs >>= return . f``

For example::

  ·∾ fmap (+1) [1,2,3]
  [2,3,4]

  ·∾ [1,2,3] >>= return . (+1)
  [2,3,4]

Try it for yourself:

.. raw:: html

   <script id="asciicast-tn9lH3aOJcsmX84oyM0Df30TS"
   src="https://asciinema.org/a/tn9lH3aOJcsmX84oyM0Df30TS.js"
   async></script>

It's important to understand this chain of
dependency:

  ``Functor`` -> ``Applicative`` -> ``Monad``

So whenever you implement an instance of
monad for a type, you necessarily have an
applicative and a functor as well.

18.2.2 Core operations
^^^^^^^^^^^^^^^^^^^^^^
The ``Monad`` type class defines three core
operations, although you only need to
define ``(>>=)`` for a minimally complete
instance.

Let's look at all three:

  | (**>>=**)  ::  **m a** -> (**a** -> m b) -> m b
  | (**>>**)   ::  m a -> m b -> m b
  | **return** ::  **a** -> **m a**

(Notice something?)

.. opposite (adj.)

   late 14c., "placed or situated on the
   other side of (something),"

   from Old French opposite, oposite
   "opposite, contrary" (13c.),

   from Latin oppositus "standing against,
   opposed, opposite," past participle of
   opponere "set against,"

   **from assimilated form of ob "in front
   of, in the way of" (see ob-) + ponere "to
   put, set, place" (see position (n.)).**

   The meaning "contrary in character, of a
   totally different nature" is from 1570s.
   As a noun from late 14c., "the opposite
   side of" (a place, the body, etc.), "an
   opposite position or condition." From
   early 15c. as "that which is opposite in
   character or quality;" also "an
   opponent." As a preposition from 1758. As
   an adverb from 1817. Related: Oppositely.

.. ob-

   tl;dr PIE *opi "near, against"

   word-forming element meaning "toward;
   against; before; near; across; down,"
   also used as an intensive,

   from Latin ob (prep.) "in the direction
   of, in front of, before; toward, to, at,
   upon, about; in the way of; with regard
   to, because of,"

   from PIE root *epi,

   also *opi "near, against"

   (see epi-).

.. topic:: Operator pronounciation guide

   * ``(>>=)`` is pronounced as "bind"
   * ``(>>)`` is sometimes called the
     "sequencing operator", though there is
     no offical name for it.
   * ``return`` is read as ... return. What
     did you expect?

``return`` puts something inside a context, it
has a default class method of ``pure = return``.

``(>>)`` sequences two actions while
discarding any resulting value of the first
action.

``(>>=)`` is what we'll talk about next.

18.2.3 The novel part of Monad
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
What's unique to monad, at least from the
point of view of types?

We already saw that it's not ``return``.
It also isn't ``(>>)``. And it also isn't
``(>>=)``, at least not in its entirety.

The type of ``(>>=)`` is visibly similar to
that of ``fmap`` and ``(<*>)``, which makes
sense since monads are applicative functors.

For the sake of making this maximally
similar, we're going to change the *m* of
monad to *f*:

.. include:: figures/18.2/fmap_ap_bind_similarities.hs
   :code:

So, the idea of mapping a function over a
value while bypassing its surrounding
structure is not unique to monad.

We can demonstrate this by fmapping a
function of type ``(a -> m b)`` to make it
more like ``(>>=)``, and it will work:

.. include:: figures/18.2/specialized_fmap_vs_bind.txt
   :code:

After mapping a function that generates
additional monadic structure in its return
type, we want a way to discard on layer of
that structure. So, how do we accomplish
that?

Well, at least with lists, we already know
how::

  ·∾ :type concat
  concat :: Foldable t => t (t a) -> t a

The module ``Control.Monad`` provides a
similar function, ``join``::

  ·∾ import Control.Monad (join)

  ·∾ :type join
  join :: Monad m => m (m a) -> m a

Monad, in a sense, is a generalization of
``concat``! ::

  ·∾ import Control.Monad (join)

  ·∾ :set -XTypeApplications

  ·∾ :type concat @[]
  concat @[] :: [[a]] -> [a]

  ·∾ :type join @[]
  join @[] :: [[a]] -> [a]

Allowing the function itself to alter the
structure is something we've not seen in
``Functor`` and ``Applicative``. The ability
to flatten those two layers of structure
into one is what makes ``Monad`` special.

By putting that ``join`` function together with
``fmap``, we can create bind.

So how do we get bind?

::

  ·∾ :{
   ⋮ bind :: Monad m => (a -> m b) -> m a -> m b
   ⋮ bind f ma = join (fmap f ma)
   ⋮ :}

  ·∾ :type bind
  bind :: Monad m => (a -> m b) -> m a -> m b

  ·∾ :type flip (>>=)
  flip (>>=) :: Monad m => (a -> m b) -> m a -> m b

  ·∾ :type join
  join :: Monad m => m (m a) -> m a

  ·∾ -- a ~ m a

18.2.4 What Monad is not
^^^^^^^^^^^^^^^^^^^^^^^^
A monad is not:

1. Impure. Monadic functions are pure
   functions.
2. An embedded language for imperative
   programming. While monads are often used
   for sequencing actions in a way that
   looks like imperative programming, there
   are commutative monads that do not order
   operations.
3. A value. Monads are type classes (or
   algebras, as a general concept), not
   values.
4. About strictness. The monadic operations
   of bind and return are nonstrict.

The ``Monad`` type class is generalized
structure manipulation with some laws to
make it sensible. Just like ``Functor`` and
``Applicative``. That's all there is to it.

18.2.5 Monad also lifts!
^^^^^^^^^^^^^^^^^^^^^^^^
The monad type class also includes a set of
``lift`` functions that are the same as the
ones we already aw i ``Applicative``. They
don't do anything different, but they are
still around because some libraries used
them before applicatives were discovered.

.. raw:: html

   <script id="asciicast-rrxHSW0M3cacRlq9Sut5Fm8et"
   src="https://asciinema.org/a/rrxHSW0M3cacRlq9Sut5Fm8et.js"
   async></script>


18.3 Do syntax and monads
-------------------------
Do syntax works with any monad, not just IO.

This section is going to talk about why
``do`` is sugar and demonstrate what the
``join`` of ``Monad`` can do for us.

To begin with, let's look at some
correspondences::

  (*>) :: Applicative f => f a -> f b -> f b
  (>>) :: Monad m =>       m a -> m b -> m b

For our purposes, ``(*>)`` and ``(>>)`` are
the same thing, sequencing functions, but
with two different constraints.

We can see what do syntax looks like after
the compiler desugars it for us by manually
transforming it ourselves:

.. include:: figures/18.3/SequencingAndBinding.hs
   :code:

When fmap alone isn't enough
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This won't work::

  ·∾ import Control.Monad

  ·∾ putStrLn <$> getLine
  Will this print?

Why? Look at the type signature::

  ·∾ :type putStrLn <$> getLine
  putStrLn <$> getLine :: IO (IO ())

This will fix it::

  ·∾ join (putStrLn <$> getLine)
  Will *this* print?
  Will *this* print?

**What join did here is merge the effects
of** ``getLine`` **and** ``putStrLn`` **into
a single IO action.** As it happens, the
cleanest way to express ordering in a lambda
calculus without bolting on something unpleasant
is thorough nesting of expressions or lambdas.

Let's get back to desugaring ``do`` syntax
with our now-enriched understanding of what
monads do for us:


18.5 Monad laws
---------------

18.5.1 Identity laws
^^^^^^^^^^^^^^^^^^^^
* **Right identity**: ``m >>= return`` :math:`=` ``m``
* **Left identity**: ``return x >>= f`` :math:`=` ``f x``

18.5.2 Associativity
^^^^^^^^^^^^^^^^^^^^
* **Associativity**:

  ``(m >>= f) >>= g`` :math:`=` ``m >>= (\x -> f x >>= g)``
