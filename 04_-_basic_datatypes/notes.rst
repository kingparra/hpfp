****************************
 Chapter 4: Basic datatypes
****************************


4.1 Basic datatype
------------------
In this chapter, we will:

* review types we have seen in previous chapters;
* learn about datatypes, type constructors, and data constructors;
* work with predefined datatypes;
* introduce control flow with if-then-else expressions;
* learn more about type signatures and a bit about type classes.


4.2 What are types?
-------------------
* Every value has an associated type.
* Types are groups of values that share something in common.
* In Haskell values and types live in different namespaces:
  term-level and type-level.


4.3 Anatomy of a data declaration
---------------------------------
You can examine the type of an expression with ``:type``.

You can examine the definition of a datatype with ``:info``.

Here is a simple datatype declaration::

  --               OR
  --                v
  data Bool = False | True
  --    ^        ^      ^
  --   type       \     |
  -- constructor  data constructors

The type constructor is the name of the type. It lives in
the type level.

Data constructors are either concrete values or functions to
construct concrete values, and lives in the term level.

You can have a type constructor and data constructor with
the same name, and it will be unambiguous, because they live
in different namespaces (type level and term level).

.. include:: exercises/4.3.1_-_exercises_mood_swing.rst


4.4 Numeric types
-----------------

Integral numbers
^^^^^^^^^^^^^^^^
Theses are whole numbers, positive and negative.

``Int``

  A fixed-precision integer type with at least the range
  -2^29..2^29-1. The exact range for a given implementation
  can be determined by using ``minBound`` and ``maxBound``
  from the ``Prelude.Bounded`` class. ::

    ·∾ 1 :: Int
    1
    ·∾ minBound :: Int
    -9223372036854775808
    ·∾ maxBound :: Int
    9223372036854775807

``Integer``

  Arbitrarily large (or small) numbers. ::

    ·∾ minBound :: Integer

    <interactive>:33:1: error:
        • No instance for (Bounded Integer)
            arising from a use of ‘minBound’
        • In the expression: minBound :: Integer
          In an equation for ‘it’: it = minBound :: Integer

``Word``

  An unsigned integral type, with the same size as Int. The
  smallest number is 0. Word is suitable when you want to
  express whole digits that don't include negative numbers.

Fractional
^^^^^^^^^^
``Float``

 Single-precision floating point numbers. Floating point can
 shift how many bits it uses to represent numbers before or
 after the decimal point.

 The implementation details of how this happens can lead to
 some weird behaviour. For example ``0.3 + 0.3 + 0.3`` =
 ``0.8999999999999999``, rather than ``.9``. Because of the
 potential to lose precision, it's probably a good idea to
 use other types where possible.

``Double``

  Double-precision floating point numbers. It has twice as
  many bits to describe numbers as ``Float`` does.

``Rational``

  A fractional number that represents a ratio of two
  ``Integer``'s. ``Rational`` is arbitrarily precise but not as
  efficient as ``Scientific``.

  You can use it like::

    ·∾ 3 / 4 :: Rational
    3 % 4

  Some fractional types have a instance of ``Real``, which
  provides ``toRational``. Using it, you can do
  conversions::

    ·∾ toRational 3.8
    4278419646001971 % 1125899906842624

  There is also a more general datatype called ``Ratio``
  that allows a ratio of any two ``Integral`` types, rather
  than ``Integer`` types specifically. You can use it like::

    ·∾ import Data.Ratio
    ·∾ 3 % 8
    3 % 8

``Fixed``

  A fixed precision type that provides factional types of
  predefined resolution::

    ·∾ import Data.Fixed
    ·∾ Fixed E
    E0  E1  E12  E2  E3  E6  E9
    ·∾ 0.01 :: Fixed E2
    0.01
    ·∾ (0.3 :: Fixed E2) + 0.3 + 0.3
    0.90

``Scientific``

  Almost arbitrary precision. The coefficient is an
  ``Integer`` and the exponent is an ``Int``.

  This type comes from the `scientific
  <https://hackage.haskell.org/package/scientific>`_
  package.

  An example::

    ❯❯❯ stack ghci --package scientific
    ·∾ import Data.Scientific
    ·∾ scientific 8 20
    8.0e20
    ·∾ scientific 8 20 + scientific 99 3
    8.00000000000000099e20

All of these numeric types have an instance of a type class
called ``Num``, which provides ``+``, ``-``, and ``*``. This
means that those operations are polymorphic::

  ·∾ 8 * (8 :: Float)
  64.0
  ·∾ 8 * (8 :: Word)
  64

However, if you give them mismatched types, you'll still
have to explicitly convert one of them::

  ·∾ (8 :: Integer) * (8 :: Int)

  <interactive>:8:19: error:
      • Couldn't match expected type ‘Integer’ with actual type ‘Int’
      • In the second argument of ‘(*)’, namely ‘(8 :: Int)’
        In the expression: (8 :: Integer) * (8 :: Int)
        In an equation for ‘it’: it = (8 :: Integer) * (8 :: Int)


4.5 Comparing values
--------------------
* Haskell has the standard relational operators ``==``,
  ``/=``, ``<``, ``>``.
* Greater than and less than also check ordering, and are
  part of the Ord typeclass.
* ``==`` and ``/=`` are part of the Eq typeclass.


4.6 Go on and Bool me
---------------------
* ``&&`` logical and
* ``||`` logical or
* ``not``
* The if-then-else control flow construct is an expression
  in Haskell, not a statement. ``if condition then
  expression else expression``, where expression must have
  the same type in both arms.

.. include:: exercises/4.6.1_-_exercises_find_the_mistakes.rst


4.7 Tuples
----------
Tuples are heterogeneous structures which contain a fixed number of values.

::

  ·∾ :type (1,'o',True)
  (1,'o',True) :: Num a => (a, Char, Bool)

  ·∾ :type (1,"aletheia")
  (1,"aletheia") :: Num a => (a, [Char])

They are actually a family of types. There is a type constructor and data
constructor for each arity, up to 32 arguments, defined in ``GHC.Tuple``.

::

  ·∾ :info (,)
  data (,) a b = (,) a b  -- Defined in ‘GHC.Tuple’

  ·∾ :info (,,,)
  data (,,,) a b c d = (,,,) a b c d      -- Defined in ‘GHC.Tuple’

You'll often hear tuples of different arities described as n-tuple, or
three-tuple, or triple, or pair, or unit (for the empty tuple ``()``).

Each tuple constructor has implementations of different type classes that permit
operations on them, but sometimes the behaviour is unexpected::

  ·∾ length (1,2)
  1

  ·∾ fmap (+3) (1,2)
  (1,5)

  ·∾ fmap (+3) (1,2,3)
  <interactive>:10:1: error:
      • Non type-variable argument in the constraint: Functor ((,,) a b1)
        (Use FlexibleContexts to permit this)
      • When checking the inferred type
          it :: forall a b1 b2.
                (Functor ((,,) a b1), Num b2, Num a, Num b1) =>
                (a, b1, b2)

Personally I find this enough of a foot-gun to avoid using tuples.

Here are some useful functions for tuples, which come from ``Data.Tuple``::


  ·∾ :type fst
  fst :: (a, b) -> a
  ·∾ fst (1,2)
  1

  ·∾ :type snd
  snd :: (a, b) -> b
  ·∾ snd (1,2)
  2

  ·∾ :info curry
  curry :: ((a, b) -> c) -> a -> b -> c   -- Defined in ‘Data.Tuple’
  ·∾ curry fst 1 2
  1

  ·∾ :type uncurry
  uncurry :: (a -> b -> c) -> (a, b) -> c
  ·∾ uncurry (+) (1,2)
  3

  ·∾ import Data.Tuple
  ·∾ :type swap
  swap :: (a, b) -> (b, a)
  ·∾ swap (1,2)
  (2,1)

When should you use tuples over records, or ``Map``'s (dictionaries), or some
other data structure?


4.8 Lists
---------
Lists in Haskell are similar to singly-linked lists, but they can be of infinite
length. All elements of the list must be of the same type.


4.9 Chapter Exercises
---------------------
For these exercises, you'll need these definitions in scope at the repl::

  awesome = ["Papuchon", "curry", ":)"]
  also = ["Quake", "The Simons"]
  allAwesome = [awesome, also]

.. include:: exercises/4.9.1_-_untitled.rst

.. include:: exercises/4.9.2_-_correcting_syntax.rst

.. include:: exercises/4.9.3_-_match_the_function_names_to_their_types.rst
