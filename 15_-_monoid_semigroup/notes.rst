*******************************
 Chapter 15: Monoid, Semigroup
*******************************


15.1 Monoids and semigroups
---------------------------
One of the finer points of the Haskell community has been
its propensity for recognizing abstract pattern in code
which have well-defined, lawful representations in
mathematics.

A word frequently used to describe these abstractions is
*algebra*, by which we mean one or more operations and the
set they operate over.

Over the next few chapters, we're going to be looking at
some of these.

This chapter will include:

* Algebras!
* Laws!
* Monoids!
* Semigroups!


15.2 What we talk about when we talk about algebras
---------------------------------------------------
An algebra refers to some operations and the set they
operate over.

.. What does "operate over" mean?

.. https://en.wikipedia.org/wiki/Closure_(mathematics)

.. In mathematics, a set is **closed* under an opereration
   if performing that operation on members of the set always
   produces a member of that seflsame set.

   For example, the positive integers are closed under
   addition, but not under subtraction. (1-2 is not a
   positive integer even though both 1 and 2 are positive
   integers.)

   A set is closed under an operation if the operation
   returns a member of the set when evaluated on members
   of the set.

   Sometimes the requirement that the operation be valued
   in a set is explicitly stated, in which case it is know
   as the *axiom of closure*.

   For example, on may define a group as a set with a binary
   product operator obeying several axioms, including an axiom
   that the product of any two elements of the group is again
   a element.

.. justsomeguy   Sometime I see the phrase "algebra" used to describe typeclasses,
..               and I'm a little confused by it. Does an algebra mean a single
..               operation and the set it operates on, or a collection of
..               operations and the set they operate on? Also, do algebras need to
..               conform to laws (or properties), or is that not a requirement to
..               call something an algebra?
..           *   justsomeguy tried reading about it on wikipedia but the math jargon only confused him more.
..         pjb   justsomeguy: https://en.wikipedia.org/wiki/Algebra_(disambiguation)#Mathematical_structures
..         pjb   justsomeguy: the number of operation will depend on the type of
..               algebra on you type of values.
..         pjb   justsomeguy: of course, you're not limitd to the structure that
..               have been defined so far by mathematicians. Basically any set of
..               operation on your types can be defined as AN algebra…

In Haskell, these algebras can be implemented with
typeclasses; the typeclasses define the operations.
The set is the type the operations are for. Instance
declarations define how each operation will perform
for a given type or set.


15.3 Monoid
-----------
**A monoid is a binary associative operation with an identity.**

Binary

  Takes two arguments.

Associative

  An operator is considered *mathematically associative*
  if, given some expression with multiple contiguous
  occurrences of that operator, the order that they're
  performed in won't change the result.

  Commutativity is a different concept than associativity.
  Commutativity is concerned with whether rearranging
  operands will change the result.

  Associativity is where evaluating operations in a
  different order won't change the result.

  Commutativity is where reordering the position of
  operands won't change the result.

  Here is an example of an expression where the operator
  is associative but not commutative::

    -- associative
    ·∾ ("Hey " ++ "you ") ++ "guys!" == "Hey " ++ ("you " ++ "guys!")
    True

    -- but not commutative
    ·∾ "Hey " ++ "you " ++ "guys!" == "guys!" ++ "you " ++ "Hey "
    False

  Here is an operation that is commutative but not
  associative::

    ·∾ absSub x y = abs (x - y)

    ·∾ a = 1
    ·∾ b = 2
    ·∾ c = 3

    -- commutative
    ·∾ a `absSub` b  == b `absSub` a
    True

    -- but not associative
    ·∾ (a `absSub` b) `absSub` c  ==  a `absSub` (b `absSub` c)
    False

Operation

  A function, usually infix.

Identity

  An "empty" value that when combined with any other value
  produces that other value.

  One example of this is the number :math:`0` for addition,
  since :math:`0+x` is always :math:`x`, and :math:`x+(-x) = 0`.

  For multiplication, the identity value is :math:`1`,
  since :math:`1*x` is always :math:`x`, and :math:`x*(x/x) = x*1`.

  When mixing colors in Photoshop, clear is the identity
  color since mixing any color with clear results in that
  color, and mixing any color with the exact opposite of
  that color (a color with opposite RGB and opacity values)
  results in clear.

Monoid is the typeclass that generalizes these laws across
types.


15.4 How Monoid is defined in Haskell
-------------------------------------
The typeclass ``Monoid`` is defined::

  class Semigroup a => Monoid a where
    mempty  ::   a
    mappend ::   a  ->  a  ->  a
    mconcat ::  [a] ->  a
    {-# MINIMAL mempty #-}

The ``mconcat`` class method has a default implementation
of::

  mconcat = foldr mappend mempty

The ``mappend`` class method has an infix synonym, spelled
``(<>)``, which is inherited from the ``Semigroup`` typeclass.

For your perusal, `here is a link <https://hackage.haskell.org/
package/base-4.14.1.0/docs/src/GHC.Base.html#Monoid>`_ to the
source code for ``Monoid`` in the ``base`` module. (It is also
defined in ``Data.Monoid``.)


15.6 What Integer doesn't have a Monoid
---------------------------------------
Some types can be viewed as a monoid in more than one way.
For example, both addition and multiplication are monoids
for numbers. In these cases we often define newtypes to
wrap those values and make them instances of ``Monoid``,
instead.

For numeric types, the newtypes ``Sum`` and ``Product``
are defined in ``Data.Monoid``.

Here's an example of their use::

  ·∾ import Data.Monoid

  ·∾ mappend 1 2
  <interactive>:6:1: error:
      • Ambiguous type variable ‘a0’ arising from a use of ‘print’
        prevents the constraint ‘(Show a0)’ from being solved.
        Probable fix: use a type annotation to specify what ‘a0’ should be.
        These potential instances exist:
          instance (Show a, Show b) => Show (Either a b)
            -- Defined in ‘Data.Either’
          instance forall k (f :: k -> *) (a :: k).
                   Show (f a) =>
                   Show (Ap f a)
            -- Defined in ‘Data.Monoid’
          instance Show a => Show (First a) -- Defined in ‘Data.Monoid’
          ...plus 32 others
          ...plus 63 instances involving out-of-scope types
          (use -fprint-potential-instances to see them all)
      • In a stmt of an interactive GHCi command: print it

  ·∾ mappend (Sum 1) (Sum 5)
  Sum {getSum = 6}

  ·∾ mappend (Product 5) (Product 5)
  Product {getProduct = 25}

  ·∾ mappend (Sum 4.5) (Sum 3.4)
  Sum {getSum = 7.9}


15.7 Why bother?
----------------
Having a name for this pattern of composition allows us
to communicate about it, and look up existing solutions
that use it. Once we know our problem is *monoidal*, we
can refer to research papers outside of programming to
find new approaches to solving our problem.

Monoids are also useful because they provide a common
interface. This way we don't have to remember a bunch of
operations for combining things that are unique to each
type.
