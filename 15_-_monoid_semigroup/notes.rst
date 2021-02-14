*******************************
 Chapter 15: Monoid, Semigroup
*******************************


15.1 Monoids and semigroups
---------------------------
One of the finer points of the Haskell community has been
its propensity for recognizing abstract pattern in code
which have well-defined, lawful representations in mathematics.

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
operate over. These operations must adhere to laws.

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
``(<>)``, which is inherited from the ``Semigroup``
typeclass.  Because of this, you don't have to define
``mappend`` if the type already has an instance of
``Semigroup``, it is automatically derived.

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

Newtype declarations are used rather than data declarations
because it constrains the constructor to a single argument,
signals intent, and can still be used to define unique
instance declarations against (like data declarations, but
unlike type aliases). Additionally, newtypes don't have any
overhead.


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

Here is a more concrete example usage of monoids that is
currently way beyond my comprehension:
https://apfelmus.nfshost.com/articles/monoid-fingertree.html

.. https://boris-marinov.github.io/category-theory-illustrated/03_monoid/


15.8 Laws
---------
Algebras are defined by their laws and are used principally
for their laws. Laws make up what algebras are. Laws
primarily exists to make it easier to reuse components by
making how they can be combined predictable. Who wouldn't
want that?

Here are the laws for ``Monoid``. Remember that ``(<>)`` is
a synonym for ``mappend``.

* **Right identity**: ``x <> mempty = x``
* **Left identity**: ``mempty <> x = x``
* **Associativity**: ``x <> (y <> z) = (x <> y) <> z`` (Semigroup law)
* **Concatenation**: ``mconcat = foldr (<>) mempty``


15.9 Different instances, same representation
---------------------------------------------
.. The name ``mappend`` may evoke imagery of hanging
.. something onto an existing structure[1], but
.. often ``mappend`` combines values in a completely
.. different way.

.. In the case of the ``Product`` newtype for numbers,
.. ``mappend`` is multiplication -- an operation that
.. does not preserve structure or order.

.. In my opinion, the class method should be named something
.. reflective of its general nature, such as "unite" or
.. "fuse" or "meld" or "merge" rather than ``mappend``.

.. [1]: (Fun fact: The "pend" in "append" comes from the Latin
.. pendere "to hang, cause to hang". An early, now obsolete,
.. meaning of "append" used in the 1640s was "to hang on,
.. attach as a pendant".)
A few types support multiple monoidal operations, which
are implemented as instances on a newtype named after
the operation.

``All`` and ``Any`` are the newtypes for ``Bool``'s
monoids::

  ·∾ import Data.Monoid

  -- newtype All = All { getAll :: Bool }
  ·∾ All True <> All True
  All {getAll = True}
  ·∾ All True <> All False
  All {getAll = False}

  -- newtype Any = Any {getAny :: Bool}
  ·∾ Any True <> Any False
  Any {getAny = True}
  ·∾ Any False <> Any False
  Any {getAny = False}

``First`` and ``Last`` are from ``Data.Monoid`` are
newtypes for ``Maybe`` values.  There are replacements
in ``Data.Semigroup`` for both ``First`` and ``Last``,
so their use is discouraged by the documentation.

``First`` returns the leftmost non-nothing value::

  -- newtype First a = First {getFirst :: Maybe a}
  ·∾  getFirst $ First (Just "hello") <> First Nothing <> First (Just "world")
  Just "hello"
  ·∾  getFirst $ First (Just 1) <> First (Just 2)
  Just 1

``Last`` returns the rightmost non-nothing value::

  ·∾  getLast (Last (Just "hello") <> Last Nothing <> Last (Just "world"))
  Just "world"


15.10 Reusing algebras by asking for algebras
---------------------------------------------
We can leverage existing types with instances of Monoid
to write composite types. This has the advantage of
making ``mappend`` easier to write, since the wrapped
component types already have well defined behaviour for
how to combine them using ``mappend``.

.. include:: exercises/15.10.1_-_optional_monoid.rst

