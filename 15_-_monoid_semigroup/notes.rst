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
An algebra refers to some operations and the set they operate
over. These operations may have laws associated with them.

.. topic:: What does "operate over" mean?

   https://en.wikipedia.org/wiki/Closure_(mathematics)

   In mathematics, a set is *closed* under an opereration
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

  An "empty" value that when combined with any other
  value produces that other value.

  An identity is a value with a special relationship
  with an operation: it turns the operation into the
  identity function. There are no identities without
  operations.

  One example of this is the number :math:`0` for
  addition, since :math:`0+x` is always :math:`x`, and
  :math:`x+(-x) = 0`.

  For multiplication, the identity value is :math:`1`,
  since :math:`1*x` is always :math:`x`, and
  :math:`x*(x/x) = x*1`.

  When mixing colors in Photoshop, clear is the identity
  color since mixing any color with clear results in that
  color, and mixing any color with the exact opposite of
  that color (a color with opposite RGB and opacity
  values) results in clear.

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
Some types can be viewed as a monoid in more than one
way. For example, both addition and multiplication are
monoids for numbers. In these cases we often define
newtypes to wrap those values and make them instances
of ``Monoid``, instead.

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

.. -ity "condition or quality of being ____"
.. plectere (latin) "to weave, braid, twine, entwine", from PIE plek-to- "to plait"
.. plait - A braid of material (such as hair or straw).

15.10.4 The problem of orphan instances
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
An orphan instance is when an instance is defined for a
datatype and typeclass, but not in the same module as
either the typeclass or datatype declaration.

This can lead to situations where there are multiple
conflicting instances for the typeclass/type pair. Even
if you haven't imported conflicting instances, if they
exist at all it is no longer safe to assume you know what
class methods will do anymore.

Avoid this; It's bad juju, and GHC will warn you when it
happens. If you don't own the typeclass or the datatype,
and you need an instance, create it against a newtype of
the type, instead.

There are a few solutions for addressing orphan instances:

1. You defined the type but not the typeclass? Put the
   instance in the same module as the type so that the
   type cannot be imported without its instances.

2. You defined the typeclass but not the type? Put the
   instance in the same module as the typeclass definition
   so that the typeclass cannot be imported without its
   instances.

3. Neither the type nor the typeclass are yours? Define
   your own newtype wrapping the original type and now
   you've got a type that "belongs" to you for which you
   can rightly define typeclass instances. There are means
   of making this less annoying which we’ll discuss later.


15.11 Madness
-------------
Using a lightly edited example from the Wikipedia article
on Mad Libs:

| "*exclamation*! he said *adverb* as he
| jumped into his car *noun* and drove
| off with his *adjective* wife"

We can make this into a function, like the following:

.. include:: figures/15.11/Madness.hs
   :code:


15.12 Better living through QuickCheck
--------------------------------------
::

  import Data.Monoid
  import Test.QuickCheck

  monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
  monoidAssoc a b c =
    a <> (b <> c) == (a <> b) <> c

  main = quickCheck
    (monoidAssoc :: String -> String -> String -> Bool)


15.13 Semigroup
---------------
A semigroup is a binary associative operation. Unlike
monoid, it does not require an identity value.

::

  class Semigroup a where
    (<>) :: a -> a -> a

...and we're left with one law, associativity::

  (a <> b) <> c == a <> (b <> c)

Although it's not mentioned in the book, there are a
few more class methods::

  ·∾ :info Semigroup
  type Semigroup :: * -> Constraint

  class Semigroup a where
    (<>)    :: a -> a -> a
    sconcat :: NonEmpty a -> a
    stimes  :: Integral b => b -> a -> a
    {-# MINIMAL (<>) #-}

  ·∾ :doc sconcat
  Reduce a non-empty list with '<>' The default
  definition should be sufficient, but this can be
  overridden for efficiency.

  >>> import Data.List.NonEmpty
  >>> sconcat $ "Hello" :| [" ", "Haskell", "!"]
  "Hello Haskell!"

  ·∾ :doc stimes
  Repeat a value @n@ times.

  Given that this works on a 'Semigroup' it is allowed
  to fail if you request 0 or fewer repetitions, and
  the default definition will do so.

  By making this a member of the class, idempotent
  semigroups and monoids can upgrade this to execute
  in \(\mathcal{O}(1)\) by picking @stimes =
  'Data.Semigroup.stimesIdempotent'@ or @stimes =
  'stimesIdempotentMonoid'@ respectively.

  >>> stimes 4 [1]
  [1,1,1,1]

15.13.2 NonEmpty, a useful datatype
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
One useful datatype that can't have a ``Monoid`` instance
but does have a ``Semigroup`` instance is the ``NonEmpty``
list type. It is a list datatype that can never be an empty
list.

::

  data NonEmpty a  =  a :| [a]

There is no empty list to serve as an identity for any
operation over a ``NonEmpty`` list, yet there is still a
binary associative operation: two ``NonEmpty`` lists can
still be concatenated.

::

  ·∾ (1 :| [2,3]) <> (4 :| [5,6])
  1 :| [2,3,4,5,6]
  ·∾ :info (<>)
  class Semigroup a where
    (<>) :: a -> a -> a
    ...
          -- Defined in ‘GHC.Base’
  infixr 6 <>
  ·∾ :info (:|)
  data NonEmpty a = a :| [a]      -- Defined in ‘GHC.Base’
  infixr 5 :|

You can read more about it `here, on hoogle <https://
hackage.haskell.org/package/base-4.14.1.0/docs/
Data-List-NonEmpty.html>`_.


15.14 Strength can be weakness
------------------------------
The *strength* of an algebra usually refers to the number
of operations it provides. Making an algebra stronger may
in turn require enforcing more laws, which means that fewer
types can have legal instances of it. Generally, it is
better to have multiple smaller algebras than one larger
one. Some types defy uniform generalization, but have
efficient machine-level representations.


15.15 Chapter exercises
-----------------------

.. include:: exercises/15.15.1_-_semigroup_exercises.rst
