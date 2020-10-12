************************
 Chapter 6: Typeclasses
************************


6.1 Typeclasses
---------------
In this chapter we will

* examine the typeclasses ``Eq``, ``Ord``, ``Enum`` and ``Show``;
* learn about type-defaulting typeclasses and typeclass inheritance;
* look at some common but often implicit functions that create side effects.


6.2 What are typeclasses?
-------------------------
**Type classes provide a way to introduce overloaded
operations.**

The main idea of a type class is to define a set of
overloaded names that work for many types. Names can be
either expressions, like ``minBound``, or functions, like
``(+)``. Overloaded names have different term-level
implementations for each type.

You define a type class in two parts, one part contains the
overloaded names, and the other part supplies type-specific
implementations of each name.

Class declarations
^^^^^^^^^^^^^^^^^^
The ``class`` declaration introduces a new type class.

This consists of a header with the type class name, a body
of overloaded expression names, and associated type
signatures for each name in the body. You can also include
fixity declarations here, for names that represent infix
functions.

Syntactically, class declarations have the general form:

  **class** *constraints* **=>** *Classname* *typevars*
  **where** **{** *class_method_names_and_type_signatures*
  **}**

The *constraints* part here can be used to specify a superclass.

Here's an example that defines a simplified version of the
type class ``Num``, which introduces the overloaded
functions ``(+)`` and ``negate``::

  class Num a where
    (+)    :: a -> a -> a
    negate :: a -> a

This declaration may be read "a type ``a`` is an instance of
the class ``Num`` if there are class methods ``(+)`` and
``negate``, of the given types, defined on it."

Instance declarations
^^^^^^^^^^^^^^^^^^^^^
An ``instance`` declaration defines the term-level
implementation of each overloaded operation — called class
methods — for the specified type.

The general form of an instance declaration is:

  **instance** *constraints* **=>** *Classname* *Typename*
  *typevars* **where** **{** *class_method_definitions*
  **}**

Here we implement ``Num`` for the concrete types ``Int`` and
``Float``::

  instance Num Int where
    x + y       =  addInt x y
    negate x    =  negateInt x

  instance Num Float where
    x + y       =  addFloat x y
    negate x    =  negateFloat x

The first declaration above may be read "``Int`` is an
instance of the class ``Num`` as witnessed by these
definitions (i.e. class methods) for ``(+)`` and ``negate``."

Type membership
^^^^^^^^^^^^^^^
If a type implements a type class, it's considered a member
of that class. How can we tell what classes a type is a
member of?

One way to determine which classes a type belongs to is
by looking it up on `hoogle <https://hoogle.haskell.org/>`_,
which extracts documentation from the source code.

But what if you don't have access to hoogle or the source
code?

You can find membership information in the output of ``:info
TypeName``, like this::

  ·∾ :info Float
  data Float = GHC.Types.F# GHC.Prim.Float# -- Defined in ‘GHC.Types’
  instance Eq Float           -- Defined in ‘GHC.Classes’
  instance Ord Float          -- Defined in ‘GHC.Classes’
  instance Enum Float         -- Defined in ‘GHC.Float’
  instance Floating Float     -- Defined in ‘GHC.Float’
  instance Fractional Float   -- Defined in ‘GHC.Float’
  instance Num Float          -- Defined in ‘GHC.Float’
  instance Real Float         -- Defined in ‘GHC.Float’
  instance RealFloat Float    -- Defined in ‘GHC.Float’
  instance RealFrac Float     -- Defined in ‘GHC.Float’
  instance Show Float         -- Defined in ‘GHC.Float’
  instance Read Float         -- Defined in ‘GHC.Read’

Since ``Float`` has an instance defined in ``GHC.Float``, it
is said to be a member of the ``Num`` type class.

More precisely, a type is a member of a type class if it has
an ``instance`` declaration for it that defines the minimally
required class methods listed in the ``class`` declaration.

Minimally required methods
^^^^^^^^^^^^^^^^^^^^^^^^^^
Wait, there's something new here. What is a minimally
required class method? How can you tell which class methods
are required?

Look no further than ``:info TypeClassName``. (I'm showing
the actual output for ``Num`` here, not the example version
of ``Num`` that we used above.) ::

  ·∾ :info Num
  class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a
    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
          -- Defined in ‘GHC.Num’
  instance Num Word     -- Defined in ‘GHC.Num’
  instance Num Integer  -- Defined in ‘GHC.Num’
  instance Num Int      -- Defined in ‘GHC.Num’
  instance Num Float    -- Defined in ‘GHC.Float’
  instance Num Double   -- Defined in ‘GHC.Float’

Syntax for specifying minimally required class methods
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The ``{-# MINIMAL (+), (*), abs, signum, fromInteger,
(negate | (-)) #-}`` syntax here is an example of a
minimal pragma.

Pragmas are not a built-in language feature, but a
facility provided by GHC. Minimal pragmas were
introduced in GHC version 7.10, and are treated as a
comments by earlier versions. You can find the `docs on
minimal here <https://downloads.haskell.org/~ghc/latest/
docs/html/users_guide/glasgow_exts.html#minimal-pragma>`_.

Minimal pragmas include a boolean expression that determine
which class method definitions are required for membership.

Within these expressions the stroke ``|`` indicates a
logical OR, and the comma ``,`` represents logical AND.

So, for membership in ``Num``, either ``(-)`` or ``negate``
are required, but not both. Additionally ``(+)``, ``(*)``,
``abs``, ``signum``, and ``fromInteger`` are all required.

Default class methods
^^^^^^^^^^^^^^^^^^^^^
So, if some class methods are not required for membership,
then what happens if you leave them undefined?

Either they won't work (because they don't exist), or more
interestingly, there may be a default implementation of the
method provided by the type class.

These default methods are usually created by composing the
minimally required methods together in some way. (Though
there are other possibilities.)

How do you supply your own default methods for a type class?

You include their term-level definition directly in the
class declaration, like so::

  class (Units a) => PID a where
    extendedGCD :: a -> a -> (a,a,a)
    extendedGCD = extendedEuclid divMod
    gcd :: a -> a -> a
    gcd x y = let (g,_,_) = extendedGCD x y in g

Derivable type classes
^^^^^^^^^^^^^^^^^^^^^^
What's better than not having to write class methods? Not
having to write entire instance declarations!

Many of the default classes provide generic instances that
can be derived automatically.

To do this you can add a ``deriving`` clause after a
``data`` declaration which will generate the instances.

For example, here's a custom list type::

  data List a = Cons a (List a) | Nil deriving (Eq, Ord, Show)

Haskell has just written the instances for ``Eq``, ``Ord``,
and ``Show`` for you. But how do they work?

What ``Eq`` and ``Show`` does here is pretty obvious.
``Show`` turns any data constructor into a string, and the
methods within ``Eq`` will compare the names of the data
constructors.

How about ``Ord``? There are a number of ways we could
determine how things are ordered. The default implementation
takes a simple approach.

When using a derived ``Ord`` instance, data constructors are
considered to have a higher ordinal value the further right
they are in the data declaration. So in this case ``Nil`` is
ordered after ``Cons``. Given our ``List`` definition above,
for example::

  ·∾ (Cons 3 (Nil)) < Nil
  True

Deriving does not work universally, however. So, how can you
determine which type classes can be derived, and which
cannot?

Here's what ``#haskell`` had to say::

  <justsomeguy> How can I determine which type classes can be derived,
                and which cannot? Do I just have to know, or can I
                query them somehow?

  <dolio>       justsomeguy: They're listed in the Haskell
                report, and GHC has some extras turned on by extensions.

  <dsal>        justsomeguy: I often just assume I can't derive
                classes and write them, but I've found GHC's been able
                to do some pretty fantastic deriving at times. There's
                lots of magic when you're ready for it.

  <dsal>        Is there a particular class you want derived?

  <justsomeguy> dsal: I can't think of a particular example
                right now; I was just reading about type classes in the
                book I'm learning from, and it piqued my interest.

  <dsal>        justsomeguy: Very often, there's only one way to do
                it, so it makes sense that it can be derived.

The language report says:

  Derived instances are possible only for classes known to
  the compiler: those defined in either the Prelude or a
  standard library.

  . . .

  The only classes in the Prelude for which derived instances
  are allowed are ``Eq``, ``Ord``, ``Enum``, ``Bounded``,
  ``Show``, and ``Read``

You will get a static error if you try to derive a type
class that doesn't support it. So I guess another way to
figure it out is just to try to derive and see if you get an
error.

Common type classes
^^^^^^^^^^^^^^^^^^^
Here are a few type classes you should know about. You'll
definitely encounter these, as they're pretty common used.

``Eq``

  Things that can be compared for equality. Can be derived
  -- data constructor names are compared.

``Ord``

  Things that can be ordered. Can be derived. It inherits
  from ``Eq``. This gives you the standard relational operators.

``Enum``

  Things that can be enumerated.

``Show``

  Things that can be rendered into strings. Can be derived.

``Read``

  This type class parses things into strings. Don't use it.

``Bounded``

  Introduces the methods ``minBound`` and ``maxBound``,
  which define the minimal and maximal elements of the type.
  Can be derived -- the first and last data constructors are
  used as bounds. For derivation, every constructor must be
  nullary or the type must have only one constructor.


6.3 Back to Bool
----------------
As you've just read, type classes have a hierarchy of sorts.

All ``Fractional`` numbers implement ``Num``. All members of
``Ord`` must be members of ``Eq``. All members of ``Enum``
must be members of ``Ord``.

::

  Num --> Fractional

  Eq --> Ord --> Enum


6.4 Eq
------
Tests for equality are implemented with a type class
called ``Eq``.

Some languages bake equality into every object in the
language. Since some datatypes don't have a sensible notion
of equality, Haskell doesn't do this.

(Which languages do that? What are some examples of
datatypes that don't have a sensible notion of equality?)

``Eq`` provides these methods::

  ·∾ :info Eq
  class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    {-# MINIMAL (==) | (/=) #-}
          -- Defined in ‘GHC.Classes’
  ...

...over many types. Run ``:info Eq`` yourself to see the full
output.


6.5 Writing type class instances
--------------------------------
Here is an very simple example of an ``Eq`` instance for a
custom datatype::

  data Trivial = Trivial'

  instance Eq Trivial where
    Trivial' == Trivial' = True

There is a single quote at the end of the data constructor
to disambiguate it from the type constructor.

A more interesting example::

  data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  data Date = Date DayOfWeek Int

  instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _   _   = False -- <-- This catch-all line is important!

  instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
         weekday == weekday' && dayOfMonth == dayOfMonth'

Note that the constructors won't print in GHCi unless you
define an instance of ``Show`` for them.

6.5.2 Partial functions
^^^^^^^^^^^^^^^^^^^^^^^
A partial function is a function which doesn't terminate
and yield a value for all given inputs. Conversely a total
function terminates and is always defined for all inputs.

We should take care to avoid partial functions, since they
can blow up at runtime. But it seems like it would be easy
to overlook an input. So then, how can we ensure all our
functions are total?

One way is to match unconditionally and then write some
logic to deal with those inputs safely. (By, say, returning
an identity value for that type.) Otherwise, you may want to
use a wrapper type like ``Maybe`` to indicate the possibility
of failure explicitly. Yet another possibility is to use a
type with a smaller cardinality, and define cases for all
possible inputs.

GHC flags can help you realize when functions are partial.
If we turn on ``-Wall``, we'll get an error message if a
function has cases left undefined. The error will even tell
you which inputs your function needs cases for.

Certain historical parts of ``Prelude`` are full of partial
functions. What about those?

Honestly that seems hard. `Diehl has more to say on the
matter <http://dev.stephendiehl.com/hask/#partial-functions>`_.

To me it seems that Haskell has bad defaults in this area.
I don't think I'll have the wherewithal to avoid them. Using
an alternative prelude kind of sucks, too. Hopefully there
is some tooling or a language extension to deal with this.

6.5.3 Sometimes we need to ask for more
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Sometimes, in order to write our operations, we may need to
use functions from other type classes. To make them visible,
you can add a constraint to the instance declaration, like
so::

  --      constraint
  --       vvvvvvv
  instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'
  --                                  ^^^^^^^
  --                        needed for this comparison

.. include:: exercises/6.5.4_-_eq_instances.rst


6.6 Num
-------
``Num`` provides ``(+)``, ``(*)``, ``(-)``, ``negate``,
``abs``, ``signum`` and ``fromInteger``. You can read the
`docs on Num here <https://hackage.haskell.org/package/
base-4.14.0.0/docs/Prelude.html#t:Num>`_.

``abs``: return the absolute value.

``signum``: for positive numbers return 1, for negative
numbers return -1, for zero return 0.

6.6.1 Integral
^^^^^^^^^^^^^^
Integral provides ``quot``, ``rem``, ``div``, ``mod``,
``quotRem``, ``divMod``, ``toInteger``. `Docs on Integral
are here <https://hackage.haskell.org/package/base-4.14.0.0/
docs/Prelude.html#t:Integral>`_

Descendant type classes can't override methods of their
ancestor type classes. Type classes respect their ancestors.

.. include:: exercises/6.6.2_-_tuple_experiment.rst

6.6.3 Fractional
^^^^^^^^^^^^^^^^
Fractional numbers, supporting real division.
``Fractional`` provides ``(/)``, ``recip``, and
``fromRational``. `Docs for Fractional are here
<https://hackage.haskell.org/package/base-4.14.0.0/
docs/Prelude.html#t:Fractional>`_.

Here's an example of how to use ``recip``::

  ·∾ recip 3.4
  0.29411764705882354

  ·∾ recip (3 / 4 :: Rational)
  4 % 3

  ·∾ import Data.Ratio (%)
  ·∾ recip (3 % 4)
  4 % 3


6.7 Type-defaulting type classes?
---------------------------------
In some cases, there may be no clear concrete type for a
constrained polymorphic type variable to resolve to. There
may be multiple types that satisfy the class constraints.
This type variable is said to be of an ambiguous type.

To prevent this situation, some type classes provide a
default type to resolve expressions to.

In the expression ``1 / 2``, there are multiple types that
could satisfy the ``Fractional`` class constraint that
``(/)`` creates.

But the result is of type ``Double``, because somewhere in
the source code for Prelude the ``default Fractional Double``
default declaration is provided.

You can use a different concrete type that has an instance
of ``Fractional`` by providing a type annotation::

  ·∾ 1 / 2
  0.5

  ·∾ 1 / 2 :: Float
  0.5

  ·∾ 1 / 2 :: Double
  0.5

  ·∾ 1 / 2 :: Rational
  1 % 2

Here are some good defaults to be aware of::

  default   Num          Integer
  default   Real         Integer
  default   Enum         Integer
  default   Integral     Integer
  default   Fractional   Double
  default   RealFrac     Double
  default   Floating     Double
  default   RealFloat    Double

Types can be made more specific, but not more general or
polymorphic.


6.8 Ord
-------
``Ord`` provides ``compare``, ``(<)``, ``(>=)``, ``(>)``,
``(<=)``, ``max``, and ``min``. `Docs for Ord <https://
hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#t:Ord>`_.

Compare was a new one to me::

  ·∾ compare 3 3
  EQ

  ·∾ compare 3 4
  LT

  ·∾ compare 3 2
  GT

Any time we ask GHCi to print a return value in our
terminal, we are indirectly invoking ``print``.


6.8.1 Ord instances
^^^^^^^^^^^^^^^^^^^
A few things to keep in mind about writing ``Ord``
instances; It's wise to ensure that your ``Ord`` instances
agree with your ``Eq`` instances. Also you want to define a
sensible total order. (wtf?) You ensure this in part by
covering all cases and not writing partial instances.

.. include:: exercises/6.8.3_-_will_they_work.rst


6.9 Enum
--------
``Enum`` provides ``succ``, ``pred``, ``toEnum``, ``fromEnum``,
``enumFrom``, ``enumFromThen``, ``enumFromTo``, ``enumFromThenTo``.
`Docs for Enum hereeeee mothafuckaaaa <https://hackage.haskell.org/
package/base-4.14.0.0/docs/Prelude.html#t:Enum>`_.

There's a ``toEnum``? ::

  ·∾ toEnum 8 :: Char
  '\b'

  ·∾ fromEnum 'c'
  99


6.10 Show
---------
Show is not a serialization format. `Docs for Show
<https://hackage.haskell.org/package/base-4.14.0.0/
docs/Prelude.html#t:Show>`_. A minimal implementation only
requires ``show`` or ``showsPrec``.

What the hell is ``showsPrec``? ::

  ·∾ :type showsPrec
  showsPrec :: Show a => Int -> a -> ShowS

  ·∾ :info ShowS
  type ShowS = String -> String

  ·∾ showsPrec 8 "this" "that"
  "\"this\"that"
