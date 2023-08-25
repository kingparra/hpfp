*********************************
 Chapter 11: Algebraic Datatypes
*********************************
.. 385

.. 386

11.1 Algebraic datatypes
------------------------
This chapter explores datatypes more fully.
The ``data`` and ``type`` and ``newtype`` declarations will be compared.
There will be an explanation of record types.
Finally, the calculation of possible term-level values
from a type declaration will be discussed.


11.2 Data declarations review
-----------------------------
The way you structure your datatypes
impacts how your interface works.

Let's review the anatomy of data declarations.
First, we'll examine a type with one data constructor.

::

  --    type
  -- constructor
  --     v
  data Trivial = Trivial
  --                ^
  --              data
  --           constructor

In this example, the data constructor and type constructor
have the same name, but are different entities. One is a
term-level value, and the other is the name of a type.

Now let's look at a sum type.
The following declaration can be read as "a value of type
``Bool`` can be *either* ``False`` or ``True``."
The pipe character is read as "or".

::

  --               xor
  --                v
  data Bool = False | True

Here is a composite type that takes a *type variable*.

::

  --    type variables
  --  (must be same type)
  --   vvv        v   vvv
  data [a] = [] | a : [a]

To construct a value using cons (``:``), you must
provide it two arguments, that must be of types ``a``
and ``[a]`` (list of a) respectively.


11.3 Data and type constructors
-------------------------------
There are two types of constructors:
data constructors and type constructors.
Data constructors exist only on the term-level,
and type constructors exist only at the type level.

Data constructors may be nullary -- in which case they're
constant values -- or the may have any number of parameters.

Not all type arguments to a type constructor need to
be used by the types data constructors. These arguments
are said to be *phantom*, since they don't have a value-level
witness.

::

  --    phantom type var
  --            v
  data PhantomA a = NoAHere | OrHere | ThereJustIsntAnA


11.4 Type constructors and kinds
--------------------------------
Kinds are types of types, or types one level up.
In a kind signature, the symbol ``*`` represents a type.

::

  ·∾ :info Bool
  data Bool = False | True -- concrete type; no type args
  ·∾ :kind Bool
  Bool :: *

  ·∾ :info []
  data [] a = [] | a : [a] -- one type arg
  ·∾ :kind []
  [] :: * -> *

  ·∾ :info Either
  data Either a b = Left a | Right b -- two type args
  ·∾ :kind Either
  Either :: * -> * -> *


11.5 Data constructors and values
---------------------------------
This section is a big example of what was already covered in 11.{2,3,4}.
Nothing new but there is an exercise.


11.6 What's a type and what's data?
-----------------------------------
Types are static and resolve at compile time.
Information about types does not persist through to runtime.


11.7 Data constructor arities
-----------------------------
This section is about arity. Nothing new here.


11.8 What makes these datatypes algebraic?
------------------------------------------
The cardinality of a datatype is the number of term-level values
you can create from it.

Given a datatype declaration, it's possible to calculate the
cardinality using two rules: sum and product.

This is the algebra of algebraic datatypes.

Here's a quick summary of the rules, from `The algebra (and calculus!) of algebraic data types by Joel Burget
<https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types>`_.

+--------------------------------------------+------------------+---------------------------------------+
|  Haskell                                   |  Math            | Notes                                 |
+============================================+==================+=======================================+
|  ``data Void``                             |  0               | Needs ``-XEmptyDataDecls``.           |
+--------------------------------------------+------------------+---------------------------------------+
|  ``data Unit = Unit``                      |  1               | Type with just one term.              |
+--------------------------------------------+------------------+---------------------------------------+
|  ``data Bool = True | False``              |  1 + 1           |                                       |
+--------------------------------------------+------------------+---------------------------------------+
|  ``data Maybe a = Just a | Nothing``       |  a + 1           | Read "+" as "Either".                 |
+--------------------------------------------+------------------+---------------------------------------+
|  ``data Either a b = Left a | Right b``    |  a + b           |                                       |
+--------------------------------------------+------------------+---------------------------------------+
|  ``data (a, b) = (a, b)``                  |  a × b           |  Read "×" as "And".                   |
+--------------------------------------------+------------------+---------------------------------------+
|  ``a -> b``                                |  b^a             |  b to the power of a                  |
+--------------------------------------------+------------------+---------------------------------------+

There is an exercise here, but I don't feel like doing it.


11.9 ``newtype``
----------------
Type synonyms created with ``type`` do not create new types,
they just rename existing types, and don't prevent confusing
different values.

The ``newtype`` construct exists to make semantically separate
types at compile time based on the same underlying type.

A newtype declaration looks like this:

::

  newtype Velocity = Velocity Double

There can only be exactly one argument to the data constructor.

You can define type class instances for a newtype that differs
from the instances for its underlying type. You can't do that for
type synonyms.

If we want to re-used user defined typeclasses for the type we're
wrapping, we can turn on ``-XGeneralizedNewtypeDeriving``.

Here's what that looks like:

::

  {-# LANGUAGE GeneralizedNewtypeDeriving #-}
  class TooMany a where
    tooMany :: a -> Bool

  instance TooMany Int where
    tooMany n = n > 42

  newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

There is an exercise here on defining typeclasses with
and without that language extension.


11.10 Sum types
---------------
Sum types represent alternative values.


11.11 Product types
-------------------
Product types represent compound values.
Record syntax looks like this:

::

  data Person =
    Person { name :: String, age :: Int }
    deriving (Eq, Show)

Constructing a value looks like this:

::

  Person { name="Chris", age=35 }

If you leave out a field when constructing a record, you can hit
bottom.

::

  data Programmer =
    Programmer { os :: OperatingSystem, lang :: ProgLang }

  >>> Programmer { os = Mac, lang = Haskell } -- OK

  >>> Programmer { os = Linux } -- Will throw an exception!

So don't try to do partial application when creating records.
Define all the fields at once or not at all.


11.12 Normal form
-----------------
I really don't care about this.


11.3 Constructing and deconstructing values
-------------------------------------------
If you can construct a value, you can deconstruct it.
Nothing new here.


11.14 Function type is exponential
----------------------------------
Nothing new here.


11.15 Higher-kinded datatypes
-----------------------------
I think I hate this chapter.


11.16 Lists are polymorphic
---------------------------
Yes, I know that.


11.17 Binary tree
----------------
.. include:: exercises/BinaryTree.hs
   :code:
