*********************************
 Chapter 11: Algebraic datatypes
*********************************

.. This chapter roughly corresponds to Chapter 12 of Get
.. Programming with Haskell by Will Kurt and section 4.2.1 of the
.. 2010 Haskell Language Report. Stephen Diehls guide also has
.. a section, http://dev.stephendiehl.com/hask/#algebraic-datatypes
.. Also check out this nice article:
.. https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types


11.1 Algebraic datatypes
------------------------
This chapter introduces a few new ways to declare types, including record
types, type aliases using ``type``, and wrapper types using ``newtype``.
Along the way, examples of how to calculate the number of term-level
inhabitants (or cardinality) of a type are discussed.


11.2 Data declarations review
-----------------------------
::

  --    type         data
  -- constructor  constructor
  --      v            v
  data Trivial  =   Trivial'
  -- ^
  -- keyword
  --
  -- In this examples both constructors are constants,
  -- since they take no arguments.

  -- Constructors may also take arguments.
  --
  --          type variable   term-level placholder
  --                |         for a value of type a
  --                v                 v
  data UnaryTypeCon a = UnaryValueCon a
  --
  -- There is no requirement to use type constructor
  -- parameters as arguments to your data constructors;
  -- They can be phantom (lacking a term-level witness),
  -- instead.

.. In any realistic program, you'll need to define your own data
.. types to capture the specific requirements of the problem you're
.. solving and the specific forms of data you’re working with. Not
.. only that, but there's a significant payoff to thinking carefully
.. about the design of data types: the more precisely types capture
.. the requirements of a problem, the more closely type errors will
.. mirror errors in reasoning. (Paraphrased from Type-Driven
.. Development with Idris by Edwin Brady)


11.3 Data and type constructors
-------------------------------
Type constructors are used only at the type level. Data constructors
construct values at the term level. Type and data constructors that take no
arguments (like Bool, and True/False) are constants.

Sometimes constructors take arguments. In those cases, it's like a function
in at least one sense -- it must be applied to become a concrete type or
value.

When writing a type it's possible to create parameters for the type
constructor, called type variables. These parameters are placeholders for
potential types that can be used to construct the overall composite type.

Type variable names may in turn be used by data constructors. However, you
aren't required to use them -- they may be *phantom*, lacking a value-level
witnesses.


11.4 Type constructors and kinds
--------------------------------
Types constructors themselves have kind signatures to help us reason about their
arity and possible number of inhabitants. These kinds are essentially a simple
type system one level up for our type constructors. There are three possible
components of a kind signature: ``*`` (pronounced "type"), ``->`` which denotes
type constructor application, and ``#`` a GHC extension that represents unboxed
types, like those used to create primitive types, found in ``GHC.Prim``.

::

  import Data.Ratio (Ratio)

  ·∾ :kind Ratio
  Ratio :: * -> *

  ·∾ :kind Ratio Int
  Ratio Int :: *

One interesting use of kind signatures is to limit the arity of an input
type constructor, used to create the ``f a`` and ``f b`` types in
``fmap``'s type signature::

  ·∾ :info fmap
  class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b

There is more nuance to kinds, but I don't have time to read about it right
now, so I'll just leave this link to the GHC wiki here.
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/type-type


11.5 Data constructors and values
---------------------------------
The behavior of constructors is such that if they don't take any arguments, they
behave like (type or value-level) constants. If they do take arguments, they act
like (type or value-level) functions that don't *do* anything except get applied.

.. include:: exercises/11.5.1_-_dog_types.rst


11.6 What's a type and what's data?
-----------------------------------
In Haskell types are *static*, meaning that they only have semantic meaning
during compile time, and not at runtime. (However, there are some facilities for
introspection during runtime, check out ``Data.Typeable`` and ``Type.Reflection``.)

::

  type constructors  -- compile time
         ...         -- phase separation
  data constructors  -- runtime

.. include:: exercises/11.6.1_-_vehicles.rst


11.7 Data constructor arities
-----------------------------
This section over-explains what arity is. Again. It also hints at what a product
type is, without revealing any new useful information. How frustrating.


11.8 What makes these datatypes algebraic?
------------------------------------------
Algebraic datatypes in Haskell are algebraic because we can describe the
patterns of argument structures using two basic operations: sum and product.
With this, we can calculate the cardinality of a type -- how many term-level
inhabitants it has. In some cases, using this information, we can determine how
may different possible implementation there are of a function for a given type
signature.


See `this blog post <https://codewords.recurse.com/issues/three/
algebra-and-calculus-of-algebraic-data-types>`_ for a more thorough
treatment of how to calculate cardinality than what this section
covers. In brief...

+--------------------------------------+---------------+
|  Type definition                     |  Cardinality  |
+======================================+===============+
|  data Void                           |       0       |
+--------------------------------------+---------------+
|  data Unit = Unit                    |       1       |
+--------------------------------------+---------------+
|  data Bool = True | False            |     1 + 1     |
+--------------------------------------+---------------+
|  data Maybe a = Just a | Nothing     |     a + 1     |
+--------------------------------------+---------------+
|  data Either a b = Left a | Right b  |     a + b     |
+--------------------------------------+---------------+
|  data (a,b) = (a,b)                  |     a × b     |
+--------------------------------------+---------------+
|  a -> b                              |     b ^ a     |
+--------------------------------------+---------------+

.. include:: exercises/11.8.1_-_cardinality.rst

.. include:: exercises/11.8.3_-_for_example.rst


11.9 ``newtype``
----------------
The ``newtype`` keyword is used to define a type that can only have a
single unary data constructor. It's often used to rename existing types.
Unlike type aliases with the ``type`` keyword, you can't use the underlying
type in place of its alias. ``newtype``'s are semantically different from
the wrapped type, even though they share the same underlying
representation.

::

  newtype Age = Age { unAge :: Int }

  newtype N = N Int

One key contrast between a newtype and a type alias is that you can define
type class instances for newtypes that differ from the instances for their
underlying type. You can't do that for type synonyms.

On the other hand, what about the case where we want to reuse the type class
instances of the type our newtype contains? For user-defined type classes, we
can use a language extension called ``GeneralizedNewtypeDerinving``. Here's a
simple example::

  {-# LANGUAGE GeneralizedNewtypeDeriving #-}
  class TooMany a where { tooMany :: a -> Bool }
  instance TooMany Int where { TooMany n = n > 42 }
  newtype Goats = Goats Int deriving (Eq, Show, TooMany)
  --                     the magic happens here ^^^^^^^
  -- The instance for (Goats Int) is derived from (TooMany Int)

``GeneralizedNewtypeDeriving`` is useful for cases when we want every type
class instance to be the same except for the one we want to change.

This section had a lot of interactive material, so I scripted it and made a
terminal recording. Annoyingly, the error messages are displayed with a fake
typing effect. Sorry about that. Check out the ``figures`` directory to read
the expect script directly.

.. raw:: html

   <script id="asciicast-Oth8hhFIAzqdLWNpzpwJfw3aV"
   src="https://asciinema.org/a/Oth8hhFIAzqdLWNpzpwJfw3aV.js"
   async></script>

A few things bothered me, so I asked about them on ``#haskell``::

  <justsomeguy> Does using the “type” keyword incur a runtime cost?
  <ski> justsomeguy : no
  <justsomeguy> (I'm confused as to why books keep on mentioning that “newtype”
  doesn't incur a runtime cost. Why would it? There wasn't any mention of cost
  when discussing type classes.)
  <ski> `type' synonyms are just abbreviations for more complicated type expressions
  <dminuoso> justsomeguy: data does incur a runtime cost, even if you write
  `data Foo = Foo Int`
  <ski> the point with `newtype' is that `data New = Mk Old' does incur a
  run-time cost, while `newtype New = Mk Old' doesn't
  <dminuoso> In case of `newtype F = F T`, the runtime representation of `F` is
  the runtime representation of `T`.

  <justsomeguy> Then, why didn't the language designers optimize “data” for that
  case, instead of inventing a new keyword?

  <dminuoso> justsomeguy: Because `data` allows its content to be lazy, for
  example. And data allows to have multiple fields.
  <ski> the difference between `newtype New = Mk Old' and `type New = Old' is
  that in the latter case, `New' and `Old' are synonyms, are the same type (also
  `New' can't be recursive, `Old' can't refer to / be defined in terms of
  `New'), while in the former case (just like with `data') `New' is a distinct
  type from `Old', and you have to explicitly use `Mk' (wrapping with a call to
  it, or unwrapping with pattern-matching), to convert between `New' and `Old'.
  (also, with `newtype', `New' can be recursive)

  <justsomeguy> dminuoso: Oh, true, “data” is more general. “type” communicates
  some intent that you want the constructor to be unary.
  <justsomeguy> (I guess that's kind of like asking "why have “const” when you
  can just not mutate variables?".)

  <justsomeguy> ski: Thanks, that's helpful. I wasn't sure if you could use the
  aliased type in place of the new name you defined with “type”, but it seems
  you can. ``newtype`` is different.
  <ski> justsomeguy : "Then, why didn't the language designers optimize “data”
  for that case, instead of inventing a new keyword?" -- replacing `data New =
  Mk Old' by `newtype New = Mk Old' can change program behaviour, so it can't be
  a mere optimization
  <ski> they could have specified that using `data' with a single data
  constructor (with a single component / field / argument / parameter) behaved
  exactly like `newtype' now works .. but then this case would not fit into the
  same pattern as how all the other cases work
  <ski> (the reason for the difference is that `data' constructors can be
  non-strict. if that wasn't the case, then there wouldn't be a difference in
  behaviour)
  <justsomeguy> Ahh
  <ski> @let data MyInt = MkMI Int deriving Show
  <lambdabot>  Defined.
  <ski> @let newtype MyInt' = MkMI' Int deriving Show
  <lambdabot>  Defined.
  <ski> > case undefined of MkMI _ -> ()
  <lambdabot>  *Exception: Prelude.undefined
  <ski> > case undefined of MkMI' _ -> ()
  <lambdabot>  ()
  <ski> wrapping or unwrapping a `newtype' data constructor is a no-op, does
  nothing at run-time. but matching on the `data' data constructor forces the
  value
  <ski> @let data MyInt'' = MkMI'' !Int deriving Show  -- this is a `data' type
  with a strict data constructor (field). forcing a call to it forces the
  field/component
  <lambdabot>  Defined.
  <ski> > case undefined of MkMI'' _ -> ()  -- matching on behaves the same, as
  with the non-strict `data' constructor
  <lambdabot>  *Exception: Prelude.undefined
  <ski> however, while the data constructor of the first (`data') type is
  non-strict, the data constructor of the `newtype' is also strict (meaning that
  if you call it with a bottom value, you'll get a bottom result. so calling it
  with `undefined', and forcing the result, forces that `undefined' argument)
  <ski> > let !_ = MkMI undefined in ()  -- the usual (non-strict) `data' data
  constructor is .. non-strict. forcing the result doesn't force the argument
  <lambdabot>  ()
  <ski> > let !_ = MkMI' undefined in ()  -- in the `newtype' case, the data
  constructor is strict, so forcing the result does force the argument. this is
  because the representation is the same, there is no actual wrapping. so
  forcing the result forces the argument, since they're effectively the same
  thing, at run-time
  <lambdabot>  *Exception: Prelude.undefined
  <lambdabot>  *Exception: Prelude.undefined
  <ski> > let !_ = MkMI'' undefined in ()  -- in the strict `data' case, the
  data constructor is also strict, so forcing the result does also force the
  argument. the representation is *not* the same, but the data constructor is
  defined to explicitly force its argument (to be "non-lazy"), unlike the usual
  (non-strict / "lazy") `data' case
  <lambdabot>  *Exception: Prelude.undefined
  <justsomeguy> That was very helpful; Thank you ski :^). (Also, it was a good
  intro to how to use strictness annotations, which I haven't seen before,
  except as eBNF.)
  <ski> attempt at summary : `newtype' data constructor is strict, like in the
  strict `data' case (with `!' annotation on component type), but unlike the
  usual (non-strict) `data' case, so forcing result does force the argument.
  however, *matching* on `newtype' data constructor (unlike strict `data' case)
  does not force argument, since the representation is the same, so that calling
  and matching on `newtype' data constructor is a no-op
  <ski> (this is somewhat subtle, yes, hence my attempt at highlighting the
  differences)
  <justsomeguy> The subject was way deeper than I expected!
  <sshine> justsomeguy, this is implied here.
  <justsomeguy> I guess I just described the entirety of Haskell, lol.

  <ski> one could have assumed that, a `newtype' data constructor being strict
  (like in the strict `data' case), means that matching on it, would force the
  argument. but that is *not* the case ! (due to calling and matching on
  `newtype' data constructor being no-op). that's the main subtle thing here,
  that can be easy to trip over / miss, i guess

  <dolio> It's not really that matching on it doesn't force the argument.
  Matching on it doesn't force the value of the newtype.
  <dolio> Your examples showed that.

  <ski> yes, that amounts to the same thing. of course, matching on `x' or `_' (as
  in `case undefined of x -> ()') doesn't force anything. but the subtlety here is
  that, matching on a `newtype' data constructor *also* doesn't force anything
  (unlike matching on other data constructors)

  <ski> > case undefined of x -> ()
  <lambdabot>  ()
  <dolio> Like, matching on strict data doesn't require forcing the argument,
  except that it forces the data value as well, which requires forcing the
  argument.
  * ski nods

.. include:: exercises/11.9.1_-_logic_goats.rst


11.10 Sum types
---------------

.. include:: exercises/11.10.1_-_pity_the_bool.rst


11.11 Product types
-------------------
A product type's cardinality is the product of the cardinalities of its
inhabitants. Any data constructor with two or more type arguments is a product.

+-------------------------------------------------+-------------------------+
|   Type definition                               |      Cardinality        |
+=================================================+=========================+
| ``data (a,b) = (a,b)``                          |  :math:`a × b`          |
+-------------------------------------------------+-------------------------+
| ``data TripleOrNot a b c = Triple a b c | Not`` |  :math:`a × b × c + 1`  |
+-------------------------------------------------+-------------------------+

11.11.1 Record syntax
^^^^^^^^^^^^^^^^^^^^^
**Records are data constructors that have named parameters.**
These parameters are called fields. Here's how a point may be
represented as a regular data constructor versus as a record.

::

  data Point = Point Int Int

  data PointRecord = PointRecord { x :: Int, y :: Int }

When you want to create a ``PointRecord`` value, you'd write it
like this::

  point = PointRecord { x = 8, y = 12 }

If you write something like::

  ·∾ point { y = 99 }
  Point {x = 8, y = 99}

GHCi responds with a new ``Point`` with and updated ``y`` field.
This is called a field update.

One interesting things about records in Haskell is how fields are
made available for retrieval. Rather than writing ``point.x``, an
accessor function named ``x`` is generated from the type
declaration in the global namespace, which you can then use to
access the field it's named after, like::

  ·∾ x point
  8
  ·∾ y point
  12

This lack of scoping has some drawbacks. For one thing, if you
have the same field name present in records *from different type
declarations*, the generated accessor functions may overwrite or
conflict with each other.

Consider::

  data Record = Record { a :: String }

  data RecordClash = RecordClash { a :: String }

Compiling this file results in::

  record.hs:2:34:
      Multiple declarations of `Main.a'
      Declared at: record.hs:1:24
                   record.hs:2:34

You may have thought that haskell would automatically overload
the function, and then dispatch the appropriate version of the
accessor function based on the type of its argument, but
surprisingly that's not what happens.

The simplest possible workaround is to prefix fields with the
name of the type they belong to. Here's an example of what I
mean::

  data Comment = Comment {
        commentId           :: CommentId
      , commentContent      :: Content
      , commentReviewId     :: ReviewId
      , commentSubmissionId :: SubmissionId
      , commentConferenceId :: ConferenceId
      , commentDate         :: ISODate
      , commentReviewerNumber :: Int
    } deriving (Show)

Another relatively simple solutions is to use the `DisambiguateRecordFields
<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/disambiguate_record_fields.html>`_
language extension. This allows the compiler to automatically choose
between identically-named record selectors based on type if the choice is
unambiguous.

Other solutions exist, too. Here is a wiki article with links to a ton of
them: https://gitlab.haskell.org/ghc/ghc/-/wikis/records. Unfortunately,
some of these are really complicated. I think this is because they're a
special case of an more general framework for retrieving sub-structures. --
Total overkill for just getting a record field, but maybe useful for other
problems.

11.12 Normal form
-----------------
All the existing algebraic rules for products and sums apply in the type system,
and that includes the distributive property. In this section, expressions that
have been distributed are considered to be "more evaluated" than those that have
not.

.. include:: figures/11.12/ItsAlgebraicDearWatson.hs
   :start-after: -- begin fig 2
   :end-before: -- end fig 2
   :code:


11.13 Constructing and deconstructing values
--------------------------------------------

11.13.4 Accidental bottoms from records
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
What happens if we construct a value using record syntax but forget a field? If
you assign it to something and don't use it, nothing. If you try to use it,
however, you'll get an exception! (Just like any other data constructor without
all of its parameters filled in.)

::

  ·∾ data Programmer = Programmer { lang :: String, os :: String, awesome :: Bool } deriving (Eq, Show)
  ·∾ x = Programmer { lang="agda" }
  <interactive>:23:5: warning: [-Wmissing-fields]
      • Fields of ‘Programmer’ not initialised: os, awesome
      • In the expression: Programmer {lang = "agda"}
        In an equation for ‘x’: x = Programmer {lang = "agda"}
  ·∾ x2 = x { os="linux" }
  ·∾ x2
  Programmer {lang = "agda", os = "linux", awesome = *** Exception:
  <interactive>:23:5-30: Missing field in record construction awesome
  ·∾ x3 = x2 {awesome=True}
  ·∾ x3
  Programmer {lang = "agda", os = "linux", awesome = True}
  ·∾
  ·∾ (1,)
  <interactive>:29:1: error: Illegal tuple section: use TupleSections

The book cautions against building up a record this way, though. "Either define
the whole record at once or not at all." I think this is because a partial
record is does not have the missing fields reflected in the type signature, like
a data constructor for a product type with unsaturated parameters would. If you
need to build up a value like this, use a product type, instead, so partial
application is reflected in the type signature.

::

  --- Unsaturated fields are not reflected in the type signature
  ·∾ :type x
  x :: Programmer
  ·∾ :type x2
  x2 :: Programmer
  ·∾ :type x3
  x3 :: Programmer

  --- But for this product type, they are
  ·∾ type Lang = String
  ·∾ type Os = String
  ·∾ type Awesome = Bool
  ·∾ data Programmer' = Programmer' Lang Os Awesome deriving (Eq, Show)
  ·∾ y = Programmer' "haskell"
  ·∾ :type y
  y :: Os -> Awesome -> Programmer'
  ·∾ y2 = y "linux"
  ·∾ :type y2
  y2 :: Awesome -> Programmer'
  ·∾ y3 = y2 True
  ·∾ :type y3
  y3 :: Programmer'


11.17 Binary Tree
-----------------

11.17.1 Inserting into trees
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
"Left lesser, right greater" is a common convention for arranging binary trees.

11.17.3 Write ``map`` for ``BinaryTree``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

11.17.4 Write ``foldr`` for ``BinaryTree``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


11.18 Chapter Exercises
-----------------------

.. include:: exercises/11.18.1_-_multiple_choice.rst
