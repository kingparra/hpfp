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
This chapter introduces a few new ways to declare types, including record types,
type aliases using ``type``, and wrapper types using ``newtype``. Along the way,
a few hints on how to calculate the number of term-level inhabitants (or
cardinality) of a type are discussed.


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
Type constructors are used only at the type level. Data constructors construct
values at the term level. Type and data constructors that take no arguments
(like Bool, and True/False) are constants.

Sometimes constructors take arguments. In those cases, it's like a function in
at least one sense -- it must be applied to become a concrete type or value.

When writing a type it's possible to create parameters for the type constructor,
called type variables. These parameters are placeholders for potential types
that can be used to construct the overall type.

Type variable names may in turn be used by data constructors.  However, you
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

One interesting use of kind signatures is to limit the arity of an input type
constructor, used to create the ``f a`` and ``f b`` types in ``fmap``'s type
signature::

  ·∾ :info fmap
  class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b

There is more nuance to kinds, but I don't have time to read about it right now,
so I'll just leave this link to the GHC wiki here.
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/type-type


11.5 Data constructors and values
---------------------------------

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
inhabitants it has.


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
|  data (a,b) = (a,b)                  |     a x b     |
+--------------------------------------+---------------+
|  a -> b                              |     b ^ a     |
+--------------------------------------+---------------+

.. include:: exercises/11.8.1_-_cardinality.rst


11.9 newtype
------------
The ``newtype`` keyword is used to define a type that can only have a single
unary data constructor. It's often used to rename existing types. Unlike type
aliases with the ``type`` keyword, you can't use the underlying type in place of
its alias. ``newtype``'s are semantically different from the wrapped type, even
though they share the same underlying representation.

::

  newtype Age = Age [ unAge :: Int }

  newtype N = N Int

One key contrast between a newtype and a type alias is that you can define type
class instances for newtypes that differ from the instances for their underlying
type. You can't do that for type synonyms.

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


-------------------------------------------------------------------------------------

.. topic:: The problem with record types

   When you define a data constructor with record fields, the field names are
   used to generate accessor funtions automatically. The problem is, those
   accessor functions are not unique to the type they're defined in. If you
   have the same field name shared by records of different types, the generated
   accessor functions will shadow, or overwrite, each other.

   Consider::

     data Record = Record { a :: String }
     data RecordClash = RecordClash { a :: String }

     Compiling this file results in:

     record.hs:2:34:
         Multiple declarations of `Main.a'
         Declared at: record.hs:1:24
                      record.hs:2:34

   This has been a pain point for a long time, and many approaches exist to deal
   with it. Here is a wiki article with links to many of them:

   https://gitlab.haskell.org/ghc/ghc/-/wikis/records

   The simplest possible soution is to prefix field names with the type they
   belong to. Here's an example of what I mean::

      data Comment = Comment {
            commentId           :: CommentId
          , commentContent      :: Content
          , commentReviewId     :: ReviewId
          , commentSubmissionId :: SubmissionId
          , commentConferenceId :: ConferenceId
          , commentDate         :: ISODate
          , commentReviewerNumber :: Int
        } deriving (Show)

-------------------------------------------------------------------------------------


11.18 Chapter Exercises
-----------------------

.. include: exercises/11.18.1_-_multiple_choice.rst
