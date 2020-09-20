****************************
 Chapter 4: Basic datatypes
****************************

The quote by Robin Milner comes from the preface of "The Little MLer".


4.1 Basic Datatypes
-------------------
In this chapter we will

* review types we have seen in previous chapters;
* learn about datatypes, type constructors, and data constructors;
* work with predefined datatypes;
* learn more about type signatures and a bit about type classes.


4.2 What are types?
-------------------
Types are how we categorize values that share something in common.

A type has a name, a set of values, and operations that are
permissible for those values.

In Haskell the operations are not part of the type definition, but
implemented as functions or type class methods, instead.

The intent is to prevent operations that don't make sense.


4.3 Anatomy of a data declaration
---------------------------------
Data declarations are how datatypes are defined.

Here's a simple example::

  --  type constructor
  --    |
  --    |       disjunction
  --    |          "or"
  --    |           |
  --    v           v
  data Bool = False | True
  --           ^        ^
  --           |        |
  --        data constructors

You can find the datatype definition for built-in types using
``:info`` in ghci.

.. include:: exercises/4.3.1_-_exercises_mood_swing.rst
