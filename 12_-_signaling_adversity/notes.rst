*********************************
 Chapter 12: Signaling adversity
*********************************


12.1 Signaling adversity
------------------------
This chapter is all about dealing with failure conditions using datatypes like
Maybe and Either, and how to expose functions that filter invalid input that
still typechecks while constructing datatypes. It also has some things to say
about higher-kindness and anamorphisms.

.. https://blog.thomasheartman.com/posts/haskells-maybe-and-either-types/
.. http://dev.stephendiehl.com/hask/#algebraic-datatypes Control-F for "smart constructors". He has a decent example here.
.. https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling
.. https://leanpub.com/finding-success-in-haskell


12.2 How I learned to stop worrying and love Nothing
----------------------------------------------------
``Maybe`` represents the potential of failure when computing a result. It is
used to express that something can be in one of two states: defined or
undefined. Another way to think of this is that the result is partial; that is,
not defined for all values of arguments. To get around this, we use ``Maybe`` to
return eithther a constructor that *wraps* the result type, or the ``Nothing``
data constructor, which acts as an explicit signal of failure to return sensible
result.

So, what does it look like? ::

  data Maybe a = Nothing | Just a

Here is a simple example of its use::

  ifEvenAdd2 :: Integer -> Maybe Integer
  ifEvenAdd2 n = if even n then Just (n+2) else Nothing

12.2.1 Smart constructors for datatypes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Let's consider a ``Person`` type that keeps track of two things, a name and age.

::

  type Name = String
  type Age = Integer
  data Person = Person Name Age deriving Show

There are a few problems here. One is that we could construct a ``Person`` with
an empty string for a name, or make a person who is negative years old. This is
no problem to fix with ``Maybe``, though. Instead, we'll create a *smart
constructor*, a function to construct values of type ``Person``, that will
filter out invalid inputs for us.

::

  mkPerson :: Name -> Age -> Maybe Person
  mkPerson name age
    | name /= "" && age >= 0  =  Just (Person name age)
    | otherwise               =  Nothing


12.3 Bleating Either
--------------------
``Either`` is mostly used as a generalization of ``Maybe`` in which ``Left`` not
only encodes failure but is accompanied by an error message. ``Right`` encodes
success and the accompanying value. As you can see by the type variables ``a``
and ``b``, ``Left`` and ``Right`` may each wrap different types::

  data Either a b = Left a | Right b

Treating ``Left`` as a failure state has a few motivations, but it's mostly just
a convention. One reason to adhere to this convention is how pre-defined type
class instances have chosen to implement behaviour for ``Left``.  For example,
``Functor`` will not map over the left type argument.

Around this section, the book has an example where ``mkPerson`` is incrementally
modified so that it uses ``Either`` to return an indication of possible failure
modes for constructing a ``Person``. You can view these in the
``figures/13.{2,3}`` directories.

.. TODO Create a terminal recording where I interact with the figures,
..      and do the "try it" sections. (Those are mostly captured by an
..      expect script right now, and the figures have test cases, so I
..      have something to start with.)


12.4 Kinds, a thousand stars in your types
------------------------------------------
In this section there are many detail about kinds. There are also two long ghci
sessions where you inspect kind signatures. I haven't included this in my notes
because I can't answer the following questions:

* Why is this important?
* How is this relevant to getting shit done?
* Why should I care?

Without that, this seems like useless trivia, and I'm pretty sure I'll forget
it. That's OK, though, I can look it up when I need to. Search engines exist.


12.5 Chapter Exercises
----------------------

.. include:: exercises/12.5.1_-_determine_the_kinds.rst