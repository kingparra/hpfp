********************
 Chapter 3: Strings
********************


3.1 Printing strings
--------------------
In this chapter we'll learn about the ``String`` type, use a few list
processing functions, go over some basic syntax, and interact with the
repl.


3.2 A first look at types
-------------------------
Strings are represented as lists of UTF-16 characters. They are
enclosed in double quotes. Single quotes are reserved for characters.

When you query the type of a string in the repl, its composite nature
becomes apparent::

  Prelude> : type "Hello!"
  "Hello!" :: [Char]

Sometimes you may see ``String`` in type signatures. This is really a
type alias for ``[Char]``.

Functions related to strings:
* ``putStr`` print a string
* ``putStrLn`` print a string with a newline appended
* ``show`` convert a datatype to a string representation -- not all types
  implement this function, only types that have instances of the ``Show`` type
  class methods do. Think of this like ``__repr__`` in Python.
* ``print`` this is like ``putStrLn . show``


3.4 Top-level versus local definitions
--------------------------------------
Top-level declarations are declarations that are visible anywhere
within a module. In other words, they aren't nested within other
functions.

You can use ``let``, and lambdas to create definitions local to an
expression.

The ``where`` keyword can be used to create definitions that are
local to a declaration.

.. include:: exercises/3.4.1_-_exercises_scope.rst


3.5 Types of concatenation functions
------------------------------------
* ``(++)`` append to a string or list
* ``concat`` concatenate a list of lists or strings


3.7 More list functions
-----------------------
* ``(:)`` prepend to a list or string
* ``head`` take the first element
* ``tail`` take the last element
* ``take`` take ``n`` elements
* ``drop`` remove ``n`` elements from the beginning
* ``(!!)`` get an element at an index


3.8 Chapter Exercises
---------------------

.. include:: exercises/3.8.1_-_reading_syntax.rst

.. include:: exercises/3.8.2_-_building_functions.rst
