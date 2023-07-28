****************************
 Chapter 2: Hello, Haskell!
****************************


2.1 Hello, Haskell
------------------
In order to run our code, we'll need a compiler.

Install stack by following the directions on their `website
<https://docs.haskellstack.org/en/stable/README/>`_.

.. topic:: Per-user install of stack, the quick version

   ::

     $ wget http://get.haskellstack.org/linux-x86_64.tar.gz &&
     tar -xf linux-x86_64.tar.gz '*/stack' -C ~/.local/bin &&
     printf '%s\n' 'PATH=$HOME/.local/bin:$PATH' >> ~/.profile

   This snippet makes assumptions based on my environment,
   and may not make sense for you, so consider the trade-offs.
   I've also omitted directions on verifying the binary and making
   sure ``$PATH`` is sane.

.. topic:: The difference between stack and cabal

  Here is a short summary of the difference between stack and cabal:
  https://gist.github.com/merijn/8152d561fb8b011f9313c48d876ceb07

Stack manages the entire tool-chain that you'll typically use for a
project in an isolated way. Right now we're only interested in it
because it will provide a compiler to execute our code with.

.. topic:: Setting up a knowledge base

    If you're like me, at this point you probably want to set up a small
    knowledge base of resources to learn haskell. Here are some relevant
    links:

    * `Codeslowers Haskell cheat sheet (pdf) <https://cheatsheet.codeslower.com/CheatSheet.pdf>`_
    * `Haskell in one video, a video cheat sheet <https://www.youtube.com/watch?v=02_H3LjqMr8>`_
    * `A survey and orientation guide of the Haskell landscape (tooling,
      libraries, best practices, some basic language features) <http://dev.stephendiehl.com/hask/>`_
    * `An excellent video series <https://www.youtube.com/playlist?list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV>`_
    * `The GHC users guide <https://downloads.haskell.org/ghc/latest/docs/html/users_guide>`_
    * `The Haskell 2010 language report <https://www.haskell.org/onlinereport/haskell2010/>`_
    * `A hierarchial list of libraries included with a typical installation of haskell
      <http://downloads.haskell.org/~ghc/8.6.5/docs/html/libraries/index.html>`_

    I've included a bash script to download these things for you, in pdf
    where possible, under ``02_-_hello_haskell/fetch_resources.bash``.

    Some other resources:

    * `Hoogle <https://wiki.haskell.org/Hoogle>`_ is like a search engine
      for API docs. You'll want to bookmark this.
    * A directory of frequently used modules, included with GHC:
      http://downloads.haskell.org/~ghc/8.6.5/docs/html/libraries/index.html
    * The freenode IRC #haskell channel is incredibly helpful, too.


2.2 Interacting with Haskell code
---------------------------------
Although Haskell is an ahead-of-time compiled language, it provides
both a compiler and an interpreter.

The compiler is known as GHC, short for Glasgow Haskell Compiler. You
can read more on the `homepage <https://www.haskell.org/ghc/>`_ or
peruse its `documentation <https://downloads.haskell.org/ghc/latest/
docs/html/users_guide/>`_. The interpreter, GHCi, is also part of the
GHC project.

When working with a compiler, the steps to run your code go roughly
like this:

* Edit and save your source code into a file,
* compile that file by running ``stack ghc $file`` as a separate step,
* and run the compiled artifact it produced from your shell like ``./progname``.

(This is of course the most naive possible workflow; it can be more automated.)

Working with an interpreter goes more like this:

* Start the interpreter with ``stack ghci``;
* type code into it and watch it be executed immediately.

Now try bringing up your prompt with ``stack ghci`` and entering some
arithmetic, to see if it's working.

::

  Prelude> 2 + 2
  4
  Prelude> 7 < 9
  True
  Prelude> 10 ^ 2
  100

As an elaboration of the workflow above, ghci allows you to load
existing files into your interactive session. To do so, type ``:load
$filename`` inside the repl.

You can also load a module, like ``:load module`` and GHC will search
for it in its search path. ``:load *module`` will not only load the
declarations the module exports, but also things internal to it.

Special commands that only GHCi understands start with the ``:``
character. Here are a few useful ones::

  $ stack ghci
  :load filename
  :reload
  :{
    multi-line
    expression
  :}
  -- evaluation order matters in the repl
  let x = 4  -- "let" is needed to declare regular variables in older versions of GHCi
  let x = 10 -- unlike in source files, you can reassign the same name to different values
  -- newer versions of GHCi don't need the "let"
  :info
  :type
  :doc -- only available in newer version of ghci
  :browse
  :show bindings
  :quit

For more, check the GHC documentation.


2.2.2 What is Prelude?
^^^^^^^^^^^^^^^^^^^^^^
Prelude is the standard module. It is imported into all Haskell files
by default, unless there is an explicit import declaration hiding it,
or the ``NoImplicitPrelude`` compiler extension is enabled, like
``stack ghci -XNoImplicitPrelude``.

Prelude is part of the ``base`` package, which comes with GHC. The
``base`` package includes other modules, too. You can read more `here
<http://dev.stephendiehl.com/hask/#base>`_.

Here is a function with a type signature for your inspection::

  sayHello :: String -> IO ()
  sayHello x = putStrLn ("Hello, "  ++ x ++ "!")

The double colon, ``::``, is read as "has the type". The entire line
would be read as "sayHello has the type String to IO unit". (Unit is
how you pronounce the empty tuple, ``()``.)


2.3 Understanding expressions
-----------------------------
Everything in Haskell is an expression or declaration.

Expressions are the building blocks of our programs, and programs
themselves are one big expression made of smaller expressions.

Declarations are top-level bindings which allow us to name
expressions.

2.3.1 Normal form
^^^^^^^^^^^^^^^^^
We say that expressions are in *normal form* when there are no more
evaluation steps that can be taken, or put differently, when they've
reached an irreducible form. Reducible expressions are also called
*redexes*.


2.4 Functions
-------------
A function is an expression that is applied to an argument and always
returns a result.

As in the lambda calculus, all functions in Haskell take one argument
and return one result. When it seems like we're passing multiple
arguments to a function, we are actually applying a series of nested
functions, each to one argument. This is called currying.

Functions are how we factor out patterns common to expressions into
something we can reuse with different inputs.

Here's one example of a simple function definition::

  -- name    body
  -- v       vvvvv
  triple x = x * 3
  --     ^
  --   parameter

2.4.2 Capitalization matters!
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Unlike Ada, Nim, and windows batch, identifiers are case sensitive.

``camelCase`` is the current convention for variables and functions.

``PascalCase`` is used for type constructors, data constructors, and
type class names, things you'll learn about later. This is enforced by
the compiler.

You can also use underscores and single quotes in identifiers.

Adding a single quote after a variable name sometimes suggests a
slightly altered version of it. In that circumstance, a single quote
is read as "prime".

Adding a ``_`` after the name may suggest that the output is thrown
out. There are a few loose conventions like this, you'll learn them
over time.


2.5 Evaluation
--------------
Evaluation is program execution. Haskell uses a non-strict evaluation strategy,
which defers evaluation of terms until they're forced by other terms requiring
them. Simplifying a term is called reducing. Values are a terminal point of
reduction.

Haskell doesn't evaluate everything to normal form by default. Instead
it only evaluates to weak head normal form.

Here's a great video on it `Haskell for Imperative Programmers #31 - Weak Head
Normal Form`_.

.. _Haskell for Imperative Programmers #31 - Weak Head Normal Form:
   https://www.youtube.com/
   watch?v=QBQ9_9R7o8I&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
   &index=32&t=0s

.. TODO Exercises: Comprehension Check -- page 35


2.6 Infix operators
-------------------
Functions default to prefix syntax, but you can also create infix
functions, which are known as operators.

Here's an example::

  ·∾ x |> f = f x
  ·∾ 3 |> (+ 12)
  15

If the function name is alphanumeric, it is a prefix function by
default. If the name is a symbol, it is prefix by default.

You can also use prefix functions as infix by surrounding them with
backticks::

  ·∾ 10 `div` 4
  2
  ·∾ div 10 4
  2

...and you can use infix functions a prefix by surrounding them with
parenthesis::

  ·∾ (+) 3 4
  7

2.6.1 Associativity and precedence
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can query the fixity, associativity, and precedence of a function
using ``:info``::

  ·∾ :info (+)
  class Num a where
    (+) :: a -> a -> a
  --     v-- precedence out of the range 0..9, where 9 binds most tightly
  infixl 6 +
  --   ^-- the l means left associative


.. topic:: The detils of associativity, precedence, and fixity

   Associativity is how arguments group to functions during a step
   in the evaluation process. If something is left associative,
   arguments to the left of an infix function are consumed first. If
   something is right associative, arguments to the right of the
   infix function are consumed first.  Haskell additionally has
   non-associative operators, like ``==``, ``/=``, ``<``, ``<=``,
   ``>``, ``>=``, ``\`elem\```, and ``\`notElem\```.

   Here's two examples where each term in the overall expression has the
   same associativity:

   Left associative,  ``f 1 2 3`` ≡ ``(((f 1) 2) 3)``.
   Right associative, ``1 2 3 f`` ≡ ``(1 (2 (3 f)))``.

   Precedence determines which function is evaluated first (who gets
   first turn at consuming arguments). In Haskell precedence goes
   from 0-9, where 9 is the highest (evaluated first). In Haskell,
   functions get a precedence of 9 by default (``infixl 9``).
   Additionally function application behaves as if it has fixity 10,
   record updates behave as if they have fixity 11.

   I asked about the default precedence of prefix function
   definitions on IRC, and learned some things not in the 2010
   Language Report.

   | **geekosaur** the default fixity of a function is infixl 9.
   |
   | **ski** justsomeguy: fixity of function application
   | (juxtaposition syntactic operator) is 10.
   |
   | **geekosaur** Function application behaves as if it has fixity
   | 10, record updates behave as if they had fixity 11.
   |
   | **justsomeguy** ski: I thought that precedence only goes from
   | 0..9? Or maybe that's only the range for fixity declarations,
   | rather than fixity in general?
   |
   | **ski** Ordinary (definable) operators have precedences
   | from zero to nine, sure.
   |
   | **justsomeguy** Thank you, that clears things up. I wish the
   | language report made that a little more obvious.
   |
   | **monochrom** Function application and record syntax cannot
   | be formally given predence levels because there is no
   | binary operator to attach the predence levels to.
   |
   | And the Haskell Report is supposed to give formal
   | definitions not intuitive conceptual moral fast-and-loose
   | bedtime stories.
   |
   | So it has to hide the intuitive interpretation in a
   | formal grammar.
   |
   | [Ed: The function application operator is technically not
   |  an operator, but a component of the AST.]

   You can have expressions that contain many functions with different
   associativity and different precedence. For example::

     -- (^) is right associative
     8 + 3 ^ 12 * 3

     -- Explicitly parenthesised, ()s show
     -- associativity, {}s show precedence.
     --
     (8 +) {({3 (^ 12)} *) 3 }
     -- ^        ^      ^
     -- left    right   left

   Fixity is where functions or operators are placed in relation to the
   arguments they consume. Some examples of fixity are infix, prefix,
   postfix, `circumfix, precircumfix, and postcircumfix <https://
   docs.raku.org/language/functions#Defining_operators>`_,
   or even `mixfix <https://agda.readthedocs.io/en/v2.5.2/language
   /mixfix-operators.html>`_. In haskell there are two possible
   fixities: prefix or infix. Language extensions may enable you to
   have more, I don't know.

   Arity is the number of arguments that a function consumes. A binary
   function takes two arguments. A ternary function takes three. A
   finitary function takes a finite number of arguments. An infinitary
   function takes an infinite number of arguments. A nullary function
   takes no arguments. You get the idea.


.. include:: exercises/2.6.2_-_parentheses_and_association.rst


2.7 Declaring values
--------------------
The order of declaration in a source code file doesn't matter because
GHCi loads the entire file at once, so it known all the values that
have been defined.

On the other hand, when you enter them one by one into the repl, the
order does matter.

Module names must begin with an uppercase letter.

2.7.1 Troubleshooting
^^^^^^^^^^^^^^^^^^^^^
White-space is significant in Haskell, just like Python.

The basic rule is that subsequent lines belonging to an expression
should be written under the beginning of that expression at the same
level of indentation.

 +---------------+---------------+
 |   Correct     |   Incorrect   |
 +===============+===============+
 | ::            | ::            |
 |               |               |
 |   let         |   let x = 3   |
 |     x = 3     |    y = 4      |
 |     y = 4     |               |
 |               |   -- or       |
 |   -- or       |               |
 |               |   let         |
 |   let x = 3   |    x = 3      |
 |       y = 4   |     y = 4     |
 |               |               |
 +---------------+---------------+

You can read about the particulars in the 2010 language report,
section 2.7 Layout.

.. include:: exercises/2.7.2_-_heal_the_sick.rst


2.8 Arithmetic functions in Haskell
-----------------------------------

.. include:: figures/arithmetic_functions.rst

The ``mod`` and ``rem`` functions keep on tripping me up.
https://ebzzry.io/en/haskell-division/

.. .. include:: figures/arithmetic_ghci_examples.rst


2.8.1 Laws for quotients and remainders
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. include:: figures/laws_for_quotients_and_remainders.rst

2.8.2 Using mod
^^^^^^^^^^^^^^^
If you're unfamiliar with modular division, you may not understand the
useful difference between mod and rem.

Modular arithmetic is a system of arithmetic for integers where
numbers "wrap around" upon reaching a certain value, called the
*modulus* (the second argument to ``mod``).

2.8.3 Negative numbers
^^^^^^^^^^^^^^^^^^^^^^
Negative numbers need to be surrounded in parenthesis, like this
``(-9)``. Otherwise the compiler may confuse the negative sign with
the infix subtraction operator. When ``-`` is used infix, it's a
synonym for ``subtract``.

Using ``-`` to make a number negative is syntactic sugar; You can
instead write it like this ``(negate 9)``. This is a bit of a special
case.


2.9 Parenthesization
--------------------
If you want to inspect an infix operator with ghci using the ``:info``
command, you usually have to surround it with parenthesis, like
``:info (^)``, for example.

The ``$`` operator can be used to avoid parenthesis, sometimes. It
will allow everything to the right of it to be evaluated first and can
be used to delay function application.

::

  ·∾ (2^) $ (+2) $ 3 * 2
  256

2.9.1 Parenthesizing infix operators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can also use parenthesis to apply only some arguments to a
function, and leave the other parameters available for binding.
Applying only some arguments is known as *partial application*.

For example ``(2^)`` is equivalent to ``(^) 2``, or more verbosely
``\x -> 2 ^ x``.

When you do this by surrounding the function with parenthesis, this is
sometimes known as *sectioning*.

Subtraction is a special case; ``(-2) 1`` won't work, because ``-`` has a
special case that it's treated as ``negate`` within parenthesis and
prefacing a numeric literal. ``(subtract 2) 1`` will give the desired
effect.

When sectioning operators, pay special attention to the associativity,
it will change the result.


2.10 Let and where
------------------
``let`` introduces an expression, so it can be used wherever you can
have an expression, but ``where`` is a declaration, and is bound to
the surrounding syntactic construct.

.. include:: exercises/2.10.1_-_exercises_a_head_code.rst


2.11 Chapter Exercises
----------------------

.. include:: exercises/2.11.1_-_parenthesization.rst

.. include:: exercises/2.11.2_-_equivalent_expressions.rst

.. include:: exercises/2.11.3_-_more_fun_with_functions.rst
