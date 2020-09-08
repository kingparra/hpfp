****************************
 Chapter 2: Hello, Haskell!
****************************


2.1 Hello, Haskell
------------------
First, install stack by following the directions on their `website
<https://docs.haskellstack.org/en/stable/README/>`_.

Stack manages the entire tool-chain that you'll typically use for a
project. Right now we're only interested in it because it will provide
a compiler/interpreter to our execute code with, though.

If you're like me, at this point you probably want to set up a small
knowledge base of resources to learn haskell. Here are some relevant
links:

* Cheat sheet: https://cheatsheet.codeslower.com/CheatSheet.pdf
* Haskell in one video: https://www.youtube.com/watch?v=02_H3LjqMr8
* A survey and orientation guide of the Haskell landscape: http://dev.stephendiehl.com/hask/
* A good video series: https://www.youtube.com/playlist?list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
* The GHC manual: https://downloads.haskell.org/ghc/latest/docs/html/users_guide
* The Haskell 2010 language report: https://www.haskell.org/onlinereport/haskell2010/

I've included a bash script to download these things for you, under
``02_-_hello_haskell/fetch_resources.bash``.


2.2 Interacting with Haskell code
---------------------------------
Although Haskell is an ahead-of-time compiled language, it provides
both a compiler and an interpreter.

The compiler is known as GHC, short for Glasgow Haskell Compiler. You
can read more on the `homepage <https://www.haskell.org/ghc/>`_ or
peruse its `documentation <https://downloads.haskell.org/ghc/latest/
docs/html/users_guide/>`_.

When working with a compiler, the steps to run your code go roughly
like this:

* Edit and save your source code into a file,
* compile that file by running ``stack ghc $file`` as a separate step,
* and run the compiled artifact it produced from your shell like ``./progname``.

(This is of course the most naive possible workflow; most of these
steps can be automated.)

Working with an interpreter goes more like this:

* Start the interpreter with ``stack ghci``;
* type code into it and watch it be executed immediately.

Now try bringing up your prompt with ``stack ghci`` and entering some
arithmetic, to see if it's working.

::

  ·∾ 2 + 2
  4
  ·∾ 7 < 9
  True
  ·∾ 10 ^ 2
  100
 
As an elaboration of the workflow above, ghci allows you to load
existing files into your interactive session. To do so, type ``:load
$filename`` inside the repl. 

You can also load a module, like ``:load module`` and ghc will search
for it in its search path. ``:load *module`` will not only load the
modules regular top-level imports, but also things internal to it.

Special commands that only GHCi understands start with the ``:``
character. Here's a quick summary::

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
  :doc
  :browse
  :show bindings
  :quit

What is Prelude?
^^^^^^^^^^^^^^^^
Prelude is the standard library. It is imported into all Haskell files
by default, unless there is an explicit import statement for it, or
the ``NoImplicitPrelude`` compiler extension is enabled, like ``stack
ghci -XNoImplicitPrelude``.

Prelude is part of the ``base`` package, which comes with GHC. The
``base`` package includes other modules, too. You can read more 
`here <http://dev.stephendiehl.com/hask/#base>`_.

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

Normal form
^^^^^^^^^^^
We say that expressions are in *normal form* when there are no more
evaluation steps that can be taken, or put differently, when they've
reached an irreducable form. Reducable expressions are also called
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
something we can use with different inputs.

Here's one example of a simple function definition::

  -- name    body
  -- v       vvvvv
  triple x = x * 3
  --     ^
  --   parameter

Capitalization matters!
^^^^^^^^^^^^^^^^^^^^^^^
Identifiers are case sensitive. ``camelCase`` is the current
convention. Underscores and single quotes are allowed in variable
names, too.


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

You can query the fixity, associativity, and precedence of a function
using ``:info``::

  ·∾ :info (+)
  class Num a where
    (+) :: a -> a -> a
    ...
    -- Defined in ‘GHC.Num’
  --     v-- precedence out of the range 0..9, where 9 binds most tightly
  infixl 6 +
  --   ^-- the l means left associative

.. TODO 2.6.1 Exercises: Parentheses and Association -- page 39


2.7 Declaring values
--------------------
The order of declaration in a source code file doesn't matter because
GHCi loads the entire file at once, so it known all the values that
have been defined.

On the other hand, when you enter them one by one into the repl, the
order does matter.

Troubleshooting
^^^^^^^^^^^^^^^
White-space is significant in Haskell, just like Python. When confused,
add more spaces.

.. TODO 2.7.1 Exercises: Heal the Sick -- page 45


2.8 Arithmetic functions in Haskell
-----------------------------------

.. TODO import the table of arithmetic functions here

The ``div`` function rounds down to negative infinity, ``quot`` rounds
towards 0.
