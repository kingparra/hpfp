*************************************
 Chapter 7: More Functional Patterns
*************************************
I think I'll respond to the Sussman quote with an epigraph
from chapter 1 of `SICP <https://xuanji.appspot.com/isicp/>`_.

.. epigraph::

   The acts of the mind, wherein it exerts its power over
   simple ideas, are chiefly these three:

   1. Combining several simple ideas into one compound
      one, and thus all complex ideas are made.

   2. The second is bringing two ideas, whether simple
      or complex, together, and setting them by one
      another so as to take a view of them at once,
      without uniting them into one, by which it gets
      all its ideas of relations.

   3. The third is separating them from all other ideas
      that accompany them in their real existence: this
      is called abstraction, and thus all its general
      ideas are made.

   -- John Locke, An Essay Concerning Human Understanding (1690)


7.1 Make it func-y
------------------
This chapter explores common idioms enabled by first-class
functions, pattern matching, and composition. Particularly,
we will:

* learn about the mechanics of pattern matching;
* explore case expressions and guards -- elegant control
  flow constructs that allow selection based on pattern
  matches;
* nest functions, experiment with lambda expression;
* use combinators that change the order of function
  application, like ``(.)`` and ``$``.


7.2 Arguments and parameters
----------------------------
Functions are defined by the fact that they can be applied to an
argument and return a result.

.. topic:: What happens if you try to apply a value to another value?

    First, let's try it with a concrete type.

    .. code-block::
       :emphasize-lines: 3

        ·∾ True False
        <interactive>:20:1: error:
            • Couldn't match expected type ‘Bool -> t’ with actual type ‘Bool’
            • The function ‘True’ is applied to one argument,
              but its type ‘Bool’ has none
              In the expression: True False
              In an equation for ‘it’: it = True False
            • Relevant bindings include it :: t (bound at <interactive>:20:1)

    The highlighted line above tells us that GHC was expecting a
    function, but the nullary data constructor ``True`` doesn't take
    any arguments, so it can't reduce the expression.

    What if we try it on a polymorphic value, like a number (which
    defualts to ``Num a => a``), instead?

    .. code-block::
       :emphasize-lines: 6

        ·∾ 1 2
        <interactive>:4:1: error:
            • Non type-variable argument in the constraint: Num (t1 -> t2)
              (Use FlexibleContexts to permit this)
            • When checking the inferred type
                it :: forall t1 t2. (Num t1, Num (t1 -> t2)) => t2

    Alghough it should be possible to puzzle this out based on the
    inferred type signature, I'd be pretty baffled if I encountered
    this error without seeing the term-level code, first.

    If the error just had an ``In the expression: 1 2`` added to it
    things would be much more obvious. At least the line and column
    numbers help.

    This is why I do these experiments -- it makes it easier to
    know where to start looking.

A parameter is a name that acts as a placeholder for an input within a
function definition. An argument is a value that is applied to the
function.

When you apply a function to an argument, the value of that argument
is bound to a parameter name within the definition for that particular
instance of that function, or that function call.

::

  ·∾ -- v parameter
  ·∾ id x = x

  ·∾ -- v argument
  ·∾ id 3 -- parameter x is bound to 3 for this particular function invocation.
  3

Functions are applied to arguments which binds their parameters to
values.

7.2.1 Setting parameters
^^^^^^^^^^^^^^^^^^^^^^^^
As you add parameters infix type constructors for curried functions and their
respective input type variables are added to the type signature. ::

  ·∾ -- Notice what happens to the type
  ·∾ -- signature as we introduce more
  ·∾ -- parameters.

  ·∾ myNum = 1 :: Integer

  ·∾ myVal f g = myNum
  ·∾ :type myVal
  myVal :: p1 -> p2 -> Integer

  ·∾ myVal f g h = myNum
  ·∾ :type myVal
  myVal :: p1 -> p2 -> p3 -> Integer

  ·∾ myVal f g h i = myNum
  ·∾ :type myVal
  myVal :: p1 -> p2 -> p3 -> p4 -> Integer

Saturating a parameter with an argument removes one ``-> a`` from the
type signature. ::

  ·∾ -- And what happens as we apply
  ·∾ -- arguments to those parameters.

  ·∾ myVal f g h i = myNum
  ·∾ :type myVal
  myVal :: p1 -> p2 -> p3 -> p4 -> Integer

  ·∾ :type myVal 1
  myVal :: p1 -> p2 -> p3 -> Integer

  ·∾ :type myVal 1 2
  myVal :: p1 -> p2 -> Integer

  ·∾ :type myVal 1 2 3
  myVal :: p1 -> Integer

  ·∾ :type myVal 1 2 3 4
  myVal :: Integer

7.2.2 Binding variables to values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Haskell is lexically scoped. This means that resolving the value for a named
entity depends on the names location in source code. (This is in contrast to
dynamic scoping, where the execution context of what previously ran determines
what a name stands for, for example. Bash does this -- yeah, let that sink in.)

The lexically innermost binding for a variable of a particular name always takes
precedence.

If an inner scope defines a name that already exists in the outer scope, it
*shadows* that name, so the outer definition is no longer visible within that
inner scope. ::

  ·∾ :{
   ⋮ bindExp :: Integer -> String
   ⋮ bindExp x = let x = 10; y = 5 in
   ⋮               "x: " ++ show x ++
   ⋮               ", "  ++
   ⋮               "y: " ++ show y
   ⋮ :}
  ·∾ bindExp 8
  "x: 10, y: 5"
  ·∾ -- The parameter x is shadowed by the definition for x within
  ·∾ -- the let expression, essentially blocking the visibility of
  ·∾ -- the parameter.

In a repl session, each line shadows the next. The mechanics of this are
conceptually similar to nested lambda expressions. ::

  ·∾ x = 5
  ·∾ y = x + 5
  ·∾ y
  10

This analogy begins to break down when you consider top-level declarations,
like imports, though.

Also note that multi-line expressions are treated as one logical "line". ::

  ·∾ -- multi-line expressions are don't shadow names until they're ended
  ·∾ :{
   ⋮ x = 3
   ⋮ x = 4
   ⋮ :}
  <interactive>:93:1: error:
      Multiple declarations of ‘x’
      Declared at: <interactive>:91:1
                   <interactive>:93:1


7.3 Anonymous functions
-----------------------
Anonymous functions are function literals, or functions without a name.

Here is a named function::


   triple :: Integer -> Integer
   triple x = x * 3

...and here is the same function but with anonymous function syntax::

   (\x -> x * 3) :: Integer -> Integer

.. include:: exercises/7.3.1_-_grab_bag.rst

7.3.2 The utility of lambda syntax
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You'll most often use lambdas when you're passing a function as an argument, and
that's the only place in your program where you'll need it. If you aren't going
to call the function again, why give it a name?


7.4 Patten matching
-------------------
Pattern matching allows you to do three things:

* compare literal values against an input value;
* access arguments or fields of an input data constructor using a pattern
  (destructuring);
* and bind names to successful matches (for literals or destructured arguments).

Literal values
^^^^^^^^^^^^^^
A literal value can be any data constructor, like ``True``, or ``1``, or even
compound structures like ``(1,2)``, ``Just 3``, and ``[('c',2),('d',4)]``.

Simple values
"""""""""""""
This function matches a pattern, but doesn't do any destructuring or name
binding. Even literal values are patterns. It returns ``True`` if given ``1``,
and throws an exception for anything else::

  isOne 1 = True

In ghci::

  ·∾ isOne 1
  False
  ·∾ isOne 8
  *** Exception: <interactive>:185:1-14: Non-exhaustive patterns in function isOne


Destructuring compound values
"""""""""""""""""""""""""""""
Here is a function that works on a more complicated structure. This pattern uses
destructuring to access sub-elements, but does not bind names. It will return
``False`` if you give it this very particular tuple literal::

  contrived :: ([a], Char, (Int, Float), String, Bool) -> Bool
  contrived    ([],  'b',  (1,   2.0),   "hi",   True) = False

In ghci::

  ·∾ contrived ([], 'b', (1, 2.0), "hi", True)
  False

If you give it a different tuple, you'll get an error, since we haven't defined
a catch-all pattern::

  ·∾ contrived ([], 'F', (8, 9.8), "bye", False)
  *** Exception: <interactive>:165:1-49: Non-exhaustive patterns in function contrived

Name binding
^^^^^^^^^^^^
Patterns look a lot like data constructors, but in place of data constructor
parameter you can supply a name, instead. If a value (or argument) inhabits that
parameter, it gets bound to the name you've supplied.

Name binding and destructuring
""""""""""""""""""""""""""""""
Here is an example pattern that assigns the third element of a triple to ``a`` and
discards the rest::

  ·∾ (_,_,a) = (1,2,3)
  ·∾ a
  3

The ``_`` you see here is a wildcard -- a special name that throws away the
result and always matches.

Mechanics of name binding
"""""""""""""""""""""""""
We said that the wildcard always matches; so does that mean it's possible for a
name not to match?

Yes. Here are the rules:

* Matching can have one of three results. It may fail; it may succeed, returning
  a binding for each variable in the pattern; or it may diverge.
* Pattern matching proceeds from left to right and outside to inside.
* Patterns in any one equation are not allowed to have more than one occurrence
  of the same formal parameter (a property called linearity).
* Except for the wildcard, ``_``, which you can use more than once.

There is more nuance for you to explore, (like refutable vs irrefutable
patterns, strictness, as patterns) but I'm not going to add that here. Instead,
I refer you to `section 3.17 of the 2010 Haskell language report
<https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-580003.17>`_
so you can look up the details as you need them. Remember that programming
languages are tools with infinite fractal universes of detail, and *doign is the
best way to reach understanding.* In fact -- doing is the entire point!

(But also, you can check my old notes, which contain the details, you masochist.)

7.4.1 Handling all the cases
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Here's something more pragmatic -- if you write your patterns in the wrong
order, your function will fail.

In the function ``isItTwo`` the wildcard is always matched, and it wont ever
return ``True`` for ``2``::

  isItTwo :: Integer -> Bool
  isItTwo _ = False
  isItTwo 2 = True

Here's what happens if we load this into ghci and try::

  ·∾ :load is.hs
  [1 of 1] Compiling Main             ( is.hs, interpreted )
  is.hs:3:1: warning: [-Woverlapping-patterns]
      Pattern match is redundant
      In an equation for ‘isItTwo’: isItTwo 2 = ...
    |
  3 | isItTwo 2 = True
    | ^^^^^^^^^^^^^^^^
  Ok, one module loaded.
  ·∾ isItTwo 8
  False
  ·∾ isItTwo 2
  False

This is called an overlapping pattern. To avoid this situation, try to order you
patterns from most to least specific, particularly when using the wildcard.

Another issue is that of non-exhaustive patterns. This is when you don't have
patterns defined to deal with every case. (A partial function.) You patterns
will then fail to match, and return bottom, which will throw an exception, which
if unhandled will make your program fail.

When given an input without a case, the patterns will fail to match and return
bottom, which will cause an exception.

It helps to have ``-Wall`` turned on. GHC will then warn about both of these
situations.

.. include:: exercises/7.4.4_-_variety_pack.rst
