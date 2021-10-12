**********************
 Chapter 8: Recursion
**********************


  "Recursion (rĭ-kûr’-zhən) noun. If you still don't get it, see recursion."
  ~ Unknown

  "Recursion is the root of computation since it trades description for time."
  ~ Alan Perlis

  "The power of recursion evidently lies in the possibility of defining
  an infinite set of objects by a finite statement. In the same manner,
  an infinite number of computations can be described by a finite recursive
  program, even if this program contains no explicit repetitions."
  ~ Niklaus Wirth, Algorithms + Data Structures = Programs, 1976 ISBN 978-0-13-022418-7

  "One of the characteristics of recursion, then, is that it can take
  its own output as the next input, a loop that can be extended
  indefinitely to create sequences or structures of unbounded length
  or complexity."
  ~ http://assets.press.princeton.edu/chapters/s9424.pdf

  "Recursion is the process that consists in defining the value of
  a function by using other values of the same function."
  ~ Daniel Szmulewicz, Lisp ≠ Lambda Calculus,
  https://danielsz.github.io/blog/2019-08-05T21_14.html


8.1 Recursion
-------------
In this chapter, we will

* explore what recursion is and how recursive functions evaluate;
* go step-by-step through the process of writing recursive functions;
* have fun with bottom.

.. {{{

8.1 & ½, Some remarks about recursion
-------------------------------------

What is recursion, in general?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  "In general, something is recursive if it's defined in terms of itself."

  ~ Will Kurt,
    Get Programming with Haskell,
    Lesson 7: Rules for recursion and pattern matching,
    7.1 Recursion,
    Paragraph 1,
    Sentence 1.

  "An entity or concept is said to be recursive when simpler or
  smaller self-similar instances form part of its constituents."

  ~ Manuel Rubio-Sánchez,
    Introduction to Recursive Programming,
    Chapter 1: Basic Concepts of Recursive Programming,
    1.1 Recognizing Recursion,
    Paragraph 1,
    Sentence 1.

Here is the pattern of recursion: **When a structure contains
progressively smaller instances of itself, then it is recursive.**
The recursive programming techniques discussed in this book are
only particular uses of this general pattern.

One place recursion is often employed are function definitions. So,
how do you recognize a recursive function? Well, if a function calls
itself within its definition, then it's recursive.

.. !etymology recursion {{{

   recursion (n.)
   "return, backward movement," 1610s, from Latin recursionem (nominative recursio) "a running
   backward, return," noun of action from past-participle stem of recurrere "run back" (see recur).

.. !etymology recur

   recur (v.)
   late 14c., recuren, "to recover from illness or suffering" (a sense now obsolete); mid-15c., "to
   return" (to or into a place), from Latin recurrere "to return, run back, hasten back,"
   figuratively "revert, recur," from re- "back, again" (see re-) + currere "to run" (from PIE root
   *kers- "to run"). Originally of persons; application to thoughts, ideas, etc., "return to the
   mind," is recorded from 1620s. Meaning "happen again" is from 1670s. Related: Recurred;
   recurring.

.. !etymology re-

   re-
   word-forming element meaning "back, back from, back to the original place;" also "again, anew,
   once more," also conveying the notion of "undoing" or "backward," etc. (see sense evolution
   below), c. 1200, from Old French re- and directly from Latin re- an inseparable prefix meaning
   "again; back; anew, against."

   Watkins (2000) describes this as a "Latin combining form conceivably from Indo-European *wret-,
   metathetical variant of *wert- "to turn." De Vaan says the "only acceptable etymology" for it is
   a 2004 explanation which reconstructs a root in PIE *ure "back."

   In earliest Latin the prefix became red- before vowels and h-, a form preserved in redact,
   redeem, redolent, redundant, redintegrate, and, in disguise, render (v.). In some English words
   from French and Italian re- appears as ra- and the  following consonant is often doubled (see
   rally (v.1)).

   The many meanings in the notion of "back" give re- its broad sense-range: "a turning back;
   opposition; restoration to a former state; "transition to an opposite state." From the extended
   senses in "again," re- becomes "repetition of an action," and in this sense it is extremely
   common as a formative element in English, applicable to any verb. OED writes that it is
   "impossible to attempt a complete record of all the forms resulting from its use," and adds that
   "The number of these is practically infinite ...."

   Often merely intensive, and in many of the older borrowings from French and Latin the precise
   sense of re- is forgotten, lost in secondary senses, or weakened beyond recognition, so that it
   has no apparent semantic content (receive, recommend, recover, reduce, recreate, refer, religion,
   remain, request, require). There seem to have been more such words in Middle English than after,
   e.g. recomfort (v.) "to comfort, console; encourage;" recourse (n.) "a process, way, course."
   Recover in Middle English also could mean "obtain, win" (happiness, a kingdom, etc.) with no
   notion of getting something back, also "gain the upper hand, overcome; arrive at;" also consider
   the legal sense of recovery as "obtain (property) by judgment or legal proceedings."

   And, due to sound changes and accent shifts, re- sometimes entirely loses its identity as a
   prefix (rebel, relic, remnant, restive, rest (n.2) "remainder," rally (v.1) "bring together"). In
   a few words it is reduced to r-, as in ransom (a doublet of redemption), rampart, etc.

   It was used from Middle English in forming words from Germanic as well as Latin elements
   (rebuild, refill, reset, rewrite), and was used so even in Old French (regret, regard, reward,
   etc.).

   Prefixed to a word beginning with e, re- is separated by a hyphen, as re-establish, re-estate,
   re-edify, etc. ; or else the second e has a dieresis over it: as, reëstablish, reëmbark, etc. The
   hyphen is also sometimes used to bring out emphatically the sense of repetition or iteration :
   as, sung and re-sung. The dieresis is not used over other vowels than e when re is prefixed :
   thus, reinforce, reunite, reabolish. [Century Dictionary, 1895]

.. !etymology *kers-

   *kers-
   Proto-Indo-European root meaning "to run."

   It forms all or part of: car; career; cargo; caricature; cark; carpenter; carriage; carrier;
   carry; charabanc; charette; charge; chariot; concourse; concur; concurrent; corral; corridor;
   corsair; courant; courier; course; currency; current; curriculum; cursive; cursor; cursory;
   discharge; discourse; encharge; excursion; hussar; incur; intercourse; kraal; miscarry; occur;
   precursor; recourse; recur; succor.

   It is the hypothetical source of/evidence for its existence is provided by: Greek -khouros
   "running;" Latin currere "to run, move quickly;" Lithuanian karšiu, karšti "go quickly;" Old
   Irish and Middle Welsh carr "cart, wagon," Breton karr "chariot," Welsh carrog "torrent;" Old
   Norse horskr "swift."

.. !etymology motif

   motif (n.)
   "theme, predominant feature that recurs often in an artistic or dramatic work," 1848, from French
   motif "dominant idea, theme," from Medieval Latin motivus "moving, impelling," from past
   participle stem of movere "to move" (from PIE root *meue- "to push away"). Also a Middle English
   form of motive (late 14c.).

.. }}}

Recursive function calls as a control flow mechanism
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Control structures that overwrite names like loops are forbidden in Haskell, since they violate
referential transparency, so recursion is the only mechanism in the language that we have to express
repetition.

Recursion as an algorithmic problem solving tool
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Most recursive solutions are a special case of divide-and-conquer. In divide-and-conquer a problem
is repeatedly broken down into sub-problems, which are then used as a black box. The result of these
black box sub-problems are then combined to produce the final result. With recursive solutions,
however, each sub-problem is a smaller instance of the overall problem.

Defining recursive functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. "Note: Recursion is the process that consists in defining the value of a function by using other
.. values of the same function." ~ Lisp ≠ Lambda Calculus, Daniel Szmulewicz

A recursive function is often defined as "a function that calls itself". What we
mean by "calls itself" is that the function creates another execution instance.
A function is a "cookie cutter" from which any number of execution instances can
be created. While there is only on definition for a recursive function, there
can be any number of execution instances.

Termination and base cases
^^^^^^^^^^^^^^^^^^^^^^^^^^
If the definition of a recursive function were written so that the function
calls itself unconditionally, then every execution instance would
unconditionally call another instance, forever. This is referred to as
*infinite recursion*, or non-termination.

So, to make our functions useful, we must supply a non-recursive component, that
is triggered on some condition, to stop the recursive self-referencing calls.
This component is known as the *base case*.

In order to ensure that our base case is triggered, though, we must design the
recursive case to work towards it. Each invocation (or execution instance) of
the recursive case should either decrease the input size or simplify the
problem in a way that brings you closer to the base case.

Varieties of recursive patterns
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
**Linear recursion** occurs when methods call themselves only once.

**Tail recursion** is a type of linear recursion where the recursive call is the
last operation carried out int the recursive case. Therefore, they do not
manipulate the result of the recursive call.

**Multiple recursion** is where a method calls itself several times in some
recursive case.

**Mutual recursion** is when a functions call each other in a cyclical order.

**Nested recursion** occurs when an argument of a recursive function is defined
through another recursive call.

Examples of recursive anonymous function literals
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Here is the Fibonacci function written as an anonymous function literal. The ``fix`` function
duplicates the anonymous function and feeds it to itself. The anonymous function has an extra
parameter for an input function, named ``f``, so that it can receive its duplicated definition::

   ·∾  import Data.Function (fix)
   ·∾  fix (\f n -> if n <= 1 then 1 else n * f (n-1))  3
   6

.. }}}


8.2 Factorial!
--------------
Let's examine a simple factorial function::

  factorial 0 = 1
  factorial n = n * factorial (n - 1)

Base case: recursion stops when we get a 0. The return value, 1, is the identity
value.

Recursive case: Critically, the input size of this case gets smaller with each
application.

An example of how ``factorial 4`` evaluates::

  factorial 0 = 1
  factorial n = n * factorial (n - 1)

  -- The ==> symbol indicates a new step in the reduction process. Each call of
  -- ⧼factorial arg⧽ is replaced with its definition, where ⧼n⧽ has been replaced
  -- by the current argument.

  factorial 4 ==> 4 * factorial (4 - 1)
              ==> 4 * (4 - 1) * factorial ((4 - 1) - 1)
              ==> 4 * (4 - 1) * ((4 - 1) - 1) * factorial (((4 - 1) - 1) - 1)
              ==> 4 * (4 - 1) * ((4 - 1) - 1) * (((4 - 1) - 1) - 1) * factorial ((((4 - 1) - 1) - 1) - 1)
              ==> 4 * (4 - 1) * ((4 - 1) - 1) * (((4 - 1) - 1) - 1) * 1 -- base case is triggered
              ==> 4 * (4 - 1) * ((4 - 1) - 1) * 1 * 1
              ==> 4 * (4 - 1) * 2 * 1 * 1
              ==> 4 * 3 * 2 * 1 * 1
              ==> 4 * 3 * 2 * 1
              ==> 4 * 3 * 2
              ==> 4 * 6
              ==> 24


If we didn't supply the base case ``0 -> 1``, then the recursive call would
never stop, subtracting forever.

.. {{{

8.2.1 Another way to look at recursion
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Here's another example, to show how building up the call stack resembles
composing multiple instances of the same function::

  applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
  applyTimes 0 f b = b
  applyTimes n f b = f (applyTimes (n-1) f b)

  incTimes' :: (Eq a, Num a) => a -> a -> a
  incTimes' times n = applyTimes times (+1) n

  applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
  applyTimes 0 f b  =  b
  applyTimes n f b  =  f . applyTimes (n-1) f $ b
  --                     ^
  --  Building up the series of self-referential function compositions...

.. include:: exercises/8.2.2_-_intermission_exercise.rst
.. }}}

8.3 Bottom
----------
``⊥``, or bottom, denotes computations that don't successfully result in a
value. The two main varieties of bottom are computations that failed with an
error or those that failed to terminate. In logic ``⊥`` corresponds to
``False``.

Non-termination
^^^^^^^^^^^^^^^
Here's a non-terminating expression. When we run it, GHCi gets stuck in an
infinite loop::

  ·∾ let x = x in x
  ^CInterrupted.

With a different version of GHC, the expression ``let x = x in x`` may have
resulted in an exception, instead of heating up my laptop until I pressed Control-c.

Partial functions
^^^^^^^^^^^^^^^^^
Another source of bottom are partial functions. For example::

  ·∾ :{
   ⋮ f :: Bool -> Int
   ⋮ f False = 0
   ⋮ :}
  ·∾
  ·∾ f False
  0
  ·∾ f True
  *** Exception: <interactive>:3:1-11: Non-exhaustive patterns in function f

In order to defend against this, we can define a catch-all case. Or, if it doesn't make sense to do
so, we can explicitly return nothing, using the ``Maybe`` data type, like this::

  ·∾ :{
   ⋮ f :: Bool -> Maybe Int
   ⋮ f False = Just 0
   ⋮ f _     = Nothing
   ⋮ :}
  ·∾
  ·∾ f True
  Nothing
  ·∾ f False
  Just 0

Undefined values
^^^^^^^^^^^^^^^^
What happens if we try to evaluate ``undefined`` in the repl? ::

  ·∾ undefined
  *** Exception: Prelude.undefined
  CallStack (from HasCallStack):
    error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
    undefined, called at <interactive>:22:1 in interactive:Ghci9

Intentionally thrown errors
^^^^^^^^^^^^^^^^^^^^^^^^^^^
Another source of bottom values are intentionally thrown errors. The function
``error`` prints a string and returns ``undefined``. ::

  ·∾ error "Should this be in a monad?"
  *** Exception: Should this be in a monad?
  CallStack (from HasCallStack):
    error, called at <interactive>:27:1 in interactive:Ghci9


8.4 Fibonacci numbers
---------------------
This section demonstrates the steps you'd typically go through to
write a recursive function by demonstration with a Fibonacci function.

But first, here's quip I found useful. The recursive problem solving
process can be described loosely as follows:

  * If the given instance of the problem can be solved directly, do so.
  * Otherwise, reduce it to one or more smaller instances of the same problem.

Now, we'll return to the demonstration...

**Consider the types**
**Consider the base case**
**Consider the arguments**
**Consider the recursion**


8.6 Chapter Exercises
---------------------

.. include:: exercises/8.6.1_-_review_of_types.rst

.. include:: exercises/8.6.2_-_reviewing_currying.rst

.. include:: exercises/8.6.3_-_recursion.rst

.. include:: exercises/8.6.4_-_fixing_dividedby.rst

.. include:: exercises/8.6.5_-_mccarthy_91_function.rst
