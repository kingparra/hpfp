***********************************
 Chapter 1: All You Need is Lambda
***********************************


1.1 All you need is lambda
--------------------------

What is a calculus?
^^^^^^^^^^^^^^^^^^^
* A calculus is a notation used to formalize a
  method of calculation or reasoning.
* Calculus' have a set of axioms (fundamental
  assumptions), and a set of derivation rules
  (data transformation rules, also called
  inference rules).
* With them, you can use the calculus to prove
  something.
* You can think of a calculus as a miniature
  language for reasoning, or maybe a game.
* A calculus is just a bunch of rules for
  manipulating symbols. One can give meaning
  to the symbols (semantics), but that's not
  part of the calculus (pure syntax). One can
  associate meanings to expressions in a way
  that corresponds to computations (functional
  programs).

  Formalizing a method means ascribing a
  calculus to it, which then has meaning
  assigned to syntactic constructs, and
  defining transformation rules, as well as
  demonstrating how they are performed.


What is the lambda calculus?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* Like Turing machines, the Lambda calculus is a model of computation that
  formalizes the concept of effective computability.
* In case you're wondering what a model is, the encyclopedia of mathematics
  says "A model is a description of a system, theory, or phenomenon that
  accounts for its known or inferred properties and may be used for further
  study of its characteristics."
* In other words, that lambda calculus is able to prove whether or not an
  algorithm for a problem can eventually be computed, and reason about its
  characteristics. (Space and time complexity.)
* Anything that can be computed in the lambda calculus can be executed by a
  Turing machine, and vice versa. They are both "universal machines".
* Here is a more precise definition of a model
  of computation, from Wikipedia:

  A model of computation is a model which
  describes how an output of a mathematical
  function is computed given an input.

  A model describes how units of computations,
  memories, and communications are organized.

  The computational complexity of an algorithm
  can be measured given a model of
  computation.

  Using a model allows studying the
  performance of algorithms independently
  of the variations that are specific to
  particular implementations and specific
  technology.

How does this apply to Haskell?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* Haskell's regular language syntax reduces to
  a subset of the language called the language
  kernel.
* The language kernel reduces to the core
  type, which is an implementation of a typed
  lambda calculus called system fc.
* This eventually turns into a build artifact
  you can run on your computer.
* Furthermore, the evaluation strategy used by
  Haskell resembles the lambda calculus.


1.2 What is functional programming?
-----------------------------------
* Functional programming is a computer
  programming paradigm that relies on
  functions that behave like mathematical
  functions.
* The essence of functional programming is
  that programs are a combination of
  expressions.
* Expressions include concrete values,
  variables, and also functions.
* **Functions are expressions that can be
  applied to an argument, and once applied,
  can be reduced or evaluated.**

* What is unique about functions, as opposed to other expressions?

  For one thing, they introduce name bindings as formal parameters.

  Does the name binding mechanism of parameters imply a phase separation?

* Functions are *first-class*, which means that they can be used as values or
  passed as arguments to yet more functions.
* Personally I'd add that, for something to be considered first-class, it should
  also be able to be represented literally, without needing a name binding.
* Purity in the context of functional programming languages means that the
  language does not include any features which are not translatable to lambda
  expressions.

  * Wait, isn't everything translatable to lambda expressions? Isn't saying
    "untranslatable to lambda expressions" like saying "untranslatable to binary"
    or "untranslatable to instructions for a Turing machine"?

* Purity sometimes also is used refer to referential transparency.
* An expression is called referentially transparent if it can be replaced with
  its corresponding fully reduced value at any point in the execution of the
  program without changing the programs behavior.
* If x=y, then f(x)=f(y).

* The paper "What is a Purely Functional Language?"
  by Amr Sabry defines purity like this.

  A language is purely functional if it
  includes every simply typed lambda-calculus
  term, and its call-by-name, call-by-need,
  and call-by-value implementations are
  equivalent (modulo divergence and errors).

.. There is currently much research effort
.. towards finding efficient realizations of
.. stateful algorithms in functional languages
.. while maintaining the purity.

.. Without a formal definition of purity, we
.. can't determine the correctness of these
.. realizations.

* This means side effects in function definitions are not allowed. To do
  effectful things, those effects have to be represented as a value of some
  type.
* The importance of referential transparency is that it allows the programmer
  to reason about program execution as evaluation in a rewrite system -- which
  is exactly what the lambda calculus is.

functional vs imperative
^^^^^^^^^^^^^^^^^^^^^^^^
* Imperative languages are based on assignment sequences whereas functional
  languages are based on nested function calls.
* In imperative languages, the same name may be associated with several values,
  whereas in functional languages a name is only associated with one value.
* In imperative languages, new values may be associated with the same name
  through command repetition whereas in functional languages new names are
  associated with new names through recursive function call nesting.
* With functional languages, because there is no assignment, substructures in
  data structures cannot be changed one at a time.
* Functions are values.


1.3 What is a function?
-----------------------
* A function is a relation between a set of possible inputs and a set of
  possible outputs.
* In math parlance, an "input set" is a domain, an "output set" is a co-domain,
  and a "the set of possible outputs for a specific input" is an image.
* The same function, given the same values to evaluate, will always return the
  same result. This is known as referential transparency.
* Valid: {1,2,3} -> {A,B,C}. Not valid : {1,1,2} -> {X,Y,Z}. Also valid: {1,2,3}
  -> {A,A,A}.

.. What are some other ways to describe a function?

   Everything in a pure functional program is an expression.

   Each expression is (or reduces to) a single value.

   An expression may denote its value literally.

   An expression may instead contain a name (or names) within its definition.

   A name stands for an expression.

   An association between a name and the expression it stands for is known as a name binding.

   Names who have an expression are bound.

   Names without an expression are unbound.

   Unbound names cannot be resolved to an expression.

   Bound names can be resolved to the expression they stand for.

   A function is an expression with a parameter.

   In order to simplify a function we must bind some other expression to the name it introduces.


1.4 The structure of lambda terms
---------------------------------
* The lambda calculus has three basic components: expressions, variables, and
  abstractions.
* Variables and abstractions are types of expressions.
* Here's some ebnf that describes the entire language::

    | lcletter     ⇐   a | b | c | ... | z
    | name         ⇐   lcletter {lcletter}
    | expression   ⇐   name | function | application
    | function     ⇐   λ name . expression
    | application  ⇐   expression expression

* A space means "followed by", curly braces indicate repetition of the enclosed
  term (zero or more times). The stroke, "|", means OR.

Abstractions
^^^^^^^^^^^^
* An abstraction is a function. It has a head (a lambda), a body, and it is
  applied to an argument.
* Here's the anatomy of an abstraction::

    |  λ x . x
    |  ^─┬─^
    |    └────── Extent of the head of the lambda.
    |
    |  λ x . x
    |    ^────── The single parameter of the function. This binds any variables
    |            with the same name in the body of the function.
    |
    |  λ x . x
    |        ^── body, the expression the lambda returns when applied. This is a
    |            bound variable.

* The dot (.) separates the parameters of the lambda from the function body.

Alpha equivalence
^^^^^^^^^^^^^^^^^
* Variable/parameter names are arbitrary (equivalent to each other) and can
  be changed to make them unique.
* Sometimes this is necessary to make sense of an expression when calculating
  things by hand. Which x was I working on again?


1.5 Beta reduction
------------------
* Beta reduction is the process of evaluating an abstraction. (Computing a
  function.)
* This involves removing the head, substituting in the input for all
  occurrences of the bound variable, and then evaluating the function body.
* Applications in the lambda calculus are left associative. (*arguments*
  are associated with the function on their left.)
* Here's what the process of beta reduction looks like::

    | ((λx.x)(λy.y))z
    | [x ∶= (λy.y)]
    | (λy.y)z
    | [y ∶= z]
    | z

* The syntax ``[x ∶= z]`` here is used to indicate that ``z`` will be substituted
  for all occurrences of ``x`` (here ``z`` is the function ``λy.y``).
* Beta reduction stops when there are no longer unevaluated functions applied to
  arguments.

Free variables
^^^^^^^^^^^^^^
* Sometimes the body expression has variables that are not named in the head. We
  call those variables free variables.
* Alpha equivalence does not apply to free variables.

* We call name bindings that are waiting for a
  value to be bound to **open bindings**. In
  other words, free variables are open
  bindings.
* An expression that contains no free
  variables is said to be closed.
* An enclosing function that closes open
  bindings by providing values for them, by
  introducing new parameters that correspond
  to the open names, for example, is called a
  closure.
* In the following example the single
  occurrence of ``x`` in the expression is
  bound by the second (outermost) lambda:
  ``(λx.y (λx.z x))``.


1.6 Multiple arguments
----------------------
* Each lambda can only bind one parameter and can only accept one argument.
* Functions that require multiple arguments actually have multiple nested heads.
* So, for example, ``λxy.xy`` is shorthand for ``λx.(λy.xy)``.
* This formulation was first discovered by Moses Schönfinkel, then later Haskell
  Curry, and is commonly called currying.

.. include:: exercises/1.6.1_-_intermission:_equivalence_exercises.rst


1.7 Evaluation is simplification
--------------------------------
* A **redex**, or reducible expression, refers to
  sub-terms that can be reduced by one of the reduction rules.
* The expression to which a redex reduces is called a **reduct**.
* There are multiple normal forms in the lambda calculus.
* Beta normal form is when you cannot beta reduce (apply lambdas to arguments)
  the terms any further.
* This corresponds to a fully evaluated expression, or in programming, a fully
  executed program.

.. order of evaluation:
..
.. * full beta reduction,
.. * applicative order (call-by-value)
..
..   * left to right
..   * fully evaluate arguments before function execution instance is created
..
.. * normal order
..
..   * the leftmost outermost redex is always
..     reduced first
..
.. * call by name
..
..   * evaluates arguments as needed
..
.. * call by need
..
..   * evaluate arguments as needed and store
..     for subsequent usages
..   * implemented in haskell

1.8 Combinators
---------------
* A combinator is a lambda term with no free variables. Combinators serve
  only to combine the arguments they're given.
* If a parameter exists, but is not used in the body, it may still be a
  combinator.
* Combinators are closed expressions.


1.9 Divergence
--------------
* Divergence means that the reduction process never terminates or ends.
* This matters in programming because terms that diverge don't produce an answer
  or meaningful result.


1.11 Chapter Exercises
----------------------
.. include:: exercises/1.11.1_-_combinators.rst

.. include:: exercises/1.11.2_-_normal_form_or_diverge.rst

.. include:: exercises/1.11.3_-_beta_reduce.rst
