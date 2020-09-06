***********************************
 Chapter 1: All You Need is Lambda
***********************************


1.1 All you need is lambda
--------------------------

What is a calculus?
^^^^^^^^^^^^^^^^^^^
* A calculus is a notation used to formalize a method of calculation or
  reasoning.
* Calculus' have a set of axioms (fundamental assumptions), and a set of
  derivation rules (data transformation rules).
* With them, you can use the calculus to prove something.
* You can think of a calculus as a miniature language for reasoning, or
  maybe a game.

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

How does this apply to Haskell?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* Haskell's regular language syntax reduces to a subset of the language
  called the language kernel.
* The language kernel reduces to the core type, which is an implementation of a
  typed lambda calculus called system fc.
* That goes through several eventually turns into a build artifact you can
  run on your computer.
* Also, the evaluation strategy used by Haskell resembles the lambda calculus.


1.2 What is functional programming?
-----------------------------------
* Functional programming is a computer programming paradigm that relies on
  functions that behave like mathematical functions.
* The essence of functional programming is that programs are a combination of
  expressions.
* Expressions include concrete values, variables, and also functions.
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
* (The paper "What is a Purely Functional Language?" by Amr Saby claims that
  purity can be determined by (weak) equivalence of call-by-name, call-by-value,
  and call-by-need evaluation strategies.)
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
* There are multiple normal forms in the lambda calculus.
* Beta normal form is when you cannot beta reduce (apply lambdas to arguments)
  the terms any further.
* This corresponds to a fully evaluated expression, or in programming, a fully
  executed program.


1.8 Combinators
---------------
* A combinator is a lambda term with no free variables. These expressions serve
  only to combine the arguments they're given.


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

