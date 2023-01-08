(abstract) algebra

  A set of operations that are closed over a set of values.
  Those operations may have properties which are described
  using equational laws, such as associativity.

  Algebras are implemented as typeclasses in Haskell. The
  operations are contained within the typeclass. The values
  that those operations are closed over are the types that
  have an instance of that typeclass. Equational laws are
  not enforced by the language, but by programmer discipline
  instead.


abstract reduction system
  alphabet
  string
  rewrite (operation)
  (rewrite) rule
  unification

    Unification is an algorithmic process of solving equations between symbolic expressions.

    During unification, multiple expressions are rearranged by performing substitution until all of
    them resolve to a common form. If they can't resolve to a common form, they're not equivalent,
    and can't be unified.

    On #haskell, in regards to a discussion about type inference:

    **justsomeguy** What is unification?

    **c_wraith** the process of solving for type variables so that two types can be the same thing.

    **sshine** justsomeguy, so when **Just :: a -> Maybe a**, and you write **Just (5 :: Int)**,
    then **Int** and **a** are unified by setting **a = Int**, and the concrete **Just :: Int ->
    Maybe Int** is used.

  normalization
  confluence
  normal form


calculus

  calculus

    A calculus in its most general sense is any method or system of calculation.

    It is a formal system consisting of an alphabet, grammar, and derivation rules that is treated
    apart from intended interpretation. (Syntax can be ascribed, but it not part of the calculus.)
    This kind formulation deals with validity and satisfiability rather than truth or falsity.

  grammar
  well-formed
  reduction
  church-rosser theorem
  meta-theory
  object language
  meta language
  lambda calculus
    A lambda calculus is a formal system for expressing computations.


logic

  theorem

    A logical argument (proof) proven to be true.

  proof

    A logical argument.

  argument

  conclusion

  premise

  proposition

  inference

    A step in thinking; A logical operation.

  deduction

    https://iep.utm.edu/ded-ind/

    A deductive argument is an argument that an arguer puts forward as valid. For a valid argument,
    it is not possible for the premises to be true with the conclusion false.

  induction

    An inductive argument is an argument that an arguer puts forward as inductively strong. In an
    inductive argument, the premises are intended only to be so strong that, if they were true, then
    it would be unlikely, although possible [Ed; Think "physically possible"], that the conclusion
    is false. If the truth of the premises makes it unlikely (but not impossible) that the
    conclusion is false, then we may say that the argument is inductively strong. The following is
    an example of an inductively strong argument: 97% of the Republicans in town Z voted for McX,
    Jones is a Republican in town Z; therefore, Jones voted for McX.

    In an argument like this, an arguer often will conclude “Jones probably voted for McX” instead
    of “Jones voted for McX,” because they are signaling with the word “probably” that they intend
    to present an argument that is inductively strong but not valid.

  axiom

    A proposition that is not actually proved or demonstrated, but is considered to be self-evident
    and universally accepted as a starting point for deducing and inferring other truths and
    theorems, without any need of proof

  valid

    **A formula is valid if every assignment of values to its variables makes the formula true.**

    https://www.britannica.com/topic/validity

    validity, In logic, the property of an argument consisting in the fact that the truth of the
    premises logically guarantees the truth of the conclusion. Whenever the premises are true, the
    conclusion must be true, because of the form of the argument.

    Some arguments that fail to be valid are acceptable on grounds other than formal logic (e.g.,
    inductively strong arguments), and their conclusions are supported with less than logical necessity.

    Where the support yields high probability of the conclusion relative to the premises, such
    arguments are sometimes called inductively valid. In other purportedly persuasive arguments, the
    premises actually provide no rational grounds for accepting the conclusion; such defective forms
    of argument are called fallacies (see fallacy, formal and informal).

  sound

  well-defined (or single-valued)

  satisfiable

    A formula is satisfiable if it is true under some assignment of values to its variables.

  model

  theory


computability


complexity

  class of problems (computational complexity classes)


effective method, systematic method, mechanical method, effective procedure, systematic procedure, mechanical procedure


algorithm


automata

  automaton

  finite automaton

  context-free grammar


model of computation

   From https://www.wikiwand.com/en/Model_of_computation

   A model of computation is a model which describes how an
   output of a mathematical function is computed given an
   input.

   A model describes how units of computations, memories,
   and communications are organized.[1]

   The computational complexity of an algorithm can be
   measured given a model of computation.

   Using a model allows studying the performance of
   algorithms independently of the variations that are
   specific to particular implementations and specific
   technology.

   From these slides:
   https://ptolemy.berkeley.edu/projects/embedded/research/hsc/class/ee249/lectures/l3-IntroductionToMoC.pdf

   A mathematical description that has a syntax and rules
   for computation of the behavior described by the syntax
   (semantics).

   Used to specify the semantics of computation and
   concurrency.

   Formalization of design + environment = closed system of
   equations and inequalities over some algebra.


semantics

  denotational: in terms of mathematical relations

  operational: in terms of actions taken by some abstract machine


referential transparency


relation


function
  computable function
  recursive function
  partial function
    domain of definition
  total function
  domain
  range
  codomain
  preimage
  image


equality
  intensional equality
  extensional equality
  injective
  bijective
  surjective


set
  element or member
  subset
  proper subset
  multiset
  infinite set
  countably infinite set
  uncountably infinite set
  partially ordered set (poset)
  empty set
  singleton set
  unordered pair
  union
  intersection
  complement
  power set
  Cartesian product or cross product


sequence
  tuples
  k-tuple
  ordered pair
