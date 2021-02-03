*********************
 Chapter 14: Testing
*********************
"Program testing can be used to show the presence of bugs,
but never to show their absence!" ~ Edsger W. Dijkstra

"Beware of bugs in the above code; I have only proved it
correct, not tried it." ~ Donald Knuth

.. https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
.. https://www.researchgate.net/publication/2449938_QuickCheck_A_Lightweight_Tool_for_Random_Testing_of_Haskell_Programs
.. https://github.com/nick8325/quickcheck
.. https://dl.acm.org/doi/10.1145/3241625.2976017 (QuickFuzz paper)
.. Hypothesis, a testing library for Python that is heavily
   inspired by QuickCheck: https://hypothesis.works/articles/what-is-hypothesis/
.. https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

14.1 Testing
------------
Although Haskell emphasizes program correctness by
construction[1], no amount of inductive reasoning is as
convincing to stakeholders as exercising your code for its
intended usage.

[1]: To get a sense of what I mean, see "Program = Proof",
by Samuel Mimram.

Worse yet, without constant communication with the customer
and empirical testing, you may find that you've created a
perfectly consistent formal model of something that doesn't
perform tasks the customer intended you to automate.

Automated tests are a limited form of empirical testing --
executable sanity checks -- and an indispensable tool for
working programmers.

Really, we need all three -- constant communication with the
customer, empirical testing, and proofs of correctness
(using the type system, model checkers, etc).

This chapter of the book is pretty narrowly focused on
covering how to use Hspec and QuickCheck to write tests in
Haskell. There is also a long example program at the end of
the chapter that demonstrates using these libraries to test
a Morse code translator.

Since I'm new to testing, I've tried to include exposition
on basic concepts to fill in the gaps. Mostly this takes the
form of paraphrases from different articles on the web.

I'm assuming one day I'll look back at these notes and shake
my head in disapproval at how wrong I've gotten everything;
But for now I view this extra commentary as a useful dialog
with myself to explore the topic of testing.

If you found these notes from a web search, don't take them
too seriously. I'm only a beginner.


14.2 A quick tour of testing for the uninitiated
------------------------------------------------
Whenever you load your code into GHCi to play with a
function you wrote, you're testing your code; you're just
doing it manually.

In general, automated tests allow you to state an
expectation and then verify that the result of an operation
meets that expectation. Just like experimenting in the REPL,
tests allow you to verify that you code will do what you
want when executed.

There are multiple categories of automated tests,
categorized roughly by what they are intended to test.

If you look up "software testing basics" with a search
engine, you may find yourself looking at an overwhelming
listicle of over a thousand terms for arbitrary
categorizations of kinds of tests or testing methodologies.

Essentially, though, tests are just code, and the
categorization doesn't matter. Simply keep in mind the end
goal -- you want to prove that your software works.

To do that you'll want to test that the intent of the
program matches the tasks it can perform. You should also
test that the entire program works. Along the way, you'll
probably have to test individual components of your program
to make sure they work, too.

The last of these is called *unit testing*. Unit tests
exercise the smallest atomic units of software independently
of one another to make sure they work in isolation. In a
unit test, usually you write the function applied to a
particular input, and test that it produces a particular
expected output. Hspec is the tool we'll use in this chapter
to write unit tests.

As a beginner writing his own code, this is the first type
of test I've encountered. Perhaps if I were to contribute to
someone else's project, I'd have encounter an end-to-end
test first.

Another useful tool that Haskell programmers use is called
property testing. The general idea is to generate random
inputs that are checked against a test function, known as a
property, to see if it holds.

Here is a sample of a property test, to show what I mean::

  import Test.QuickCheck

  prop_reverseReverse :: Eq a => [a] -> Bool
  prop_reverseReverse xs = reverse (reverse xs) == xs

  main = quickCheck prop_reverseReverse

When ``main`` is run, ``prop_reverseReverse`` will be fed
random inputs of type ``Eq a => [a]`` to see if our
condition still returns ``True``.

If unit testing is essentially automating manual tests, then
property testing is automating unit tests.

``QuickCheck`` is the package the provides property testing
in Haskell. It relies on your functions type signature to
know what kinds of input to generate. The default setting is
for 100 inputs to be generated, giving you 100 results.

``QuickCheck`` is cleverly written to be as thorough as
possible and will usually check the most common edge cases
(for example empty lists and the ``maxBound`` and
``minBound`` of the types in question).

If the function being tested fails any of these tests, we
know the function doesn't have the specified property. On
the other hand, you can't be positive that it will never
fail because the data are randomly generated.

Property testing is useful for getting a strong indication
that you've met the minimum requirements to satisfy laws,
such as the laws of monads or basic associativity.

.. topic:: Other testing tools

   Hspec and QuickCheck are only the tip of the testing
   iceberg. Check out ``SmallCheck``, a library for testing
   *every* possible input of a given type, and ``Tasty``, a
   unit testing framework that integrates multiple other
   tools. `Diehl has a brief description of some of these
   here <http://dev.stephendiehl.com/hask/#testing>`_.


14.3 Conventional testing
-------------------------
First, let's set up a project that we'll put our tests into.

::

  $ mkdir addition && cd addition

  $ cat > addition.cabal << EOF
  name: addition
  version: 0.1.0.0
  author: Chicken Little
  maintainer: sky@isfalling.org
  category: Text
  build-type: Simple
  cabal-version: >=1.10

  library
    exposed-modules: Addition
    ghc-options: -Wall -fwarn-tabs
    build-depends: base >= 4.7 && <5, hspec
    hs-source-dirs: .
    default-language: Haskell2010

  EOF

  $ stack init

  $ stack build

Now that we've made the project skeleton, and stack can
build it, we'll enter ghci, and then test that the functions
from ``Addition`` are in scope.

::

  $ stack ghci

  Configuring GHCi with the following packages: addition
  GHCi, version 8.10.3: https://www.haskell.org/ghc/ :? for help
  Loaded GHCi configuration from /home/chris/.ghci
  [1 of 1] Compiling Addition   ( Addition.hs, interpreted )
  Ok, one module loaded.
  Loaded GHCi configuration from
  /tmp/haskell-stack-ghci/1506c361/ ghci-script

  ·∾ sayHello
  hello!

14.3.1 Truth according to Hspec
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Let's experiment with writing unit tests with Hspec.

To do so, we'll need to make the package available to our
project.  Add the ``hspec`` package to ``build-depends`` in
your cabal file.

Now we must bring it into scope. Import the module
``Test.Hspec`` in ``Addition.hs`` so we can use it.

14.3.2 Our first Hspec test
^^^^^^^^^^^^^^^^^^^^^^^^^^^
Here is a simple example of a unit test using Hspec:

.. include:: projects/addition/Addition.hs
   :code:

.. include:: exercises/14.3.3_-_intermission_short_exercise.rst


14.4 Enter QuickCheck
---------------------
There are two ways to run a QuickCheck test described in
this book.

The first is by using Hspec in combination with QuickCheck,
like this::

  import Test.Hspec
  import Test.QuickCheck

  main :: IO ()
  main = hspec $ do
    describe "Addition" $ do
      it "x + 1 is always greater than x" $ do
        property $ \x -> x + 1 > (x :: Int)

Another way to run QuickCheck tests is to use the facilities
provided by the ``Test.QuickCheck`` module, like this::

  import Test.QuickCheck

  preprocess s = filter isAlpha (map toLower s)

  isPalindrome :: String -> Bool
  isPalindrome s = (preprocess s) == reverse (preprocess s)

  prop_punctuationInvariant text =
    preprocess text == preprocess noPuncText
    where noPuncText = filter (not . isPunctuation) text

  main = quickCheck prop_punctuationInvariant

Qualifying test with a precondition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
QuickCheck also provides something called **conditional
properties**. Using the ``(==>)`` function we can filter out
inputs from being tested based on some precondition.

This has the general form *condition* **==>** *property*.

Here is a simple example of its use::

  import Test.QuickCheck

  qsort :: [Int] -> [Int]
  qsort []     = []
  qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
      where lhs = filter  (< x) xs
            rhs = filter (>= x) xs

  prop_maximum ::  [Int] -> Property
  prop_maximum xs = not (null xs) ==>
                    last (qsort xs) == maximum xs


  main :: IO ()
  main = quickCheck prop_maximum

In this function, only values of type ``[Int]`` that are not
empty lists (null) are permitted as inputs for our test.

Specifying an test input generator explicitly
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Properties may take the general form:

  **forAll** *generator* **$** **\\**\ *pattern* **->** *property*

For example::

  prop_Insert2 x = forAll orderedList $ \xs -> ordered (insert x xs)
    where types = x::Int

The first argument of ``forAll`` is a test data generator;
by supplying a custom generator, instead of using the
default generator for that type, it is possible to control
the distribution of test data.

14.4.1 Arbitrary instances
^^^^^^^^^^^^^^^^^^^^^^^^^^
The tricky part of QuickCheck is generating the input values
to test on. All types that QuickCheck can automatically test
must be an instance of the type class ``Arbitrary``.

The bad news is that only a few base types are instances of
``Arbitrary``. The good news is that you can install package
that greatly extends the types covered by QuickCheck, named
``quickcheck-instances``.

If you want to write an instance of ``Arbitrary`` for your
own custom type, the chapter touches on that. But it's way
over my head to be honest.

One thing that may be useful is knowing how to print sample
values of some type that QuickCheck can generate.

We can use the ``sample`` function for this, in combination
with an overloaded expression from the ``Arbitrary`` type
class named ``arbitrary``::

  ·∾ sample (arbitrary :: Gen Int)
  0
  2
  -4
  5
  2
  3
  -10
  -1
  3
  12
  -2

  ·∾ sample (arbitrary :: Gen Double)
  0.0
  0.6341225463105274
  -0.5399666722390497
  3.6986851136506376
  4.927328536143319
  -0.34216302388027836
  4.401389073625471
  5.706335581327833
  14.466727278626447
  -0.5275031627254437
  -8.811337993125159

As you can see, the ``sample`` function has produced some
random sample data of the types we've specified (``Gen Int``
and ``Gen Double`` respectively). The ``sample`` function is
what has introduced randomness here, ``arbitrary`` by itself
is not random.

Knowing what you do about referential transparency, you may
be wondering how these functions produce random data. After
all, the definition of a pure function is that it always
produces the same results given the same input.

The answer is apparent if you examine the type signature of
``sample``::

  ·∾ :type sample
  sample :: Show a => Gen a -> IO ()

It turns out, ``sample`` is not a pure function, but an IO
action. It needs to be, so it can ingest a source of
randomness.

In this section, a few other means of generating sample data
are demonstrated. This includes the ``elements``,
``frequency``, and ``choose`` functions.

At this point, it's not clear to me how I may use them in my
own programs, or why it's being discussed.


14.5 Morse code
---------------
.. TODO Pick this up tomorrow!

14.5.1 The Main event
^^^^^^^^^^^^^^^^^^^^^
