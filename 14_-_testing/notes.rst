*********************
 Chapter 14: Testing
*********************
"Program testing can be used to show the presence of bugs,
but never to show their absence!" ~ Edsger W. Dijkstra

"Beware of bugs in the above code; I have only proved it
correct, not tried it." ~ Donald Knuth


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

This chapter will cover:

* the whats and whys of testing;
* using the testing libraries ``Hspec`` and ``QuickCheck``;
* a bit of fun with Morse code.


14.2 A quick tour of testing for the uninitiated
------------------------------------------------
It's possible to write well-typed code that doesn't perform
as expected, and runtime errors can still occur. That's
where testing comes in.

In general, tests allow you to state an expectation and then
verify that the result of an operation meets that
expectation. The allow you to verify that you code will do
what you want when executed.

Unit testing test the smallest atomic units of software
independently of one another.

One limitation to unit (and spec) testing is that they test
atomic units of code independently, so they don't verify
that all the pieces work together properly.

Property testing is a different beast. In property testing
inputs are generated randomly by functions provided by
``QuickCheck``, and checked against a test function, known
as a property, to see if it holds.

``QuickCheck`` relies on your functions type signature to
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

Property testing is useful for determining that you've met
the minimum requirements to satisfy laws, such as the laws
of monads or basic associativity.


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

14.3.1 Truth according to ``Hspec``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This section has you add ``import Test.Hspec`` to your
``Addition`` module, and explains some possible ways things
may go wrong. This includes forgetting to add the ``hspec``
package to ``build-depends`` in our cabal file, or putting
the import declaration in the wrong place in our file (which
results in a syntax error).

14.3.2 Our first ``Hspec`` test
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. include:: projects/addition/Addition.hs
   :code:

.. include:: exercises/14.3.3_-_intermission_short_exercise.rst


14.4 Enter QuickCheck
---------------------
Add ``QuickCheck`` to ``build-depends``, and then add this
test to your ``Addition.hs`` file.

::

  it "x+1 is always greater than x" $ do
    property (\x -> x+1 > (x :: Int))

14.4.1 Arbitrary instances
^^^^^^^^^^^^^^^^^^^^^^^^^^
``QuickCheck`` relies on a type class called ``Arbitrary``
and a newtype called ``Gen`` to generate its random data.

Why would I need to know how QuickCheck generates it's
random data in the first place? I know almost nothing about
QuickCheck at this point, and you're launching into a
discussion of its mechanics already? You've really lost me
here. I think I'm going to read a tutorial online, instead.
