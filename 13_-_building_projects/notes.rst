*******************************
 Chapter 13: Building Projects
*******************************


13.1 Modules
------------
In this chapter, we'll cover:

  * writing Haskell programs with modules;
  * using the Cabal package manager;
  * building our project with Stack;
  * conventions around project organization;
  * building a small interactive game.

In order to stay organized, I'll try to track keep
the projects in their own branch off of Ch13, and
then merge them back when I'm done. I'll attempt
to note the section and page number for each
commit in the footer of the commit message.


13.2 Making packages with Stack
-------------------------------
* https://docs.haskellstack.org/en/stable/stack_yaml_vs_cabal_package_file/
* https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/

.. pull-quote::

   Before using stack, there are a few things every programmer should know:

   stack is not a package manager, it is a build tool. It does not manage a set of
   “installed” packages; it simply builds targets and their dependencies.

   The command to build a target is stack build <target>. Just using stack build on
   its own will build the current project’s targets.

   You almost certainly do not want to use stack install. stack install is not
   like npm install. stack install is like make install. stack install copies
   executables into a global location by design.

* Cabal is a package manager.
* Stack, in contrast, manages projects, which may be comprised of multiple packages (or a just one).
* Stack is like a unified interface to all the tools necessary to manage a project.

  * Basically, stack is like poetry or cargo or npm or poetry or...
  * Stacks primary purpose is to enable reproducible builds, which means that
    building the project will work the same way today that it does five years
    from now.
  * Making builds reproducible requires keeping the project state and toolchain
    isolated from the global state of the system.
  * Stack can do things like create a directory structure according to some
    predefined project template, run tests, build code, set up docker containers
    as build environments, and run performance analysis tools.
  * You can find Stacks documentation here https://docs.haskellstack.org.

* On the back-end, Stack uses Cabal to do package management.
* Hackage is the Haskell communities main repository for packages published with Cabal.
* Stackage is the package repository that Stack uses by default.
* Stackage curates generations of packages from Hackage into *snapshots* which
  are then tested to ensure its constituent packages work together.
* The snapshot used for your project is recorded by stack in ``stack.yaml`` and
  future package manager operations resolve against that snapshot.


13.3 Working with a basic project
---------------------------------
::

  git clone https://github.com/haskellbook/hello
  cd hello
  stack build # try to build
  stack setup # grab a compiler if missing
  stack ghci # run ghci within the projects environment, bring Main into scope
  hello # should fail, hello is not in $PATH
  stack exec -- hello # stack knows where hello is located

13.4 Making our project a library
---------------------------------
* A single package can have multiple executables and libraries.


13.5 Module exports
-------------------
* By default, when you don't specify any exports in a module, every top-level
  binding is exported and can be imported by another module.
* It may be a good idea to explicitly specify which modules should be exported
  in some situations.
* The syntax for that is ``module Hello ( sayHello ) where ...`` which will
  expose only ``sayHello``, and keep other top-level bindings private.

Exposing modules
^^^^^^^^^^^^^^^^
* Don't forget to update the ``exposed-modules`` directive under the ``library``
  stanza in ``hello.cabal``.
* You have to not only make sure the function is exported within the module,
  but also that cabal/stack is aware of the library.


13.6 More on importing modules
------------------------------
* Imported modules are top-level declarations. Like other top-level declarations
  they have scope throughout the module. Their ordering doesn't matter. Import
  declarations are cumulative.
* To start ghci with an empty namespace ``stack ghci --ghci-options -XNoImplicitPrelude``
* Remember that you can use ``:m`` to reset the loaded modules that are in
  scope. ``:module +|- *mod1 ... *modn``
* Examples of import syntax, from the 2010 language report 5.3.4

  +--------------------------------------+------------------------------------+
  |    Import declaration                |      Names brought into scope      |
  +======================================+====================================+
  |  ``import A``                        |    ``x``, ``y``, ``A.x``, ``A.y``  |
  +--------------------------------------+------------------------------------+
  |  ``import A ()``                     |              nothing               |
  +--------------------------------------+------------------------------------+
  |  ``import A (x)``                    |    ``x``, ``A.x``                  |
  +--------------------------------------+------------------------------------+
  |  ``import qualified A``              |    ``A.x``, ``A.y``                |
  +--------------------------------------+------------------------------------+
  |  ``import qualified A ()``           |              nothing               |
  +--------------------------------------+------------------------------------+
  |  ``import qualified A (x)``          |    ``A.x``                         |
  +--------------------------------------+------------------------------------+
  |  ``import A hiding ()``              |    ``x``, ``y``, ``A.x``, ``A.y``  |
  +--------------------------------------+------------------------------------+
  |  ``import A hiding (x)``             |    ``y``, ``A.y``                  |
  +--------------------------------------+------------------------------------+
  |  ``import qualified A hiding ()``    |    ``A.x``, ``A.y``                |
  +--------------------------------------+------------------------------------+
  |  ``import qualified A hiding (x)``   |    ``A.y``                         |
  +--------------------------------------+------------------------------------+
  |  ``import A as B``                   |    ``x``, ``y``, ``B.x``, ``B.y``  |
  +--------------------------------------+------------------------------------+
  |  ``import A as B (x)``               |    ``x``, ``B.x``                  |
  +--------------------------------------+------------------------------------+
  |  ``import qualified A as B``         |    ``B.x``, ``B.y``                |
  +--------------------------------------+------------------------------------+

* If you replace the keyword ``import`` with ``module`` and the phrase "brought
  into scope" with "made available for export", then the table illustrates how
  exports work.
* Instance declarations are not explicitly named in import or export lists.
  Every module exports all of its instance declarations and every import brings
  all instance declarations into scope. 
* But how do multi-level imports work?
* Also what paths does GHC search when looking for a module name?

  * GHC will either search the location specified with the ``-i`` option, or it
    will search the current directory, and then search ``$GHC_PACKAGE_PATH`` for
    files containing package databases, and finally ``$PATH``.
  * If ``$GHC_PACKAGE_PATH`` does not end in a ``:``, it overrides ``$PATH``.

* ::

    justsomeguy  Does the syntax "import Data.List.NonEmpty (NonEmpty(..))" import
                 all the functions related to the NonEmpty datatype? What does the
                 "(..)" part mean?
    merijn       justsomeguy: The constructors
    merijn       justsomeguy: So for example "import Data.Maybe (Maybe)" imports
                 *only* the type, Maybe, but not the constructors Just/Nothing
    merijn       justsomeguy: You can use "import Data.Maybe (Maybe(Nothing,Just))"
                 or any subset you like (both for exports and imports) (..) is just
                 short hand for "all of them"


13.7 Making our program interactive
-----------------------------------
* The ``<-`` operator is pronounced "bind". It does assignment and unwraps
  the monad burrito.
* ``hSetBuffering stdout NoBuffering``

What if we tried to pass getLine to sayHello?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* It won't work. You need da arrow to unwrap the monad burrito. Why don't you
  know that? Do you even unlift Strings, bro?
* Honestly, I don't know how the bind operator works, but here is some
  experimentation in GHCi that I think is illustrative.::

    ∾ :type getLine
    getLine :: IO String

    ∾ x <- getLine
    Have a line.
    ∾ :type x
    x :: String

    ∾ let z = getLine
    ∾ :type z
    z :: IO String
    ∾ z
    Have a line!
    "Have a line!"
    ∾ :type z
    z :: IO String
    ∾ -- seems like the line was discarded


Adding a prompt
^^^^^^^^^^^^^^^
* We added ``hSetBuffering stdout NoBuffering`` to make stdout display
  immediately, instead of buffering by line, which is the default.
* This allows us to display the prompt ``putStr "Please input your name: "``
  before input is requested.


13.8 do syntax and IO
---------------------
* ``do`` blocks are syntactic sugar that allows for sequencing monadic actions.
* Using ``do`` kind of feels like doing imperative programming in Haskell.
* The ``main`` function within the module ``Main`` must always have the type ``IO ()``.
* The ``<-`` function, pronounced bind, does assignment and also takes the
  assigned value out of a monadic context. From ``m a`` to ``a``.
* The ``return`` function returns a value inside a monadic structure.


13.9 Hangman game
-----------------
::

  stack new hangman simple
  cd hangman
  url="https://gist.githubusercontent.com/wchargin/8927565/raw/d9783627c731268fb2935a731a618aa8e95cf465/words"
  curl "$url" | LC_COLLATE=C grep -E '^[a-z]+$' > data/dict.txt
  tree --dirsfirst
  # Edit hangman.cabal and add the "random" and "split" packages to "build-depends:".


13.10 Step One: Importing modules
---------------------------------
* This section has you type in imports for Main and explains what the imported
  functions do.


13.11 Step Two: Generating a word list
--------------------------------------

