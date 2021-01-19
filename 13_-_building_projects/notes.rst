*******************************
 Chapter 13: Building Projects
*******************************


13.1 Modules
------------
In this chapter we're building a small interactive
hangman-style game. The chapter's primary focus is not so
much on code but on how to set up a project. There are a
few times we ask you to implement part of the hangman game
yourself, but much of the code is already written for you.

In this chapter, we'll cover:

  * writing Haskell programs with modules;
  * using the Cabal package manager;
  * building our project with Stack;
  * conventions around project organization;
  * building a small interactive game.

In order to stay organized, I'll try to track keep the
projects in their own branch off of ``Ch13``, and then merge
them back when I'm done. I'll attempt to note the section
and page number for each commit in the footer of the commit
message.


13.2 Making packages with Stack
-------------------------------
* https://docs.haskellstack.org/en/stable/stack_yaml_vs_cabal_package_file/
* https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/

.. pull-quote::

   Before using stack, there are a few things every
   programmer should know:

   stack is not a package manager, it is a build tool. It
   does not manage a set of “installed” packages; it simply
   builds targets and their dependencies.

   The command to build a target is stack build <target>.
   Just using stack build on its own will build the current
   project’s targets.

   You almost certainly do not want to use stack install.
   stack install is not like npm install. stack install is
   like make install. stack install copies executables into
   a global location by design.

Cabal is a package manager. Stack, in contrast, manages
projects, which may be comprised of multiple packages (or a
just one).

Stack is like a unified interface to all the tools necessary
to manage a project.

  * Basically, stack is like poetry or cargo or npm or
    poetry or...
  * Stacks primary purpose is to enable reproducible builds,
    which means that building the project will work the same
    way today that it does five years from now.
  * Making builds reproducible requires keeping the project
    state and toolchain isolated from the global state of
    the system.
  * Stack can do things like create a directory structure
    according to some predefined project template, run
    tests, build code, set up docker containers as build
    environments, and run performance analysis tools.
  * You can find Stacks documentation here
    https://docs.haskellstack.org.

On the back-end, Stack uses Cabal to do package management.
The packages you use are stored in repositories.

Hackage is the Haskell communities main repository for
packages published with Cabal.

Stackage is the package repository that Stack uses by
default. Stackage curates generations of packages from
Hackage into *snapshots* which are then tested to ensure its
constituent packages work together.

The snapshot used for your project is recorded by stack in
``stack.yaml`` and future package manager operations resolve
against that snapshot.


13.3 to 13.8
------------
These sections outline setting up the ``hello`` project.
Instead of making notes for this, I've created a branch,
``13-hello``, that has detailed commit messages that you
can peruse with ``git reflog ch13-hello``.


13.6 More on importing modules
------------------------------
Imported modules are top-level declarations. Like other
top-level declarations they have scope throughout the
module. Their ordering doesn't matter. Import declarations
are cumulative.

To start ghci with an empty namespace use ``stack ghci
--ghci-options -XNoImplicitPrelude``

Remember that you can use ``:m`` to reset the loaded modules
that are in scope. ``:module +|- *mod1 ... *modn``

Examples of import syntax, from the 2010 language report 5.3.4

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

If you replace the keyword ``import`` with ``module`` and
the phrase "brought into scope" with "made available for
export", then this table also illustrates how exports work.

Instance declarations are not explicitly named in import or
export lists. Every module exports all of its instance
declarations and every import brings all instance
declarations into scope.

But how do multi-level imports work? Well, it's really all
one namespace, but a module may choose to re-export another
module.

::

  module Queue ( module Stack, enqueue, dequeue ) where
    import Stack
    . . .

Also what paths does GHC search when looking for a module
name?

  * GHC will either search the location specified with the
    ``-i`` option, or it will search the current directory,
    and then search ``$GHC_PACKAGE_PATH`` for files
    containing package databases, and finally ``$PATH``.
  * If ``$GHC_PACKAGE_PATH`` does not end in a ``:``, it
    overrides ``$PATH``.
  * This is one reason that it's a bad idea to have the
    ``:`` character in your project directory names. Stack
    will become confused.

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


13.9 to 13.13
-------------
Since my Linux distro doesn't come with a words file,
though, here is some shell to download one. This should get
you started on the first three paragraphs of section 13.9.

::

  $ stack new hangman simple && cd hangman && mkdir data

  $ url='https://gist.githubusercontent.com/\
  wchargin/8927565/raw/d9783627c731268fb29\
  35a731a618aa8e95cf465/words'

  $ curl "$url" | LC_COLLATE=C grep -E '^[a-z]+$' > data/dict.txt

As before, further notes on this project are omitted in
favor of a git history on the ``ch13-hangman`` branch.

