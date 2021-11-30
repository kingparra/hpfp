Chapter 14 Questions
********************


Hspec functions list
--------------------

Basic functions
^^^^^^^^^^^^^^^
* ``describe``
* ``context``
* ``it``
* ``shouldBe``

Hooks
^^^^^
* ``before_`` ::

    main = hspec $ before_ flushDb $ do
      . . .

* ``after_``
* ``around_``

Placeholders
^^^^^^^^^^^^
* ``pending``
* ``pendingWith``


QuickCheck Questions
--------------------

How can I run a particular Hspec test with stack?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
`This page <https://hspec.github.io/options.html>`_ spells it out::

  stack test --test-arguments='--match "some spec item"'


Is there some special reason that QuickCheck property tests begin with ``prop_``, such as automated test discovery?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
There may be. QuickCheck provides a function named
``quickCheckAll`` that uses ``TemplateHaskell`` to discover
properties and then test all of them.
