********************************
 Chapter 19: Applying Structure
********************************


19.1 Applied structure
----------------------
Consider this chapter as a breezy survey of
how Haskeller's write code when they think no
one is looking. The code demonstrated will not
always include all necessary context to make
it run, do don't expect to be able to load the
snippets in GHCi and have them work. If you
don't have a lot of previous programming
experience and some of the applications are
difficult for you to follow, you might prefer
to return to this chapter at a later time.


19.2 Monoid
-----------

19.2.1 Templating content in Scotty
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. include:: figures/19.2.1/GreatScotty.hs
   :code:

This example left me with a lot of questions,
like:

* What does this script do?
* What is a web framework?
* How do I send messages to the web server running on port
  3000? (``echo hi | nc localhost 3000`` doesn't work)
* How do I view the output? (going to localhost:3000 in
  firefox doesn't work)
* What is Warp?
* What is WAI?
* What is a *route*?

Ok, apparently, I can browse to ``localhost:3000/keyword``,
and the web server will respond with an html page saying
``Scotty, keyword me up!``.
