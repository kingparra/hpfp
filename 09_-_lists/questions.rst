Why does Haskell use lists so much, even when they seem inappropriate, sometimes?

https://www.imn.htwk-leipzig.de/~waldmann/etc/untutorial/list-or-not-list/

**How are updates to lists handled with regards to referential transparency?**

.. Check Intro to FP through LC, Chapter 1 for some hints.

..
	1.5 Data structures in functional languages

	In FP, because there is no assignment,
	substructures in data structure cannot be
	changed one at a time. Instead, it is
	necessary to write down a whole structure
	with explicit change to the appropriate
	substructure.

	 **Functional languages provide explicit
	representations for data structres.**

	Functional languages do not provide arrays
	because without assignment there is no easy
	way to access an arbitrary element. Writing
	out an entire array with a change to one
	element would be ludicrously unwieldy.
	Instead, nested data structures like lists
	are provided.

Under which circumstances are lists slow?

What is a good rule of thumb for picking the right collection type - such as Array, Map, Set, Sequence, finger trees, tuples, or the list type?

Should I decide based on the time complexity of operations I need available?

How can I make other collection types easier to use? Not having literal notation for some of these things sucks! I also don't like having to do qualified imports.

Is "infinite sequence" the right mathematical term for an infinite lists?

..
   I asked whether "sequence" is the right word for this on the ##math channel of libera.chat

   13:57 <justsomeguy> What is the correct term for an infinite ordered collection of
   elements, where each element is related to prior elements by a function? I'm a bit confused
   on the difference between the idea of series, sequence, and progressions. Or even if any of
   those captures what I'm thinking about. Also where the elements of the set can repeat (they
   don't need to be unique).

   13:58 <cheater> what do you know about the function?
   13:59 <cheater> can you know the nth element before you know the elements 1...n-1?
   13:59 <cheater> or is the nth element always dependent on the previous elements?
   14:00 <cheater> justsomeguy

   14:00 <justsomeguy> The nth element always depends on the value of previous elements.

   14:00 <dude12312414> i think "sequence" is an appropriate term for that

   14:02 <justsomeguy> And, also, it's impossible to know a future element without knowing prior elements.

   14:02 <dude12312414> you could also say recurrence relation, though that's more about the
   equation defining the terms in terms of previous ones
   14:02 <dude12312414> sequence defined by a recurrence relation

   14:04 <justsomeguy> Thank you, that helps. I also just found this page
   https://www.embibe.com/exams/chapter/sequences-and-series-3/ which clears things up a bit.
   14:05 <justsomeguy> The original text I'm editing called it a series, which to my
   understanding is limited to sumnation.

   14:07 <Z-module> justsomeguy: YOu mean in the same ordering as 1, 2, 3, ....  ?
   14:09 <Z-module> ANY sequence  x_1, x_2, x_3, ...  of let's say real numbers is just an
   "infinite sequence". Whether there's some simple rule to calculate each x_n based on just
   the earlier terms isn't relevant to a name here. You might be thinking of the "axiom of
   dependent choices", though.
   14:10 <Z-module> and yes, some stuff does sometimes refer to these as "series", but it's
   not an infinite series as used in analysis.

Do lists in Haskell confuse the idea of an iterator (that allows one to traverse data) and its underlying collection (that holds the actual data)?

What is GHCs "list deforestation" compiler optimization? What is GHCs "list fusion" compiler optimization?

https://wiki.haskell.org/GHC_optimisations

Is it possible to add invariants to lists that are known statically (such as the length, whether it is a finite or infinite list, etc)?

What sections did I find most valuable?

Which sections didn't I get anything out of?

What surprised me in the chapter?

Which sections were straight-forward, and which were not?

Why did Chris decide to discuss the cardinality of the list type
in section 9.2? How is it relevant to the learning objectives?
What does knowing this allow us to do that we couldn't before?

  * Cardinality of [a]: ``1 + a * a^a``.

  * https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types

How are lists in other languages different from Haskell?

  * https://www2.cs.arizona.edu/~collberg/Teaching/372/2005/Html/Html-6/

  * compare and contrast arrays vs singly-linked list datatype representations operationally

Which sections or points seemed like they were intended for
complete beginners, and which points seem to be intended for
programmers with experience in other languages?

What is a list?

  A list is a structure that is used to arrange elements in a linear order.

  Within a list, there can be multiple occurrences of any element.

  Every member of a list must have the same type.

  Each element is attached to a cons cell within the list. Cons
  cells are represented as the cons data constructor which has
  two arguments: the elements value, and a link to the next cons
  data constructor or end-of-list data constructor.

  In order to get to the :math:`{n}th` element of a list, each
  cons cell before it must be traversed.

  The elements within the cons cells do not have to be evaluated
  along the way, though.
