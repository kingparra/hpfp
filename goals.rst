************************
 Goals for each chapter
************************
Here are some tasks I came up with as a
result of facing two problems while reading
this book.

The first problem is following a line of
reasoning in the face of frequent interruptions.

The second problem is coming up with a way
to prove that I really understand what I've
read.

Here's a general outline of the layout of a
chapter::

  exercises/
  figures/
  resources/

  chapter-text.rst  -- Source text, all paragraphs and sentences are numbered
  interrogation.rst -- Detailed analysis of the source text

  checklist.rst     -- Checklist of exercises
  contents.rst      -- ToC at a high level

  notes.rst
  outline.rst       -- A granular outline of the chapters structure
  questions.rst

Each file listed above is related to one of
the following goals or activities.

* First, create an outline of the chapter
  that is granular enough to enumerate every
  section, subsection, paragraph, question,
  and figure. (``outline.rst``) I use
  this to refresh my memory quickly so I can
  return to a section after being
  interrupted. Paragraphs get a one-sentence
  summary of what the subject is. I may add
  a note with the number of paragraphs in a
  section, or how many pages it spans. This
  is useful to plan out how long it will
  take to read a particular section, and
  where to look for a particular subject
  within the chapter.

* Format each figure nicely and store it in
  ``figures/``. This helps me stay organized
  when reading a long passage that is mostly
  examples, or when writing a script that
  uses them.

* Complete every programming exercise.

  * Write a full test suite for each exercise
    using ``HSpec`` and ``QuickCheck``. This
    helps me prove that I understand the
    subject.

* Record ``asciinema`` terminal sessions of
  each ``ghci`` example in the chapter. If
  it's a long terminal session, script it
  with ``expect``. This helps me prove that
  I've tried the examples myself, to
  experience what really happens when I run
  the code, and experiment with variations.

* Compile general questions about the
  chapter, and attempt to answer them. This
  lives in ``questions.rst``.  Questions that
  are "out of scope" of the chapter, but
  interesting and related in some way go
  here.

* For difficult to understand passage, number
  the paragraphs and sentences in
  ``chapter-text.rst`` so I can ask questions
  about the passage at the sentence level, in
  different orders than the source text, and
  also contrast different sentences. These
  detailed questions and analysis live in
  ``interrogation.rst``. I only do this when
  needed, it's a lot of effort.

* Write a cheat sheet of the main points of
  each section, and provide concise
  examples. This goes into ``notes.rst``.
  The notes should be more like a summary
  than a narrative re-explanation of the
  chapter contents. But, honestly, I don't
  follow my own advice, here.
