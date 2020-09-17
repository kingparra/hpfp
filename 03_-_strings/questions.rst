*********************
 Chapter 3 Questions
*********************
How do I write multi-line strings?

::

  "like\n\
  \this,\n\
  \see?"

  "Here is a backslant \\ as well as \137, \
  \a numeric escape character, and \ˆX, a control character."


What are the quoting rules?

* Haskell has the standard escape sequences ``\a``, ``\b``, ``\f``, ``\n``,
  ``\r``, ``\t``, ``\v``. Control characters are represented as ``\^X`` (for
  control x). Numeric escape codes for Unicode code points exist in decimal
  ``\135``, octal ``\o137``, and hex ``\x37``.


How do I write a raw string (a string that doesn't expand escape sequences, but
treats them literally)?

* https://hackage.haskell.org/package/raw-strings-qq

  ::

    {-# LANGUAGE QuasiQuotes #-}
    import Text.RawString.QQ
    putStrLn $ "regex is" ++ [r|\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,3}|]

Is there anything similar to a triple quoted string, or heredoc, that I can use
to quote a block of text and indent it properly without including that extra
leading indentation in the string itself?

* Using the Text.Heredoc package, from
  https://hackage.haskell.org/package/heredoc-0.2.0.0/docs/Text-Heredoc.html,
  you can instead do::

    {-# LANGUAGE QuasiQuotes #-}
    import Text.Heredoc
    famousQuote = 
      [str|Any dictator would admire the
          |uniformity and obedience of the U.S. media.
          |  -- Noam Chomsky
          |]


Are unix paths represented as strings? If not, what type do they use and how
does that behave?


What string types are available? Which are widely used?

See http://dev.stephendiehl.com/hask/#strings for a starting point.


Why is the performance of String "hilariously terrible", as Stephen Diehl puts
it, in comparison to Text?


Is there a way to use Text instead of String by default?


Is there a way to move string processing to compile time?


Why is OverloadedStrings needed?


What is QuasiQuotes?


What is TemplateHaskell?

Does Haskell have a nice templating engine like Pythons Jinja2?
