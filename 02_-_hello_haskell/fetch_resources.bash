#!/usr/bin/env bash
# Fetch handy documentation, papers, cheat sheets, and a youtube series on haskell.

# haskell 2010 language report
wget 'https://haskell.org/definition/haskell2010.pdf'

# ghc user guide
wget 'https://downloads.haskell.org/~ghc/latest/docs/users_guide.pdf'

# cabal user guide
wget 'https://cabal.readthedocs.io/_/downloads/en/3.2/epub/'

# stack user guide
wget 'https://docs.haskellstack.org/_/downloads/en/v1.0.2/htmlzip/'

# haddoc documentation
wget 'https://haskell-haddock.readthedocs.io/_/downloads/en/latest/pdf/'

# what I wish I knew when learning haskell
wget 'http://dev.stephendiehl.com/hask/tutorial.pdf'

# haskell cheat sheet
wget 'https://cheatsheet.codeslower.com/CheatSheet.pdf'

# learn haskell in one video
youtube-dl 'https://www.youtube.com/watch?v=02_H3LjqMr8'

# haskell for imperative programmers
youtube-dl 'https://www.youtube.com/playlist?list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV'

# pdf files for the "follow-up resources" section at the end of each chapter
git clone 'https://github.com/pushcx/hpffp-resources'
