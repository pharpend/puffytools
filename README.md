# mittens

A CLI-based personal assistant. Mittens is still under development, so please
don't try to use it for anything important.

# Installation

I haven't published a version yet, but you can install the development
version. You'll need `git`, `ghc`, and `cabal-install`. You can see
[bitemyapp/learnhaskell][1] for information on installing those.

1.  First, clone the repository

        git clone git://gitlab.com/pharpend/mittens.git
        cd mittens

2.  Set up a cabal sandbox

        cabal update
        cabal sandbox init
        cabal install -j

# Usage

    SHORT  LONG     DESCRIPTION                  
           license  Print out the LICENSE (BSD3).
           readme   Print out the README.        
    h      help     Print this page              
           usage    Print this page              

        
[1]: //github.com/bitemyapp/learnhaskell
