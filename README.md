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

Mittens is used via a client, called `mtn`. Note that these subcommands should
not be preceded with a hyphen. That is, run `mtn help` rather than `mtn --help`.

    OPTION              THING IT DOES
    ----------------------------------------------
    h, help, usage      Print this page
    license             Print the license (BSD-3).
    version             Print the version

        
[1]: //github.com/bitemyapp/learnhaskell
