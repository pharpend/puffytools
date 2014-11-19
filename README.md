# puffytools

This is the Puffy Toolkit, or Ptk. Ptk is a 

# Installation and usage

There isn't a stable version yet, but you can install the development
version. You'll need `git`, `ghc`, `sh`, and `cabal-install`. You can see
[bitemyapp/learnhaskell][1] for information on installing those.

## Installation

1.  First, clone the repository

        git clone git://gitlab.com/pharpend/puffytools.git -b develop
        cd puffytools

2.  Next, you'll want to run the build script

        ./ptkbuild

    The build script is relatively simple, you can see the options here. With no
    arguments, the default behavior is identical to `./ptkbuild clean build
    test`, which cleans any installation of Ptk you already have, installs the
    newest version, and runs the test suite. You can see all of the options by
    looking at the [`ptkbuild` file][3] in this repository.

## Usage

You can run `ptk help`, and get this output. This is accurate of commit 4ef330c.

**ptk** \
The Puffy Toolkit, version 0.0.0\
\
  **journal**\
  Do things with Journals\
  \
    **list**\
    List all of the available journals\
    \
    \
    **help**\
    Show help for the journal module\
    \
    \
  \
  **help**\
  Show this help menu.\
  \
  \
  **version**\
  Print the version to the console. Meant for use in other programs.\
  
# Licensing

Like most Haskell packages, Ptk is licensed under the BSD-3 license. You can see
the [`LICENSE` file][2] for more information.
        
[1]: https://github.com/bitemyapp/learnhaskell
[2]: https://github.com/pharpend/puffytools/blob/develop/LICENSE
[3]: https://github.com/pharpend/puffytools/blob/develop/ptkbuild
