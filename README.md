# puffytools, version 0.0.0

This is the Puffy Toolkit, or Ptk. Ptk is a command-line "personal assistant",
of sorts. Currently, it will keep journals. I'm adding more functionality as I
can. Pull requests are welcome, although do read the "Development version"
section below.

# Installation and usage

You can install the experimental version from Hackage. If you use Windows or OS
X, you'll need to install the
[Haskell Platform](https://www.haskell.org/platform/). On Linux or BSD, your
distribution should have the packages `ghc` and `cabal-install`. The GHC version
must be 7.8 or later. Install those, then run:

    cabal update
    cabal install puffytools

If the installation fails, you may need to install an updated version of
`cabal-install`, `alex`, and `happy`:

    cabal install cabal-install alex happy
    cabal install puffytools

## Development version

If you want the development version, the steps are a bit different. 

    git clone git://github.com/pharpend/puffytools.git -b develop
    cd puffytools
    cabal sandbox init
    cabal install --enable-tests

I use `gitflow` as a branching model, so
[read about that](http://nvie.com/posts/a-successful-git-branching-model/) if
you have never heard about it. I don't particularly care what branching model
you use, as long as you don't commit to the `master` or `develop` branches.

## Usage

```
ptk
The Puffy Toolkit, version 0.0.0

  j
  Same as journal.
  
    add_entry slug STRING
    Add an entry to journal with a given slug. The second argument is the entry text.
    
    
    ae slug STRING
    Same as add_entry
    
    
    cat slug
    Output the raw journal
    
    
    list
    List all of the available journals
    
    
    ls
    Same as list
    
    
    list_entries JOURNAL_SLUG
    List the entries in a given journal
            --time-format=STRING,
      --strftime=STRING          The format with which to print timestamps. See `man strftime` for more information.
    
    
    le JOURNAL_SLUG
    Same as listEntries
            --time-format=STRING,
      --strftime=STRING          The format with which to print timestamps. See `man strftime` for more information.
    
    
    new
    Create a new journal
      -sSTRING,
      -nSTRING --slug=STRING,
      --name=STRING                 The slug/short name of the journal.
      -tSTRING --title=STRING       The title of the journal
      -dSTRING --description=STRING The journal Description
    
    
    help
    Show help for the journal module
    
    
  
  journal
  Do things with Journals
  
    add_entry slug STRING
    Add an entry to journal with a given slug. The second argument is the entry text.
    
    
    ae slug STRING
    Same as add_entry
    
    
    cat slug
    Output the raw journal
    
    
    list
    List all of the available journals
    
    
    ls
    Same as list
    
    
    list_entries JOURNAL_SLUG
    List the entries in a given journal
            --time-format=STRING,
      --strftime=STRING          The format with which to print timestamps. See `man strftime` for more information.
    
    
    le JOURNAL_SLUG
    Same as listEntries
            --time-format=STRING,
      --strftime=STRING          The format with which to print timestamps. See `man strftime` for more information.
    
    
    new
    Create a new journal
      -sSTRING,
      -nSTRING --slug=STRING,
      --name=STRING                 The slug/short name of the journal.
      -tSTRING --title=STRING       The title of the journal
      -dSTRING --description=STRING The journal Description
    
    
    help
    Show help for the journal module
    
    
  
  help
  Show this help menu.
  
  
  version
  Print the version to the console. Meant for use in other programs.
  
  
  shell
  PTK REPL (Expiremental).
``` 

# Licensing

Like most Haskell packages, Ptk is licensed under the BSD-3 license. You can see
the [`LICENSE` file][2] for more information.
        
[1]: https://github.com/bitemyapp/learnhaskell
[2]: https://github.com/pharpend/puffytools/blob/develop/LICENSE
[3]: https://github.com/pharpend/puffytools/blob/develop/ptkbuild
