{- |
Module       : Main
Description  : Runs the puffy tool kit (ptk)
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Main where

import           Data.Version
import           Paths_puffytools
import           Ptk.Journal
import           System.Console.Argument
import           System.Console.Command
import           System.Console.Program

versionTree :: Commands IO
versionTree = Node commandVersion []
  where
    commandVersion = Command
                       "version"
                       "Print the version to the console. Meant for use in other programs."
                       printVersion
    printVersion = io . putStr $ showVersion version

helpTree :: Commands IO
helpTree = Node helpCommand []
  where
    helpCommand = Command "help" "Show this help menu." help

shellTree :: Commands IO
shellTree = Node ptkShell []
  where
    ptkShell = Command "shell" "PTK REPL (Expiremental)." (io $ interactive commandTree)

help :: Action IO
help = io $ showUsage commandTree

commandTree :: Commands IO
commandTree = Node (Command "ptk" description help) [jTree, journalTree, helpTree, versionTree, shellTree]
  where
    description = "The Puffy Toolkit, version " ++ showVersion version

main :: IO ()
main = single commandTree
