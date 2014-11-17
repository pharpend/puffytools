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
helpTree =
  let helpCommand = Command "help" "Show this help menu." help
  in Node helpCommand []

help :: Action IO
help = io $ showUsage commandTree

commandTree :: Commands IO
commandTree = Node (Command "ptk" description help) [journalTree, helpTree, versionTree]
  where
    description = "The Puffy Toolkit, version " ++ showVersion version

journalTree :: Commands IO
journalTree = Node journalCommand []
  where
    journalCommand = Command "journal" "Do things with Journals" journalHelp
    journalHelp = io $ showUsage journalTree

main :: IO ()
main = single commandTree
