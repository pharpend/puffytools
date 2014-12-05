{- |
Module       : Ptk.Journal
Description  : `ptk journal` command tree for the puffy toolkit
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Ptk.Journal (jTree, journalTree, journalHelp) where

import           Data.Traversable
import           PuffyTools.Journal
import           Ptk.Journal.AddEntry
import           Ptk.Journal.List
import           Ptk.Journal.New
import           System.Console.Argument
import           System.Console.Command
import           System.Console.Program

subCommandList = [journalAddEntryTree, journalAETree, journalListTree, journalLsTree, journalNewTree, journalHelpTree]
journalTree :: Commands IO
journalTree = Node journalCommand subCommandList
  where
    journalCommand = Command "journal" "Do things with Journals" journalHelp

jTree = Node jCommand subCommandList
  where jCommand = Command "j" "Same as journal." journalHelp

journalHelpTree = Node (Command "help" "Show help for the journal module" journalHelp) []

journalHelp = io $ showUsage journalTree
