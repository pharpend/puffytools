-- -*- hindent-style: "chris-done" -*-
-- 
-- I prefer the Gibiansky style, but it is creating invalid ASTs from
-- the code below. See https://github.com/chrisdone/hindent/issues/38
-- . The Chris Done style is my second favorite, so we'll go with that
-- for the time being.

{- |
Module       : Ptk.Journal.List
Description  : `ptk journal list` command tree
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Ptk.Journal.List (journalListTree, journalListHelp) where

import Control.Applicative
import qualified Data.Text.IO as Tio
import PuffyTools.Journal
import PuffyTools.Slug
import System.Console.Argument
import System.Console.Command
import System.Console.Program
import System.Directory

journalListHelp :: Action IO
journalListHelp = io $ showUsage journalListTree

journalListTree :: Commands IO
journalListTree = Node lsCommand []
  where lsCommand =
          Command "list" "List all of the available journals" listEntries
        listEntries :: Action IO
        listEntries =
          io $
          do journals <- mapM readJournalFromFile =<< listJournals
             case journals of
               [] ->
                 putStrLn "You haven't created any journals."
               js ->
                 mapM_ (Tio.putStrLn . unSlug . journalSlug) journals
        dirOption :: IO (Option FilePath)
        dirOption =
          option "d" ["dir","directory"] directory <$>
          getAppUserDataDirectory "puffytools" <*>
          pure "The directory in which to look for journals."
