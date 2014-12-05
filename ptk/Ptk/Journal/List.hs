{- |
Module       : Ptk.Journal.List
Description  : `ptk journal list` command tree
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Ptk.Journal.List (journalLsTree, journalListTree, journalListHelp) where

import           Control.Applicative
import qualified Data.Text.IO as Tio
import           PuffyTools.Journal
import           PuffyTools.Slug
import           System.Console.Argument
import           System.Console.Command
import           System.Console.Program
import           System.Directory

journalListHelp :: Action IO
journalListHelp = io $ showUsage journalListTree

journalLsTree :: Commands IO
journalLsTree = Node lsCommand []
  where
    lsCommand = Command "ls" "Same as list" listJournals

journalListTree :: Commands IO
journalListTree = Node listCommand []
  where
    listCommand = Command "list" "List all of the available journals" listJournals

listJournals :: Action IO
listJournals = io $ do
  journals <- mapM readJournalFromFile =<< listJournals
  case journals of
    [] -> putStrLn "You haven't created any journals."
    js -> mapM_ (Tio.putStrLn . unSlug . journalSlug) journals
dirOption :: IO (Option FilePath)
dirOption = option "d" ["dir", "directory"] directory <$> getAppUserDataDirectory "puffytools"
                                                      <*> pure
                                                            "The directory in which to look for journals."
