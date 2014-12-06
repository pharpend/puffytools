{- |
Module       : Ptk.Journal.AddEntry
Description  : `ptk journal add_entry` command tree
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Ptk.Journal.AddEntry (journalAETree, journalAddEntryTree, journalAddEntryHelp) where

import           Data.Monoid
import           Data.Text (Text, pack, unpack)
import           PuffyTools.Journal
import           PuffyTools.Slug
import           System.Console.Argument 
import           System.Console.Command hiding (name)
import           System.Console.Program (showUsage)
import qualified Data.Vector as V

journalAddEntryHelp :: Action IO
journalAddEntryHelp = io $ showUsage journalAddEntryTree

-- |Same as add_entry
journalAETree = Node aeCommand []
  where
    aeCommand = Command "ae" "Same as add_entry" aeAction

-- |This is basically the tree of things that get parsed when the
-- luser runs "ptk journal AddEntry"
journalAddEntryTree :: Commands IO
journalAddEntryTree = Node addEntryCommand []
  where
    addEntryCommand = Command
                        "add_entry"
                        "Add an entry to journal with a given slug. The second argument is the entry text."
                        aeAction

aeAction :: Action IO
aeAction = withNonOption slugType $ \slg -> withNonOption string (runAE slg)

slugType :: Type Slug
slugType = Type {parser = \s -> mkSlugEither (pack s)
                ,name = "slug"
                ,defaultValue = Nothing}

-- |Run the `ptk journal add_entry` command
runAE :: Slug -> String -> Action IO
runAE slug entryStr = io $ do
  unlessJournal (unSlug slug) $ do
    fail $ "Journal does not exist: " <> (unpack $ unSlug slug)
  jl <- readJournalDef slug
  ety <- mkEntry (pack entryStr)
  let nj = jl { journalEntries = journalEntries jl <> V.singleton ety }
  writeJournal nj
