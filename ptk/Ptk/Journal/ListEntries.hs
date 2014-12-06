{- |
Module       : Ptk.Journal.ListEntries
Description  : `ptk journal listEntries` command tree
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Ptk.Journal.ListEntries (journalLeTree, journalListEntriesTree, journalListEntriesHelp) where

import           Control.Applicative
import           Control.Monad ((<=<))
import           Data.List (sort)
import           Data.Monoid (mconcat)
import           Data.Ord (comparing)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as Tio
import           Data.Time
import           Data.Vector (toList)
import           PuffyTools.Journal
import           PuffyTools.Slug
import           System.Console.Argument
import           System.Console.Command hiding (name)
import           System.Console.Program
import           System.Directory
import           System.Locale (defaultTimeLocale)

journalListEntriesHelp :: Action IO
journalListEntriesHelp = io $ showUsage journalListEntriesTree

journalLeTree :: Commands IO
journalLeTree = Node leCommand []
  where
    leCommand = Command "le" "Same as listEntries" listEntriesOptions

journalListEntriesTree :: Commands IO
journalListEntriesTree = Node listEntriesCommand []

listEntriesCommand = Command "list_entries" "List the entries in a given journal" listEntriesOptions
                       
listEntriesOptions = (withNonOption slugType (\slg -> withOption strftimeOption (listEntriesAction
                                                                                   slg)))
strftimeOption :: Option String
strftimeOption = option "" ["time-format", "strftime"] string "%x %X %Z"
                   "The format with which to print timestamps. See `man strftime` for more information."

slugType :: Type Slug
slugType = Type {parser = \s -> mkSlugEither (pack s)
                ,name = "JOURNAL_SLUG"
                ,defaultValue = Nothing}

instance Ord Entry where
  compare = comparing entryCreated

listEntriesAction :: Slug -> String -> Action IO
listEntriesAction slg timeFormat = io $ do
  j <- readJournalDef slg
  let etys = journalEntries j
      sorted_etys = sort $ toList etys
  mapM_ (\ety -> Tio.putStrLn =<< formatEntry ety) sorted_etys 
  where
    formatEntry ety = do
      time <- formattedTime $ entryCreated ety
      return $ mconcat [pack time, "\t", entrySummary ety]
    formattedTime :: UTCTime -> IO String
    formattedTime = fmap (formatTime defaultTimeLocale timeFormat) . utcToLocalZonedTime

