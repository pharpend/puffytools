{- |
Module       : Mittens.Mtn.Journal
Description  : Argument parsing for the Journal module in Mittens
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Mittens.Mtn.Journal where

import           Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import           Data.Time
import qualified Data.Vector as V
import           Mittens.Journal
import           Mittens.Slug
import           Safe
import           System.IO

parseJournalCommand :: [String] -> IO ()
parseJournalCommand jc = case jc of
  _          -> journalHelp
  "new":rest -> journalNew rest
  "n":rest   -> journalNew rest

journalHelp :: IO ()
journalHelp = fail "help not yet implemented"

journalNew :: [String] -> IO ()
journalNew (name:_) = do
  slug <- case mkSlugEither (T.pack name) of
            Left err -> fail err
            Right s  -> return s
  jnl <- mkJournal slug
  writeJournalDef jnl
journalNew _ = journalHelp

journalAddEntry :: [String] -> IO ()

journalAddEntry (name:"-":_) = journalAddEntry [name]

journalAddEntry (name:_) = do
  journal <- readJournalName (T.pack name)
  etry <- getEntry =<< Tio.hGetContents stdin
  let nj = journal `addEntry` etry
  writeJournalDef nj

journalAddEntry (name:"-s":summary:_) = do
  journal <- readJournalName (T.pack name)
  entry <- mkEntry (T.pack summary) Nothing
  let nj = journal `addEntry` entry
  writeJournalDef nj

journalAddEntry (name:"-f":filepath:_) = do
  journal <- readJournalName (T.pack name)
  entry <- getEntry =<< Tio.readFile filepath
  let newJournal = journal `addEntry` entry
  writeJournalDef newJournal

journalAddEntry _ = journalHelp
