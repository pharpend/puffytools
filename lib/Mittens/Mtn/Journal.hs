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

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import           Mittens.Journal
import           Mittens.Slug
import           System.IO

parseJournalCommand :: [String] -> IO ()
parseJournalCommand jc = case jc of
  ("new":rest)       -> journalNew rest
  ("n":rest)         -> journalNew rest
  ("ae":rest)        -> journalAddEntry rest
  ("add-entry":rest) -> journalAddEntry rest
  ("a":rest)         -> journalAddEntry rest
  ("p":rest)         -> journalPrint rest
  ("print":rest)         -> journalPrint rest
  ("cat":rest)         -> journalPrint rest
  x                  -> journalHelp $ "mtn journal -> no such pattern: " ++ show x

journalHelp :: String -> IO ()
journalHelp = fail

journalNew :: [String] -> IO ()
journalNew (name:_) = do
  slug <- case mkSlugEither (T.pack name) of
            Left err -> fail err
            Right s  -> return s
  jnl <- mkJournal slug
  writeJournalDef jnl
journalNew xs = journalHelp $ "mtn journal new -> no such pattern: " ++ show xs

journalAddEntry :: [String] -> IO ()

journalAddEntry (name:arg:thing:rest)
  | arg == "-s" = do
    let summary = thing
    journal <- readJournalName (T.pack name)
    entry <- mkEntry (T.pack summary) Nothing
    let nj = journal `addEntry` entry
    writeJournalDef nj
  | arg == "-f" = do
    let filepath = thing
    journal <- readJournalName (T.pack name)
    entry <- getEntry =<< Tio.readFile filepath
    let newJournal = journal `addEntry` entry
    writeJournalDef newJournal
  | otherwise = journalHelp $ "mtn journal add-entry -> no such pattern: " ++ show (arg:thing:rest)

journalAddEntry (name:_) = do
  journal <- readJournalName (T.pack name)
  etry <- getEntry =<< Tio.hGetContents stdin
  let nj = journal `addEntry` etry
  writeJournalDef nj

journalAddEntry xs = journalHelp $ "mtn journal add-entry -> no such pattern: " ++ show xs

journalPrint :: [String] -> IO ()
journalPrint (name:_) = do
  slug <- case mkSlugEither (T.pack name) of
    Left err -> fail err
    Right s -> return s
  B.hPut stdout =<< B.readFile =<< generateSlugPath slug

journalPrint x = journalHelp $ "mtn journal print : no match for pattern : " ++ show x
