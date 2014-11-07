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

import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
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
  ("l":_)         -> journalList
  ("ls":_)         -> journalList
  ("list":_)         -> journalList
  x                  -> journalHelp $ "mtn journal : no match for pattern: " ++ show x

journalHelp :: String -> IO ()
journalHelp = fail

journalNew :: [String] -> IO ()
journalNew (name:_) = do
  slug <- case mkSlugEither (T.pack name) of
            Left err -> fail err
            Right s  -> return s
  jnl <- mkJournal slug
  putStrLn =<< generateJournalPath jnl 
  Bl.hPut stdout $ encodePretty jnl
  writeJournal jnl
journalNew xs = journalHelp $ "mtn journal new : no match for pattern: " ++ show xs

journalAddEntry :: [String] -> IO ()

journalAddEntry (name:arg:thing) = case (arg : thing) of
  ("-f":filepath:_) -> do
    journal <- readJournalName (T.pack name)
    entry <- mkEntry =<< Tio.readFile filepath
    let newJournal = journal `addEntry` entry
    writeJournal newJournal
  (summary:_) -> do
    journal <- readJournalName (T.pack name)
    entry <- mkEntry (T.pack summary)
    let nj = journal `addEntry` entry
    writeJournal nj
  _ -> journalHelp $ "mtn journal add-entry : no match for pattern: " ++ show (arg : thing)

journalAddEntry (name:_) = do
  journal <- readJournalName (T.pack name)
  etry <- mkEntry =<< Tio.hGetContents stdin
  let nj = journal `addEntry` etry
  putStrLn $ show nj
  writeJournal nj

journalAddEntry xs = journalHelp $ "mtn journal add-entry : no match for pattern: " ++ show xs

journalPrint :: [String] -> IO ()
journalPrint (name:_) = do
  slug <- case mkSlugEither (T.pack name) of
    Left err -> fail err
    Right s -> return s
  B.hPut stdout =<< B.readFile =<< generateSlugPath slug

journalPrint x = journalHelp $ "mtn journal print : no match for pattern : " ++ show x

journalList :: IO ()
journalList = listJournals >>= mapM_ putStrLn
