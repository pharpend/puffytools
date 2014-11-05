{- |
Module       : Mittens.MtnUtil.Journal
Description  : Parsing for the Journal module in Mittens
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Mittens.MtnUtil.Journal where

data JournalCommand = JournalHelp
                    | JournalHelpErr String
                    | JournalNew
  deriving (Eq, Show)

parseJournalCommand :: [String] -> JournalCommand
parseJournalCommand jc = case jc of
  "new":_ -> JournalNew
  "help":_ -> JournalHelp
  "-h":_ -> JournalHelp
  "--help":_ -> JournalHelp
  "--usage":_ -> JournalHelp
  x -> JournalHelpErr $ "Not recognized: " ++ show x

runJournalCommand :: JournalCommand -> IO ()
runJournalCommand cmd = case cmd of
  JournalHelp -> fail "Not yet implemented - journal help."
  JournalHelpErr e -> fail e
  JournalNew -> fail "Not yet implemented."
