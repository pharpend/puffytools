{- |
Module       : Mittens.Mtn
Description  : Utilities for the mtn executable
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Mittens.Mtn where

import           Control.Applicative
import qualified Data.ByteString as B
import           Data.Version (showVersion)
import           Paths_mittens
import qualified Mittens.Mtn.Journal as J
import           System.IO

data Command = Help
             | HelpErr String
             | Journal { rest :: [String] }
             | PrintLicense
             | PrintVersion
  deriving (Eq, Show)

-- |Parse a string into a Command, failing with an error if it doesn't
-- match.
parseCommand :: [String] -> Command
parseCommand args = case args of
  "j":x -> Journal x
  "journal":x -> Journal x
  "--license":_ -> PrintLicense
  "--help":_ -> Help
  "--usage":_ -> Help
  "--version":_ -> PrintVersion
  x -> HelpErr $ "No such command: " ++ show x

-- |Run the command
runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  Help -> help
  HelpErr e -> helpErr e
  Journal x -> J.parseJournalCommand x
  PrintLicense -> printLicense
  PrintVersion -> printVersion

-- These are some simple things that do exactly what you think they do
help :: IO ()
help = hSetBinaryMode stdout True *> getDataFileName "res/usage.txt" >>= B.readFile >>= B.hPut stdout

helpErr :: String -> IO ()
helpErr err = help *> fail err

printLicense :: IO ()
printLicense = do
  hSetBinaryMode stdout True
  B.hPut stdout =<< B.readFile =<< getDataFileName "LICENSE"
  
printVersion :: IO ()
printVersion = hPutStr stdout $ showVersion version
