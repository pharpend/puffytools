{- |
Module       : Mittens.MtnUtil
Description  : Utilities for the mtn executable
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Mittens.MtnUtil where

import           Control.Applicative
import qualified Data.ByteString as B
import           Data.Version (showVersion)
import           Paths_mittens
import           System.Exit
import           System.IO

parseArguments :: [String] -> IO ()
parseArguments args = case args of
  ["h"] -> help stdout
  ["help"] -> help stdout
  ["license"] -> printLicense
  ["usage"] -> help stdout
  ["version"] -> printVersion
  _ -> help stderr *> exitFailure

help :: Handle -> IO ()
help hdl = do
  hSetBinaryMode stdout True
  B.hPut hdl =<< B.readFile =<< getDataFileName "res/usage.txt"

printLicense :: IO ()
printLicense = do
  hSetBinaryMode stdout True
  B.hPut stdout =<< B.readFile =<< getDataFileName "LICENSE"
  
printVersion :: IO ()
printVersion = hPutStr stdout $ showVersion version
