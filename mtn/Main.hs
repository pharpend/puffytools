{- |
Module       : Main
Description  : Runs Mittens
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Main where

import           Control.Applicative
import           Mittens.MtnUtil
import           System.Environment (getArgs)

main :: IO ()
main = (runCommand . parseCommand) =<< getArgs
