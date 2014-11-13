{- |
Module       : Main
Description  : Runs the puffy tool kit (ptk)
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Main where

import           PuffyTools.Ptk
import           System.Environment (getArgs)

main :: IO ()
main = (runCommand . parseCommand) =<< getArgs
