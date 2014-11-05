{- |
Module       : Main
Description  : Test the Mittens journal functions
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Main where

import           TestMittensJournal
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [testGroup "QuickCheck Mittens.Journal"
           [testGroup "Aeson properties"
              [ testProperty "(encode . decode . encode) = (encode)" prop_encDecEnc
              , testProperty "(decode . encode . decode . encode) = (decode . encode)"
                  prop_decEncDecEnc
              , testProperty "(decode . encode . decode . encode)^n = (decode . encode)" prop_dEn
              ]]]
