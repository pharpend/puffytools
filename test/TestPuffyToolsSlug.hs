{- |
Module       : TestPuffyToolsSlug
Description  : Test the PuffyTools.Slug module
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module TestPuffyToolsSlug where

import           Control.Applicative
import           PuffyTools.Slug
import           Data.Text (pack)
import           Test.QuickCheck

instance Arbitrary Slug where
  arbitrary = do
    slugLength <- elements [4 .. 32]
    slugCandidate <- pack <$> take slugLength <$> infiniteListOf (elements slugAcceptChars)
    case mkSlugEither slugCandidate of
      Left err  -> fail err
      Right slg -> return slg
