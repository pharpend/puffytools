{- |
Module       : TestMittensSlug
Description  : Test the Mittens.Slug module
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module TestMittensSlug where

import           Control.Applicative
import           Mittens.Slug
import           Data.Text (pack)
import           Test.QuickCheck

-- |This is horrifying, but the alternative is to use
-- unsafePerformIO. This solution keeps everything in the Gen monad.
instance Arbitrary Slug where
  arbitrary = elements [4 .. 32] >>= \ln ->
                                        take ln <$> infiniteListOf (elements slugAcceptChars) >>= \str ->
                                                                                                     case mkSlugEither $ pack str of
                                                                                                       Left err -> fail err
                                                                                                       Right slg -> return slg
