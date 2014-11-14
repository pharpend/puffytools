{- |
Module       : PuffyTools.Ptk.ArgumentResult
Description  : Argument parsing for the ArgumentResult module in PuffyTools
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module PuffyTools.Ptk.ArgumentResult where

import           Control.Applicative

data ArgumentResult a = ArgumentResult { unArg :: a }
  deriving (Show, Eq)

instance Functor ArgumentResult where
  fmap f (ArgumentResult a) = ArgumentResult (f a)

instance Applicative ArgumentResult where
  pure = ArgumentResult
  (ArgumentResult f) <*> (ArgumentResult a) = ArgumentResult (f a)

instance Monad ArgumentResult where
  return = pure
  (ArgumentResult v) >>= f = f v
