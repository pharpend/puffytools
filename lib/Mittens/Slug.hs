{- |
Module       : Mittens.Slug
Description  : Generate slugs
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

These generate slugs (file names).

The slug must be alphanumeric, with the exception of "-_". It also
must be between 4 and 32 chars long
-}

module Mittens.Slug
       (Slug, mkSlugMaybe, mkSlugEither, mkRandomSlug, slugAcceptChars,
        unSlug)
       where

import           Control.Monad
import           Data.Aeson
import           Data.Monoid ((<>))
import           Data.Random
import           Data.Text (unpack)

-- |Newtype wrapper for String
newtype Slug = MkSlug { unSlug :: String }
  deriving (Eq, Show)

-- |Maybe make a Slug
mkSlugMaybe :: String -> Maybe Slug
mkSlugMaybe s = case mkSlugEither s of
  Left _  -> Nothing
  Right q -> Just q

-- |Try to make a Slug, return an error if I can't
mkSlugEither :: String -> Either String Slug
mkSlugEither s
  | length s < 4 = Left "The slug must be at least 4 chars long."
  | length s > 32 = Left "The slug must be at most 32 chars long."
  | ftb /= s = Left $ "The slug may only contain these characters: " <> slugAcceptChars
  | otherwise = Right $ MkSlug s
  where
    -- ftb ~ "filter badness"
    ftb :: String
    ftb = filter (`elem` slugAcceptChars) s

-- |Generates a random slug 32 chars long
mkRandomSlug :: IO Slug
mkRandomSlug = replicateM 32 ioc >>= \s -> case mkSlugEither s of
                                       Left err  -> fail err
                                       Right slg -> return slg
  where
    ioc :: IO Char
    ioc = runRVar rvc StdRandom

    rvc :: RVar Char
    rvc = randomElement slugAcceptChars

-- |Acceptable characters for a slug
slugAcceptChars :: String
slugAcceptChars = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "-_"

instance FromJSON Slug where
  parseJSON (String s) = case mkSlugEither (unpack s) of
    Left err  -> fail err
    Right slg -> return slg
parseJSON _ = fail "Slug must be of type String."

instance ToJSON Slug where
  toJSON (MkSlug s) = toJSON s
