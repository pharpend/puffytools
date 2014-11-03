{- |
Module       : Mittens.Journal
Description  : A Journal-keeping thing
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Mittens.Journal where

import           Control.Applicative
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import qualified Data.Vector as V
import           Mittens.Time ()

-- |A Journal is really a wrapper around a list of entries
data Journal = Journal { journalTitle :: Text
                       , journalLastEdited :: ZonedTime
                       , journalCreated :: ZonedTime
                       , journalDescription :: Maybe Text
                       , journalEntries :: V.Vector Entry
                       }
  deriving (Show, Eq)

-- |Entries
data Entry = Entry { entrySummary :: Text
                   , entryCreated :: ZonedTime
                   , entryLastEdited :: ZonedTime
                   , entryDescription :: Maybe Text
                   }
  deriving (Show, Eq)

instance FromJSON Journal where
  parseJSON (Object v) = Journal <$> v .: "title"
                                 <*> v .: "last-edited"
                                 <*> v .: "created"
                                 <*> v .: "description"
                                 <*> v .: "entries"
  parseJSON _ = fail "Must be an object"


instance FromJSON Entry where
  parseJSON (Object v) = Entry <$> v .: "summary"
                               <*> v .: "created"
                               <*> v .: "last-edited"
                               <*> v .: "description"
