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
import           Data.Time
import           Data.Vector (Vector)

-- |A Journal is really a wrapper around a list of entries
data Journal = Journal { journalTitle :: Text
                       , journalLastEdited :: UTCTime
                       , journalCreated :: UTCTime
                       , journalDescription :: Maybe Text
                       , journalEntries :: Vector Entry
                       }
  deriving (Show, Eq)

-- |Entries
data Entry = Entry { entrySummary :: Text
                   , entryCreated :: UTCTime
                   , entryLastEdited :: UTCTime
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

instance ToJSON Journal where
  toJSON (Journal t le cr des ent) = object [ "title" .= t
                                            , "last-edited" .= le
                                            , "created" .= cr
                                            , "description" .= des
                                            , "entries" .= ent
                                            ]

instance FromJSON Entry where
  parseJSON (Object v) = Entry <$> v .: "summary"
                               <*> v .: "created"
                               <*> v .: "last-edited"
                               <*> v .: "description"
  parseJSON _ = fail "Not an object"

instance ToJSON Entry where
  toJSON e = object [ "summary" .= entrySummary e
                    , "created" .= entryCreated e
                    , "last-edited" .= entryLastEdited e
                    , "description" .= entryDescription e
                    ]
