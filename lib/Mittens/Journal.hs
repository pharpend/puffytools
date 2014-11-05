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
import           Data.Monoid
import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Vector (Vector)
import           Paths_mittens
import           Mittens.Slug
import           System.IO

-- |A Journal is really a wrapper around a list of entries
data Journal = Journal { journalSlug :: Slug
                       , journalTitle :: Text
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
  parseJSON (Object v) = Journal <$> v .: "slug"
                                 <*> v .: "title"
                                 <*> v .: "last-edited"
                                 <*> v .: "created"
                                 <*> v .: "description"
                                 <*> v .: "entries"
  parseJSON _ = fail "Must be an object"

instance ToJSON Journal where
  toJSON (Journal s t le cr des ent) = object [ "slug" .= s
                                              , "title" .= t
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

-- |Makes a "blank" journal
mkJournal :: IO Journal
mkJournal = Journal <$> mkRandomSlug
                    <*> pure mempty
                    <*> getCurrentTime
                    <*> getCurrentTime
                    <*> pure mempty
                    <*> pure mempty

-- |Figures out the file path for a journal
generateJournalPath :: Journal -> IO FilePath
generateJournalPath j = getDataDir >>= \d -> return $ d <> "/" <> (T.unpack . unSlug . journalSlug) j <> ".json"

-- |Writes a journal to the default file path (~/.mittens/journal-title.json)
writeJournalDef ::  Journal -> IO ()
writeJournalDef j = generateJournalPath j >>= \fp -> writeJournalToFile fp j 

-- |Writes a journal to a file path
writeJournalToFile :: FilePath -> Journal -> IO ()
writeJournalToFile fp j = openFile fp WriteMode >>= \h -> writeJournalToHandle h j

-- |Writes a journal to a handle
writeJournalToHandle :: Handle -> Journal -> IO ()
writeJournalToHandle h j = B.hPut h $ encode j
