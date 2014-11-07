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
import           Data.Aeson.Encode.Pretty
import           Data.Monoid
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as B
import           Data.List.Utils
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Mittens.Slug
import           System.Directory
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
  parseJSON _ = fail "Not an object"

instance ToJSON Entry where
  toJSON e = object [ "summary" .= entrySummary e
                    , "created" .= entryCreated e
                    , "last-edited" .= entryLastEdited e
                    ]

addEntry :: Journal -> Entry -> Journal
addEntry j e = j { journalEntries = newEntries }
  where newEntries = journalEntries j `V.snoc` e

mkEntry :: Text -> IO Entry
mkEntry entryText = Entry entryText <$> getCurrentTime <*> getCurrentTime

-- |Makes a journal, given a slug
mkJournal :: Slug -> IO Journal
mkJournal s = getCurrentTime >>= \t -> return $ Journal s mempty t t mempty mempty

-- |Figures out the file path for a journal
generateJournalPath :: Journal -> IO FilePath
generateJournalPath j = getAppUserDataDirectory "mittens" >>= \d -> return $ d <> "/" <> (T.unpack . unSlug . journalSlug) j <> ".json"

generateSlugPath :: Slug -> IO FilePath
generateSlugPath slg = do
  ddir <- getAppUserDataDirectory "mittens"
  let fullPath = mconcat [ddir, "/", T.unpack $ unSlug slg, ".json"]
  return fullPath

-- |Writes a journal to a file path
writeJournal :: Journal -> IO ()
writeJournal j = do pth <- generateJournalPath j; B.writeFile pth $ encodePretty j

-- |Reads a journal from the default file path (~/.mittens/journal-title.json)
readJournalName :: Text -> IO Journal
readJournalName name = do
  slg <- case mkSlugEither name of
           Left err -> fail err
           Right s  -> return s
  generateSlugPath slg >>= readJournalFromFile

-- |Reads a journal from the default file path (~/.mittens/journal-title.json)
readJournalDef :: Slug -> IO Journal
readJournalDef slg = generateSlugPath slg >>= readJournalFromFile

-- |Reads a journal, given a file path
readJournalFromFile :: FilePath -> IO Journal
readJournalFromFile fp = openFile fp ReadMode >>= readJournalFromHandle

-- |Reads a journal from a handle, close handle
readJournalFromHandle :: Handle -> IO Journal
readJournalFromHandle h = do
  handleBytes <- Bs.hGetContents h
  case eitherDecodeStrict' handleBytes of
    Left err -> fail err
    Right j  -> return j
  
listJournals :: IO [FilePath]
listJournals = filter (endswith ".json") <$> allDataFiles
  where allDataFiles =  getAppUserDataDirectory "mittens" >>= getDirectoryContents
