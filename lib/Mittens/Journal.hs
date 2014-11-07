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
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Paths_mittens
import           Mittens.Slug
import           Safe
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

mkEntry :: Text -> Maybe Text -> IO Entry
mkEntry sum desc = do
  t <- getCurrentTime
  return $ Entry sum t t desc


addEntry :: Journal -> Entry -> Journal
addEntry j e = j { journalEntries = newEntries }
  where newEntries = journalEntries j `V.snoc` e

getEntry :: Text -> IO Entry
getEntry entryText = do
  let entryLines = T.lines entryText
  sumry <- case headMay entryLines of
             Just sum -> return sum
             Nothing  -> fail "empty summary, aborting"
  let desc = T.unlines <$> tailMay entryLines
  mkEntry sumry desc

-- |Makes a journal, given a slug
mkJournal :: Slug -> IO Journal
mkJournal s = getCurrentTime >>= \t -> return $ Journal s mempty t t mempty mempty

-- |Figures out the file path for a journal
generateJournalPath :: Journal -> IO FilePath
generateJournalPath j = getDataDir >>= \d -> return $ d <> "/" <> (T.unpack . unSlug . journalSlug) j <> ".json"

generateSlugPath :: Slug -> IO FilePath
generateSlugPath slg = do
  ddir <- getDataDir
  let fullPath = mconcat [ddir, "/", T.unpack $ unSlug slg, ".json"]
  return fullPath

-- |Writes a journal to the default file path (~/.mittens/journal-title.json)
writeJournalDef ::  Journal -> IO ()
writeJournalDef j = generateJournalPath j >>= \fp -> writeJournalToFile fp j 

-- |Writes a journal to a file path
writeJournalToFile :: FilePath -> Journal -> IO ()
writeJournalToFile fp j = openFile fp WriteMode >>= \h -> writeJournalToHandle h j

-- |Writes a journal to a handle, close handle
writeJournalToHandle :: Handle -> Journal -> IO ()
writeJournalToHandle h j = let encoded = encode j in B.hPut h encoded *> hClose h

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
readJournalFromFile fp = openFile fp WriteMode >>= readJournalFromHandle

-- |Reads a journal from a handle, close handle
readJournalFromHandle :: Handle -> IO Journal
readJournalFromHandle h = do
  handleBytes <- Bs.hGetContents h
  hClose h
  case eitherDecodeStrict' handleBytes of
    Left err -> fail err
    Right j  -> return j
  
