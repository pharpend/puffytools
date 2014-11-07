{- |
Module       : TestMittensJournal
Description  : Test the Mittens journal functions
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module TestMittensJournal where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad ((<=<))
import           Data.Aeson
import qualified Data.Text as T
import           Data.Time
import qualified Data.Vector as V
import           Mittens.Journal
import           System.IO.Unsafe
import           TestMittensSlug ()
import           Test.QuickCheck

-- |So, it turns out, encoding & decoding isn't an identity. However,
-- (encode . decode . encode) = encode, and (decode . encode . decode)
-- = decode
-- 
-- https://github.com/liyang/thyme/issues/12
-- 
-- First, (encode . decode . encode) = (encode)
prop_encDecEnc :: Journal -> Bool
prop_encDecEnc j = Just (encode j) == (encode <$> (de j))

-- |This is (decode . encode . decode . encode) = (encode . decode)
prop_decEncDecEnc :: Journal -> Bool
prop_decEncDecEnc j = (de <=< de) j == de j

-- |For the hell of it, we'll decode/encode a bunch of times
prop_dEn
  :: Journal                    -- ^Journal 
  -> Natural                    -- ^Number of times to execute decode/encode
  -> Bool
prop_dEn j n = de j == foldl (\journal _ -> de =<< journal) (de j) [(Natural 1) .. n]

-- |This is a helper function
de :: Journal -> Maybe Journal
de = decode . encode

-- 
-- For reference
-- 
-- -- |A Journal is really a wrapper around a list of entries
-- data Journal = Journal { journalTitle :: Text
--                        , journalLastEdited :: UTCTime
--                        , journalCreated :: UTCTime
--                        , journalDescription :: Maybe Text
--                        , journalEntries :: Vector Entry
--                        }
--   deriving (Show, Eq)

-- -- |Entries
-- data Entry = Entry { entrySummary :: Text
--                    , entryCreated :: UTCTime
--                    , entryLastEdited :: UTCTime
--                    , entryDescription :: Maybe Text
--                    }
--   deriving (Show, Eq)

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary UTCTime where
  arbitrary = pure $ unsafePerformIO getCurrentTime 

instance Arbitrary Entry where
  arbitrary = Entry <$> arbitrary <*> arbitrary <*> arbitrary 

instance Arbitrary x => Arbitrary (V.Vector x) where
  arbitrary = V.fromList <$> arbitrary

instance Arbitrary Journal where
  arbitrary = Journal <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

-- This type is NOT isomorphic!
newtype Natural = Natural { unNatural :: Int }
  deriving (Eq, Show)

instance Enum Natural where
  toEnum = mkNatural
  fromEnum = unNatural

mkNatural :: Int -> Natural
mkNatural i
  | i < 0 = Natural $ (-1) * i
  | i == 0 = Natural 1
  | otherwise = Natural i

instance Arbitrary Natural where
  arbitrary = mkNatural <$> arbitrary
