{- |
Module       : TestPtkArgumentResult.hs
Description  : Test the PuffyTools argument result monad.
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module TestPtkArgumentResult where

import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.Int
import           Data.Word
import           PuffyTools.Journal
import           PuffyTools.Ptk.ArgumentResult
import           TestPuffyToolsJournal
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (ArgumentResult a) where
  arbitrary = ArgumentResult <$> arbitrary

type AR = ArgumentResult
type ART a = ArgumentResult a -> Bool

-- boilerTest :: String -> ART a -> b
-- boilerTest s f = testGroup s

--  These test to make sure the functor laws hold. See http://hackage.haskell.org/package/base-4.7.0.1/docs/Prelude.html.
-- fmap id == id
prop_fmapIdId :: Eq a => ArgumentResult a -> Bool
prop_fmapIdId ar = fmap id ar == id ar
group_fmapIdId = testGroup "fmap id = id" 
                   [ testProperty "Int" (prop_fmapIdId :: ART Int)
                   , testProperty "String" (prop_fmapIdId :: ART String)
                   , testProperty "Bool" (prop_fmapIdId :: ART Bool)
                   , testProperty "Entry" (prop_fmapIdId :: ART Entry)
                   , testProperty "Journal" (prop_fmapIdId :: ART Journal)
                   , testProperty "Double" (prop_fmapIdId :: ART Double)
                   , testProperty "Float" (prop_fmapIdId :: ART Float)
                   , testProperty "Int8" (prop_fmapIdId :: ART Int8)
                   , testProperty "Int16" (prop_fmapIdId :: ART Int16)
                   , testProperty "Int32" (prop_fmapIdId :: ART Int32)
                   , testProperty "Int64" (prop_fmapIdId :: ART Int64)
                   , testProperty "Integer" (prop_fmapIdId :: ART Integer)
                   , testProperty "Ordering" (prop_fmapIdId :: ART Ordering)
                   , testProperty "Word" (prop_fmapIdId :: ART Word)
                   , testProperty "Word8" (prop_fmapIdId :: ART Word8)
                   , testProperty "Word16" (prop_fmapIdId :: ART Word16)
                   , testProperty "Word32" (prop_fmapIdId :: ART Word32)
                   , testProperty "Word64" (prop_fmapIdId :: ART Word64)
                   ]

-- fmap (f . g) = fmap f . fmap g
prop_fmapFDotG :: (Eq a, Eq c) => (b -> c) -> (a -> b) -> ArgumentResult a -> Bool
prop_fmapFDotG f g ar = fmap (f . g) ar == (fmap f . fmap g) ar

-- With 
prop_fmapFDotG0 :: Char ->  ART String
prop_fmapFDotG0 c = prop_fmapFDotG (map toUpper) (intersperse c)

prop_fmapFDotG1 :: Char -> ART String
prop_fmapFDotG1 c = prop_fmapFDotG (intersperse c) (map toUpper)

prop_fmapFDotG2 :: String -> ART String
prop_fmapFDotG2 s = prop_fmapFDotG (\_ -> s) length

prop_fmapFDotG3 :: Int -> ART String
prop_fmapFDotG3 n = prop_fmapFDotG (+ n) length

prop_fmapFDotG4 :: Int -> ART String
prop_fmapFDotG4 n = prop_fmapFDotG (* n) length

prop_fmapFDotG5 :: ART String
prop_fmapFDotG5 = prop_fmapFDotG show length


group_fmapFDotG1 = testGroup "some arbitrary properties"
                     [ testProperty "prop0" prop_fmapFDotG0
                     , testProperty "prop1" prop_fmapFDotG1
                     , testProperty "prop2" prop_fmapFDotG2
                     , testProperty "prop3" prop_fmapFDotG3
                     , testProperty "prop4" prop_fmapFDotG4
                     , testProperty "prop5" prop_fmapFDotG5
                     ]

-- group_fmapFDotG2 = boilerTest "show . read" (prop_fmapFDotG show read)
-- group_fmapFDotG3 = boilerTest "read . show" (prop_fmapFDotG read show)

group_fmapFDotG = testGroup "fmap (f . g) = fmap f . fmap g" [group_fmapFDotG1]

-- pure id <*> v = v
prop_pureId :: Eq a => ART a
prop_pureId v = (pure id <*> v) == v

arTestGroup = [testGroup "ArgumentResult" [testGroup "Functor laws"
                                             [group_fmapIdId, group_fmapFDotG]]]
