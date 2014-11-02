{- |
Module       : Mittens.MtnUtil
Description  : Utilities for the mtn executable
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Mittens.MtnUtil where

import qualified Data.ByteString as B
import           Data.List (transpose)
import qualified Data.Map.Lazy as M
import           Data.Monoid ((<>))
import           Paths_mittens
import           Safe
import           System.IO (hPutStr, hSetBinaryMode, stderr, stdout)
import           Text.PrettyPrint.Boxes hiding ((<>))

-- Well, hello, there. These first few functions just detail the
-- possible options. The boring options are below these first
-- few. Once you get past the interesting functions, there is a
-- comment telling you the rest of the functions are boring. I don't
-- recommend reading those functions, but you can if you want.

-- List of possible options
optionList :: [Arg]
optionList = [licenseArg, readmeArg, helpArg, usageArg]

{- THE FUNCTIONS BELOW HERE ARE NOT INTERESTING. THEY ONLY EXIST TO
HELP THE FUNCTIONS ABOVE.-}

-- |First, we have the data structure of an argument
data Arg = Arg { shortName :: Maybe String
               , longName :: String
               , description :: String
               , performAction :: IO ()
               }
         | Help { argList :: [Arg] }
  deriving (Show, Eq)

-- | Parse the command line arguments This more or less does exactly
-- what you expect.
parseArguments :: [String] -> IO ()
parseArguments args = case matchArgs args of
  Just opt -> performAction opt
  Nothing -> performAction helpArg

-- |This looks up the first option in a key-value table and returns
-- the result. 
matchArgs :: [String] -> Maybe Arg
matchArgs args = headMay args >>= \a -> M.lookup a optionMap

-- |Argument to print the help manual
usageArg :: Arg
usageArg = Arg Nothing "usage" "Print this page" $ performAction helpArg

-- |Argument to print the help manual
helpArg :: Arg
helpArg = Arg (Just "h") "help" "Print this page" . hPutStr stderr $ render_table helpData
  where
    helpData :: [[String]]
    helpData = ["SHORT", "LONG", "DESCRIPTION"] : [[case shortName a of
                                                     Just q -> q
                                                     Nothing -> "", longName a, description a] | a <- optionList]

    -- |Stolen from http://www.tedreed.info/programming/2012/06/02/how-to-use-textprettyprintboxes/
    render_table :: [[String]] -> String
    render_table rws = render $ hsep 2 left (map (vcat left . map text) (transpose rws))


-- |Argument to print the license
licenseArg :: Arg
licenseArg = Arg Nothing "license" "Print out the LICENSE (BSD3)." printLicense
  where
    printLicense :: IO ()
    printLicense = do
      hSetBinaryMode stdout True
      getDataFileName "LICENSE" >>= B.readFile >>= B.hPut stdout

-- |Argument to print the readme
readmeArg :: Arg
readmeArg = Arg Nothing "readme" "Print out the README." printReadme
  where
    printReadme :: IO ()
    printReadme = do
      hSetBinaryMode stdout True
      getDataFileName "README.md" >>= B.readFile >>= B.hPut stdout

-- |This is a map for the options, it speeds up lookup
optionMap :: M.Map String Arg
optionMap = M.fromList $ [(longName opt
                          ,opt) | opt <- optionList] <> [(case shortName opt of
                                                            Just q -> q
                                                            Nothing -> ""
                                                         ,opt) | opt <- optionList]

-- |These exist just to satisfy GHC
instance (Show x) => Show (IO x) where
  show _ = ""
instance (Eq x) => Eq (IO x) where
  _ == _ = False
