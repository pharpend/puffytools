{- |
Module       : Ptk.Journal.Cat
Description  : `ptk journal cat` command tree
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Ptk.Journal.Cat (journalCatTree, journalCatHelp) where

import           Control.Applicative
import qualified Data.ByteString as B
import           Data.Text (Text, pack, unpack)
import           PuffyTools.Journal
import           PuffyTools.Slug
import           System.Console.Argument 
import           System.Console.Command hiding (name)
import           System.Console.Program

journalCatHelp :: Action IO
journalCatHelp = io $ showUsage journalCatTree

-- |This is basically the tree of things that get parsed when the
-- luser runs "ptk journal cat"
journalCatTree :: Commands IO
journalCatTree = Node catCommand []
  where
    -- This is a "Command" object - basically says that the subcommand
    -- is "cat", and it has an associated description.
    catCommand = Command "cat" "Output the raw journal" $ withNonOption slugType runCat

slugType :: Type Slug
slugType = Type {parser = \s -> mkSlugEither (pack s)
                ,name = "slug"
                ,defaultValue = Nothing}
                
-- |Run the `ptk journal cat` command
runCat :: Slug -> Action IO               
runCat slug = io $ do
  B.putStrLn =<< B.readFile =<< generateSlugPath slug
