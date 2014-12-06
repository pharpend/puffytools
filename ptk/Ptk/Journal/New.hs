{- |
Module       : Ptk.Journal.New
Description  : `ptk journal new` command tree
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module Ptk.Journal.New (journalNewTree, journalNewHelp) where

import           Data.Text (Text, pack, unpack)
import           PuffyTools.Journal
import           PuffyTools.Slug
import           System.Console.Argument 
import           System.Console.Command hiding (name)
import           System.Console.Program

journalNewHelp :: Action IO
journalNewHelp = io $ showUsage journalNewTree

-- |This is basically the tree of things that get parsed when the
-- luser runs "ptk journal new"
journalNewTree :: Commands IO
journalNewTree = Node newCommand []
  where
    -- This is a "Command" object - basically says that the subcommand
    -- is "new", and it has an associated description.
    newCommand = Command "new" "Create a new journal" $ withOption slugOption doSlug
    -- This is an option to specify the name of the journal
    slugOption = option "sn" ["slug", "name"] string "default"
                   "The slug/short name of the journal."
    -- title
    titleOption = option "t" ["title"] string "" "The title of the journal"
    -- title
    descOption = option "d" ["description"] string "" "The journal Description"

    doSlug slug = withOption titleOption (doTitle slug)
    doTitle slug title = withOption descOption (runNJ slug title)

slugType :: Type Slug
slugType = Type {parser = \s -> mkSlugEither (pack s)
                ,name = "slug"
                ,defaultValue = Nothing}
-- |Run the `ptk journal new` command
runNJ
  :: String                     -- ^The slug
  -> String                     -- ^The title
  -> String                     -- ^The description
  -> Action IO                  -- ^The resultant action
runNJ sslug stitle sdesc = io $ do
  let slug = pack sslug
      title = pack stitle
      desc = case pack sdesc of
        "" -> Nothing
        x  -> Just x
  ifJournal slug $ do
    fail "Journal already exists"
  mdJournal <- mkJournal =<< mkSlugIO slug
  let newJournal = mdJournal { journalTitle = title }
  writeJournal newJournal
