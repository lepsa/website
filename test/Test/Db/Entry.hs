{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Db.Entry where

import           Data.Aeson
import           Database.SQLite.Simple
import           Website.Data.Entry

instance ToJSON Entry
instance FromJSON Entry
instance ToJSON EntryKey
instance FromJSON EntryKey

getAllEntries :: Connection -> IO [Entry]
getAllEntries c = query_ c "select key, created, title, value, updated from entry"
