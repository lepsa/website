{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Db.Entry where

import Website.Data.Entry
import Database.SQLite.Simple
import Data.Aeson

instance ToJSON Entry
instance FromJSON Entry
instance ToJSON EntryKey
instance FromJSON EntryKey

getAllEntries :: Connection -> IO [Entry]
getAllEntries c = query_ c "select key, created, title, value from entry"