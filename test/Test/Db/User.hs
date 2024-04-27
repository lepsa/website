{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Db.User where

import Website.Data.User
import Database.SQLite.Simple
import Data.Aeson

-- Orphan instances are here as we don't want nor need them in the main server
instance ToJSON User
instance FromJSON User

getAllUsers :: Connection -> IO [User]
getAllUsers c = query_ c "select id, email, group_name from user"