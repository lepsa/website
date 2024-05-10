module Test.Db where

import           Data.Foldable
import           Database.SQLite.Simple
import           Servant

resetDb :: Connection -> IO NoContent
resetDb c = NoContent <$ withTransaction c go
  where
    go = traverse_ (execute_ c)
      [ "delete from user_login"
      , "delete from user"
      , "delete from entry"
      , "delete from file"
      ]
