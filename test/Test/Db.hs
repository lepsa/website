module Test.Db where

import           Data.Foldable          (traverse_)
import           Database.SQLite.Simple (Connection, execute_, withTransaction)
import           Prelude                (IO, (<$))
import           Servant                (NoContent (NoContent))

resetDb :: Connection -> IO NoContent
resetDb c = NoContent <$ withTransaction c go
  where
    go = traverse_ (execute_ c)
      [ "delete from user_login"
      , "delete from user"
      , "delete from entry"
      , "delete from file"
      ]
