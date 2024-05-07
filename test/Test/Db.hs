module Test.Db where

import Database.SQLite.Simple
import Servant

resetDb :: Connection -> IO NoContent
resetDb c = NoContent <$ withTransaction c (
  do
    execute_ c "delete from user_login"
    execute_ c "delete from user"
    execute_ c "delete from entry"
  )