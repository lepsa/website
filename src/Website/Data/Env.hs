module Website.Data.Env where
import Database.SQLite.Simple
import Data.Time

--
-- Server configration values
--
data Env = Env
  { conn :: Connection
  , timeZone :: TimeZone
  }
