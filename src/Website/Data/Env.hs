module Website.Data.Env where
import Database.SQLite.Simple
import Data.Time

--
-- Server configration values
--
data Env = Env
  { _conn :: Connection
  , _timeZone :: TimeZone
  }

class HasEnv c where
  env :: c -> Env
  conn :: c -> Connection
  timeZone :: c -> TimeZone

instance HasEnv Env where
  env = id
  conn = _conn
  timeZone = _timeZone