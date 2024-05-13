{-# LANGUAGE FunctionalDependencies #-}
module Website.Data.Env where
import           Data.Time
import           Database.SQLite.Simple

--
-- Server configration values
--
data Env = Env
  { _envConn     :: Connection
  , _envTimeZone :: TimeZone
  }

class HasEnv c where
  {-# MINIMAL (env | conn, timeZone) #-}
  env :: c -> Env
  env c = Env (conn c) (timeZone c)
  conn :: c -> Connection
  conn = conn . env
  timeZone :: c -> TimeZone
  timeZone = timeZone . env

instance HasEnv Env where
  env = id
  conn = _envConn
  timeZone = _envTimeZone

class HasAuth c a | c -> a where
  auth :: c -> a

type EnvAuthed a = (Env, a)

instance HasEnv (EnvAuthed a) where
  env = fst

instance HasAuth (EnvAuthed a) a where
  auth = snd

mkEnvAuthed :: a -> Env -> EnvAuthed a
mkEnvAuthed a e = (e, a)
