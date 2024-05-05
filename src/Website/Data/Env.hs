{-# LANGUAGE FunctionalDependencies #-}
module Website.Data.Env where
import Database.SQLite.Simple
import Data.Time

--
-- Server configration values
--
data Env = Env
  { _envConn :: Connection
  , _envTimeZone :: TimeZone
  }

class HasEnv c where
  env :: c -> Env
  env c = Env (conn c) (timeZone c)
  conn :: c -> Connection
  timeZone :: c -> TimeZone

instance HasEnv Env where
  env = id
  conn = _envConn
  timeZone = _envTimeZone

class HasAuth c a | c -> a where
  auth :: c -> a

data EnvAuthed a = EnvAuthed
  { _envAuthConn :: Connection
  , _envAuthTimeZone :: TimeZone
  , _envAuthAuthed :: a
  } deriving Functor

instance HasEnv (EnvAuthed a) where
  conn = _envAuthConn
  timeZone = _envAuthTimeZone

instance HasAuth (EnvAuthed a) a where
  auth = _envAuthAuthed

mkEnvAuthed :: a -> Env -> EnvAuthed a
mkEnvAuthed a e = EnvAuthed (conn e) (timeZone e) a