module Website.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Database.SQLite.Simple
import Servant
import Data.Time.LocalTime

--
-- Server configration values
--
data Env = Env
  { conn :: Connection
  , timeZone :: TimeZone
  }

--
-- Mapping application errors to servant errors
--
data Err
  = DbError DbErr
  | Unauthenticated
  | Unauthorised
  | Other String
  deriving (Eq, Ord, Show)
data DbErr
  = NotFound
  | TooManyResults
  | FailedToInsertRecord
  deriving (Eq, Ord, Show)

errToServerError :: Err -> ServerError
errToServerError (DbError e) = dbErrToServerError e
errToServerError Unauthenticated = err401
errToServerError Unauthorised = err403
errToServerError (Other _) = err500

dbErrToServerError :: DbErr -> ServerError
dbErrToServerError NotFound = err404
dbErrToServerError TooManyResults = err404
dbErrToServerError FailedToInsertRecord = err500

--
-- Application monad stack and type constraints.
-- Helper functions when working with Servant.
--
type AppM c e m = ReaderT c (ExceptT e m)

type CanAppM c e m = (MonadReader c m, MonadError e m, MonadIO m)

runAppM :: c -> AppM c e m a -> m (Either e a)
runAppM c m = runExceptT $ runReaderT m c

runAppMToHandler :: c -> AppM c ServerError IO a -> Handler a
runAppMToHandler c m = do
  e <- liftIO $ runAppM c m
  either throwError pure e