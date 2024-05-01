module Website.Data.Error where
import Servant

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