module Website.Data.Error where

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
