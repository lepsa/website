{-# OPTIONS_GHC -Wno-orphans #-}

module Website.Data.Util where

import           Control.Monad                    (MonadFail (fail), (>=>))
import           Control.Monad.Except             (MonadError (throwError))
import           Data.Time                        (TimeZone, UTCTime,
                                                   defaultTimeLocale,
                                                   formatTime, utcToZonedTime)
import           Data.UUID                        (UUID, fromString, toString)
import           Database.SQLite.Simple           (FromRow, fromRow)
import           Database.SQLite.Simple.FromField (FieldParser, FromField,
                                                   fromField)
import           Database.SQLite.Simple.FromRow   (fieldWith)
import           Database.SQLite.Simple.ToField   (ToField, toField)
import           Prelude                          (Either (Left), String,
                                                   either, maybe, pure, ($),
                                                   (.))
import           Servant.Auth.JWT                 (FromJWT, ToJWT)
import           Website.Data.Error               (AsErr (fromErr),
                                                   DbErr (FailedToInsertRecord, NotFound, TooManyResults),
                                                   Err (DbError, Other))

-- FromField for UUIDs is an instance we need to write ourselves
-- so that we aren't explicitly wrapping and unwrapping everywhere.
instance FromField UUID where
  fromField :: FieldParser UUID
  fromField = fromField @String >=> maybe (fail "Could not parse UUID") pure . fromString
instance FromRow UUID where
  fromRow = fieldWith fromField
instance ToField UUID where
  toField = toField @String . toString
instance ToJWT UUID
instance FromJWT UUID

timeFormat :: TimeZone -> UTCTime -> String
timeFormat tz = formatTime defaultTimeLocale "%e %B, %Y" . utcToZonedTime tz

ensureSingleInsert :: (AsErr e, MonadError e m) => [a] -> m a
ensureSingleInsert = either (throwError . fromErr) pure . ensureSingleInsert'

ensureSingleResult :: (AsErr e, MonadError e m) => [a] -> m a
ensureSingleResult = either (throwError . fromErr) pure . ensureSingleResult'

ensureSingleResult' :: [a] -> Either Err a
ensureSingleResult' []  = Left $ DbError NotFound
ensureSingleResult' [a] = pure a
ensureSingleResult' _   = Left $ DbError TooManyResults

ensureSingleInsert' :: [a] -> Either Err a
ensureSingleInsert' []  = Left $ DbError FailedToInsertRecord
ensureSingleInsert' [a] = pure a
ensureSingleInsert' _   = Left $ Other "insert query returned multiple rows"
