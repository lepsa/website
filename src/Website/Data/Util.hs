
module Website.Data.Util where

import           Control.Monad.Except        (MonadError (throwError))
import           Data.Time                   (TimeZone, UTCTime,
                                              defaultTimeLocale, formatTime,
                                              utcToZonedTime)
import           Data.Time.Format.ISO8601    (iso8601Show)
import           Prelude                     (Either (Left), either, pure, ($),
                                              (.))
import           Text.Blaze.Html             (Html, stringValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Website.Data.Error          (AsErr (fromErr),
                                              DbErr (FailedToInsertRecord, NotFound, TooManyResults),
                                              Err (DbError, Other))

-- FromField for UUIDs is an instance we need to write ourselves
-- so that we aren't explicitly wrapping and unwrapping everywhere.

timeFormat :: TimeZone -> UTCTime -> Html
timeFormat tz t = H.time
  ! HA.datetime (stringValue $ iso8601Show zt)
  $ H.string $ formatTime defaultTimeLocale "%e %B, %Y" zt
  where
    zt = utcToZonedTime tz t

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
