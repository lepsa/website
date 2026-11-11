{-# OPTIONS_GHC -Wno-orphans #-}

module Website.Types where

import           Control.Monad                    ((>=>))
import           Control.Monad.Except             (ExceptT, MonadError,
                                                   runExceptT, throwError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Logger             (Loc (loc_module, loc_start),
                                                   LogLevel (LevelDebug, LevelError, LevelInfo, LevelOther, LevelWarn),
                                                   LoggingT (runLoggingT),
                                                   MonadLogger, fromLogStr)
import           Control.Monad.Reader             (MonadReader, Reader,
                                                   ReaderT (runReaderT),
                                                   runReader)
import           Data.Password.Argon2             (Argon2,
                                                   PasswordHash (PasswordHash),
                                                   unPasswordHash)
import           Data.Text                        (Text, unpack)
import           Data.Text.Encoding               (decodeUtf8Lenient)
import           Data.UUID                        (UUID, fromString, toString)
import           Database.SQLite.Simple           (FromRow, field)
import           Database.SQLite.Simple.FromField (FieldParser, FromField,
                                                   fromField)
import           Database.SQLite.Simple.FromRow   (fieldWith, fromRow)
import           Database.SQLite.Simple.ToField   (ToField, toField)
import           Prelude                          (Either, IO, String, either,
                                                   fail, flip, fst, maybe, pure,
                                                   read, show, ($), (.), (<$>),
                                                   (<>))
import           Servant                          (Handler, ServerError)
import           Servant.Auth.JWT                 (FromJWT, ToJWT)
import           System.Log.Logger                (Priority (DEBUG, ERROR, INFO, WARNING),
                                                   logM)
import           Text.Blaze                       (ToMarkup, toMarkup)
import           Website.Data.Env                 (HasEnv)
import           Website.Data.Error               (AsErr, Err)



--
-- Application monad stack and type constraints.
-- Helper functions when working with Servant.
--
type AppM c e m = ReaderT c (ExceptT e (LoggingT m))

type CanAppM c e m = (HasEnv c, AsErr e, MonadReader c m, MonadError e m, MonadLogger m, MonadIO m)
-- type CanAppM' a c e m = (HasAuth c a, HasEnv c, AsErr e, MonadReader c m, MonadError e m, MonadIO m)

runAppM :: c -> AppM c e m a -> m (Either e a)
runAppM c m = flip runLoggingT go $ runExceptT $ runReaderT m c
  where
    go loc logSource logLevel logStr = logM
      (loc_module loc)
      (priority logLevel)
      $  unpack logSource
      <> "Line "
      <> show (fst $ loc_start loc)
      <> ": "
      <> unpack (decodeUtf8Lenient $ fromLogStr logStr)
    priority l = case l of
      LevelDebug   -> DEBUG
      LevelInfo    -> INFO
      LevelWarn    -> WARNING
      LevelError   -> ERROR
      LevelOther t -> read $ unpack t


runAppMToHandler :: (Err -> Reader c ServerError) -> c -> AppM c Err IO a -> Handler a
runAppMToHandler f c m = do
  e <- liftIO $ runAppM c m
  either (throwError . flip runReader c . f) pure e

-- Orphan instances for various things that we need

instance ToMarkup () where
    toMarkup = toMarkup . show

instance FromField (PasswordHash Argon2) where
  fromField f = PasswordHash <$> fromField @Text f

instance FromRow (PasswordHash Argon2) where
  fromRow = PasswordHash <$> field @Text

instance ToField (PasswordHash Argon2) where
  toField = toField . unPasswordHash

instance FromField UUID where
  fromField :: FieldParser UUID
  fromField = fromField @String >=> maybe (fail "Could not parse UUID") pure . fromString
instance FromRow UUID where
  fromRow = fieldWith fromField
instance ToField UUID where
  toField = toField @String . toString
instance ToJWT UUID
instance FromJWT UUID
