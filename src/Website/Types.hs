module Website.Types where

import           Control.Monad.Except   (ExceptT, MonadError, runExceptT,
                                         throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger   (Loc (loc_module, loc_start),
                                         LogLevel (LevelDebug, LevelError, LevelInfo, LevelOther, LevelWarn),
                                         LoggingT (runLoggingT), MonadLogger,
                                         fromLogStr)
import           Control.Monad.Reader   (MonadReader, Reader,
                                         ReaderT (runReaderT), runReader)
import           Data.Text              (unpack)
import           Data.Text.Encoding     (decodeUtf8Lenient)
import           Prelude                (Either, IO, either, flip, fst, pure,
                                         read, show, ($), (.), (<>))
import           Servant                (Handler, ServerError)
import           System.Log.Logger      (Priority (DEBUG, ERROR, INFO, WARNING),
                                         logM)
import           Website.Data.Env       (HasEnv)
import           Website.Data.Error     (AsErr, Err)

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
