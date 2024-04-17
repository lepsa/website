module Website.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Database.SQLite.Simple
import Servant

newtype Env = Env
  { conn :: Connection
  }

data Err
  = NotFound
  | TooManyResults
  deriving (Eq, Ord, Show)

errToServerError :: Err -> ServerError
errToServerError NotFound = err404
errToServerError TooManyResults = err404

type AppM c e m a = ReaderT c (ExceptT e m) a

type CanAppM c e m = (MonadReader c m, MonadError e m, MonadIO m)

runAppM :: c -> AppM c e m a -> m (Either e a)
runAppM c m = runExceptT $ runReaderT m c

runAppMToHandler :: c -> AppM c Err IO a -> Handler a
runAppMToHandler c m = do
  e <- liftIO $ runExceptT $ runReaderT m c
  either (throwError . errToServerError) pure e