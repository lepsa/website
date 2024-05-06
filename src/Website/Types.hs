module Website.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Servant
import Website.Data.Env
import Website.Data.Error

--
-- Application monad stack and type constraints.
-- Helper functions when working with Servant.
--
type AppM c e m = ReaderT c (ExceptT e m)

type CanAppM c e m = (HasEnv c, AsErr e, MonadReader c m, MonadError e m, MonadIO m)
-- type CanAppM' a c e m = (HasAuth c a, HasEnv c, AsErr e, MonadReader c m, MonadError e m, MonadIO m)

runAppM :: c -> AppM c e m a -> m (Either e a)
runAppM c m = runExceptT $ runReaderT m c

runAppMToHandler :: (Err -> Reader c ServerError) -> c -> AppM c Err IO a -> Handler a
runAppMToHandler f c m = do
  e <- liftIO $ runAppM c m
  either (throwError . flip runReader c . f) pure e