module Website.Data.Error where
import           Control.Monad.Except (MonadError, throwError)
import           Prelude              (Either, Eq, Maybe, Ord, Show, String,
                                       either, id, pure, (.))

--
-- Mapping application errors to servant errors
--
data Err
  = DbError DbErr
  | Unauthenticated
  | Unauthorised
  | MissingUpload
  | TooManyUploads
  | Other String
  deriving (Eq, Ord, Show)
data DbErr
  = NotFound
  | TooManyResults
  | FailedToInsertRecord
  deriving (Eq, Ord, Show)

class AsErr e where
  fromErr :: Err -> e
  toErr :: e -> Maybe Err
instance AsErr Err where
  fromErr = id
  toErr = pure

liftEither_ :: (MonadError e m, AsErr e) => Either Err a -> m a
liftEither_ = either (throwError . fromErr) pure

throwError_ :: (MonadError e m, AsErr e) => Err -> m a
throwError_ = throwError . fromErr

-- User lookups can fail in a couple of ways,
-- but these are the ones we really care to catch.
-- A missing user at this stage means that the auth
-- token was valid as far as the crypto was concerned
-- but the ID it encoded no longer exists, so the
-- user shouldn't be authenticated any more.
userLookupErrors :: Err -> Err
userLookupErrors (DbError NotFound)       = Unauthenticated
userLookupErrors (DbError TooManyResults) = Unauthenticated
userLookupErrors e                        = e
