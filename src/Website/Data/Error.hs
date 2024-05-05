module Website.Data.Error where
import Control.Monad.Except

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