-- FromField for UUIDs is an instance we need to write ourselves
-- so that we aren't explicitly wrapping and unwrapping everywhere.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Website.Data.User where

import Data.UUID
import Website.Auth.Authorisation (Group)
import Data.Text hiding (group)
import GHC.Generics
import Servant.Auth.JWT
import Servant.Auth.Server
import Data.UUID.V4
import Database.SQLite.Simple
import Website.Types
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Password.Argon2 (hashPassword, mkPassword, unPasswordHash, checkPassword, PasswordCheck (..), Argon2, PasswordHash (..))
import Control.Monad.Except (MonadError(throwError), runExceptT, liftEither, ExceptT)
import Data.Text.Encoding (decodeUtf8')
import Database.SQLite.Simple.FromField
import Control.Monad
import Database.SQLite.Simple.ToField
import Website.Data.Common (ensureSingleResult, ensureSingleInsert)
import Data.Bifunctor
import Database.SQLite.Simple.FromRow
import Web.FormUrlEncoded

-- Known orphan instances. These are here so that we don't have to
-- constantly wrap and unwrap (either a newtype or text) everywhere.
instance FromField UUID where
  fromField :: FieldParser UUID
  fromField = fromField @String >=> maybe (fail "Could not parse UUID") pure . fromString
instance ToField UUID where
  toField = toField @String . toString
instance FromRow UUID where
  fromRow = fieldWith fromField

type UserId = UUID
instance ToJWT UserId
instance FromJWT UserId

data User = User
  { uuid :: UUID
  , email :: Text
  , group :: Group
  } deriving (Eq, Show, Generic)

data UserUpdate = UserUpdate
  { password :: Maybe Text
  , group :: Maybe Group
  } deriving Generic
instance FromForm UserUpdate where
  fromForm f = UserUpdate
    <$> parseUnique "password" f
    <*> parseUnique "group" f

data UserCreate = UserCreate
  { group :: Group
  , email :: Text
  , password :: Text
  } deriving Generic
instance FromForm UserCreate where
  fromForm f = UserCreate
    <$> parseUnique "group" f
    <*> parseUnique "email" f
    <*> parseUnique "password" f

newtype BasicAuthCfg' = BasicAuthCfg' Connection
type instance BasicAuthCfg = BasicAuthCfg'

instance FromBasicAuthData UserId where
  -- If anything goes wrong at any point, return BadPassword.
  -- This won't save us from timing attacks, but it'll do for now.
  -- NOTE: Attackers could guess at what we are doing based on the
  -- time it takes to return a result. This could allow them to
  -- guess how far through these checks they made it before being
  -- kicked out.
  fromBasicAuthData (BasicAuthData user pass) (BasicAuthCfg' conn) = do
    either pure (pure . Authenticated) <=< runExceptT $ do
      -- Decode the email into a friendlier type
      email <- either (const $ throwError BadPassword) pure (decodeUtf8' user)
      -- Convert the type for the hash
      password <- either (const $ throwError BadPassword) pure (decodeUtf8' pass)
      checkUserPassword conn email password

checkUserPassword :: MonadIO m => Connection -> Text -> Text -> ExceptT (AuthResult UserId) m UserId
checkUserPassword conn email pass = do
  -- Look up the user's id by their email
  uid <- liftIO (getUserId conn email) >>= liftEither .  first (const BadPassword)
  -- Get the associated password hash
  hash <- liftIO (getUserHash conn uid) >>= liftEither . first (const BadPassword)
  -- Run the password check
  case checkPassword (mkPassword pass) hash of
    PasswordCheckFail -> throwError BadPassword
    -- If the password was correct, fetch the user
    PasswordCheckSuccess -> pure uid

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

createUser :: (CanAppM Env Err m) => UserCreate -> m User
createUser (UserCreate group email password) = do
  c <- asks conn
  userId <- liftIO nextRandom
  user <- liftIO (query c "insert into user(id, email, group_name) values (?, ?, ?) returning id, email, group_name" (userId, email, group))
    >>= ensureSingleInsert
    >>= liftEither
  hash <- hashPassword $ mkPassword password
  liftIO $ execute c "insert into user_login(id, hash) values (?, ?)" (userId, hash)
  pure user

instance FromField (PasswordHash Argon2) where
  fromField f = PasswordHash <$> fromField @Text f

instance FromRow (PasswordHash Argon2) where
  fromRow = PasswordHash <$> field @Text

instance ToField (PasswordHash Argon2) where
  toField = toField . unPasswordHash


getUserHash :: Connection -> UUID -> IO (Either Err (PasswordHash Argon2))
getUserHash c uid = query c "select hash from user_login where id = ?" (Only uid) >>=
  ensureSingleResult

getUser :: Connection -> UUID -> IO (Either Err User)
getUser c uid = query c "select id, email, group_name from user where id = ?" (Only uid)
  >>= ensureSingleResult

getUserId :: Connection -> Text -> IO (Either Err UUID)
getUserId c email = query c "select id from user where email = ?" (Only email)
  >>= ensureSingleResult