{-# LANGUAGE TypeFamilies #-}
-- FromField for UUIDs is an instance we need to write ourselves
-- so that we aren't explicitly wrapping and unwrapping everywhere.
{-# OPTIONS_GHC -Wno-orphans #-}

module Website.Data.User where

import Control.Monad.Except
import Control.Monad.Reader.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Password.Argon2 (Argon2, PasswordHash (..), hashPassword, mkPassword, unPasswordHash)
import Data.Text hiding (group)
import Data.UUID
import Data.UUID.V4
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import GHC.Generics
import Website.Data.Util
import Servant
import Servant.Auth.JWT
import Web.FormUrlEncoded
import Website.Auth.Authorisation (Group)
import Website.Types
import Website.Data.Env
import Website.Data.Error

-- Known orphan instances. These are here so that we don't have to
-- constantly wrap and unwrap (either a newtype or text) everywhere.
instance FromField UUID where
  fromField :: FieldParser UUID
  fromField = fromField @String >=> maybe (fail "Could not parse UUID") pure . fromString

instance ToField UUID where
  toField = toField @String . toString

instance FromRow UUID where
  fromRow = fieldWith fromField

newtype UserKey = UserKey
  { uuid :: UUID
  }
  deriving (Eq, Ord, Show, Generic, FromField, ToField, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON)
instance FromRow UserKey
instance ToJWT UserKey
instance FromJWT UserKey

data User = User
  { uuid :: UserKey,
    email :: Text,
    group :: Group
  }
  deriving (Eq, Show, Generic)
instance FromRow User where
  fromRow = User <$> field <*> field <*> field

data UserUpdate = UserUpdate
  { password :: Maybe Text,
    group :: Maybe Group
  }
  deriving (Generic)

instance FromForm UserUpdate where
  fromForm f =
    UserUpdate
      <$> parseUnique "password" f
      <*> parseUnique "group" f

data UserCreate = UserCreate
  { group :: Group,
    email :: Text,
    password :: Text
  }
  deriving (Generic)

instance FromForm UserCreate where
  fromForm f =
    UserCreate
      <$> parseUnique "group" f
      <*> parseUnique "email" f
      <*> parseUnique "password" f

instance FromField (PasswordHash Argon2) where
  fromField f = PasswordHash <$> fromField @Text f

instance FromRow (PasswordHash Argon2) where
  fromRow = PasswordHash <$> field @Text

instance ToField (PasswordHash Argon2) where
  toField = toField . unPasswordHash

createUser :: (CanAppM Env Err m) => UserCreate -> m User
createUser (UserCreate group email password) = do
  c <- asks conn
  userId <- liftIO nextRandom
  user <-
    liftIO (query c "insert into user(id, email, group_name) values (?, ?, ?) returning id, email, group_name" (userId, email, group))
      >>= ensureSingleInsert
      >>= liftEither
  hash <- hashPassword $ mkPassword password
  liftIO $ execute c "insert into user_login(id, hash) values (?, ?)" (userId, hash)
  pure user

getUserHash :: Connection -> UserKey -> IO (Either Err (PasswordHash Argon2))
getUserHash c uid =
  query c "select hash from user_login where id = ?" (Only uid)
    >>= ensureSingleResult

getUser :: Connection -> UserKey -> IO (Either Err User)
getUser c uid =
  query c "select id, email, group_name from user where id = ?" (Only uid)
    >>= ensureSingleResult

getUserId :: Connection -> Text -> IO (Either Err UserKey)
getUserId c email =
  query c "select id from user where email = ?" (Only email)
    >>= ensureSingleResult