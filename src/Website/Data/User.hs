{-# LANGUAGE TypeFamilies #-}
-- FromField for UUIDs is an instance we need to write ourselves
-- so that we aren't explicitly wrapping and unwrapping everywhere.
{-# OPTIONS_GHC -Wno-orphans #-}

module Website.Data.User where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Password.Argon2 (Argon2, PasswordCheck (..), PasswordHash (..), checkPassword, hashPassword, mkPassword, unPasswordHash)
import Data.Text hiding (group)
import Data.UUID
import Data.UUID.V4
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import GHC.Generics
import Servant.Auth.JWT
import Web.FormUrlEncoded
import Website.Auth.Authorisation (Group)
import Website.Data.Env
import Website.Data.Error
import Website.Data.Util
import Website.Types

-- Known orphan instances. These are here so that we don't have to
-- constantly wrap and unwrap (either a newtype or text) everywhere.
type UserKey = UUID
instance ToJWT UserKey
instance FromJWT UserKey
instance FromField UUID where
  fromField :: FieldParser UUID
  fromField = fromField @String >=> maybe (fail "Could not parse UUID") pure . fromString

instance ToField UUID where
  toField = toField @String . toString

instance FromRow UUID where
  fromRow = fieldWith fromField

data UserLogin = UserLogin
  { uuid :: UserKey
  , email :: Text
  } deriving (Eq, Show, Generic)
instance ToJSON UserLogin
instance FromJSON UserLogin
instance ToJWT UserLogin
instance FromJWT UserLogin

data User = User
  { uuid :: UserKey,
    email :: Text,
    group :: Group
  }
  deriving (Eq, Show, Generic)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

data UpdatePassword = UpdatePassword
  { oldPassword :: Text,
    newPassword :: Text
  }
  deriving (Generic)

instance FromForm UpdatePassword where
  fromForm f =
    UpdatePassword
      <$> parseUnique "oldPassword" f
      <*> parseUnique "newPassword" f

data UserUpdate = UserUpdate
  { password :: Maybe UpdatePassword,
    group :: Maybe Group
  }
  deriving (Generic)

instance FromForm UserUpdate where
  fromForm f = do
    old <- parseUnique "oldPassword" f
    new <- parseUnique "newPassword" f
    group <- parseUnique "group" f
    let password = UpdatePassword <$> old <*> new
    pure $ UserUpdate password group

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

createUser :: (CanAppM c e m) => UserCreate -> m User
createUser (UserCreate group email password) = do
  c <- asks conn
  userId <- liftIO nextRandom
  user <-
    liftIO (query c "insert into user(id, email, group_name) values (?, ?, ?) returning id, email, group_name" (userId, email, group))
      >>= ensureSingleInsert
      >>= liftEither_
  hash <- hashPassword $ mkPassword password
  liftIO $ execute c "insert into user_login(id, hash) values (?, ?)" (userId, hash)
  pure user

getUserHashIO :: Connection -> UserKey -> IO (Either Err (PasswordHash Argon2))
getUserHashIO c uid =
  query c "select hash from user_login where id = ?" (Only uid)
    >>= ensureSingleResult

getUserHash :: (CanAppM c e m) => UserKey -> m (PasswordHash Argon2)
getUserHash uid = asks conn >>= liftIO . flip getUserHashIO uid >>= liftEither_

getUserIO :: Connection -> UserKey -> IO (Either Err User)
getUserIO c uid =
  query c "select id, email, group_name from user where id = ?" (Only uid)
    >>= ensureSingleResult

getUser :: (CanAppM c e m) => UserKey -> m User
getUser uid = asks conn >>= liftIO . flip getUserIO uid >>= liftEither_

getUserIdIO :: Connection -> Text -> IO (Either Err UserKey)
getUserIdIO c email =
  query c "select id from user where email = ?" (Only email)
    >>= ensureSingleResult

getUserId :: (CanAppM c e m) => Text -> m UserKey
getUserId email = asks conn >>= liftIO . flip getUserIdIO email >>= liftEither_

updateUser :: (CanAppM c e m) => UserKey -> UserUpdate -> m User
updateUser key update = do
  c <- asks conn
  case update.group of
    Nothing -> pure ()
    Just group ->
      liftIO $
        withTransaction c $
          execute c "update user set group_name = ? where id = ?" (group, key)
  case update.password of
    Nothing -> pure ()
    Just (UpdatePassword old new) -> do
      oldHash <- getUserHash key
      case checkPassword (mkPassword old) oldHash of
        PasswordCheckFail -> throwError_ $ Other "Could not verify old password"
        PasswordCheckSuccess -> do
          newHash <- hashPassword $ mkPassword new
          liftIO $
            withTransaction c $
              execute c "update user_login set hash = ? where id = ?" (newHash, key)
  getUser key

deleteUser :: (CanAppM c e m) => UserKey -> m ()
deleteUser key = do
  c <- asks conn
  liftIO $
    withTransaction c $
      do
        execute c "delete from user where id = ?" (Only key)
        execute c "delete from user_login where id = ?" (Only key)