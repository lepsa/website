{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Website.Data.User where

import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.Password.Argon2             (Argon2, PasswordCheck (..), PasswordHash (..), checkPassword,
                                                   hashPassword, mkPassword, unPasswordHash)
import           Data.Text                        hiding (group)
import           Data.UUID
import           Data.UUID.V4
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField   (ToField (..))
import           GHC.Generics
import           Web.FormUrlEncoded
import           Website.Auth.Authorisation       (Group (Admin))
import           Website.Data.Env
import           Website.Data.Error
import           Website.Data.Util
import           Website.Types
import Control.Monad.Logger
import qualified Data.Text as T

-- Known orphan instances. These are here so that we don't have to
-- constantly wrap and unwrap (either a newtype or text) everywhere.
type UserKey = UUID

newtype UserLogin = UserLogin { unUserLogin :: User }
  deriving (Eq, Show, Generic)
instance FromRow UserLogin where
  fromRow = UserLogin <$> fromRow

class (HasEnv c, HasAuth c UserLogin, OptionalUser c) => RequiredUser c
instance RequiredUser (EnvAuthed UserLogin)

class HasEnv c => OptionalUser c where
  mAuth :: c -> Maybe UserLogin

instance OptionalUser Env where
  mAuth _ = Nothing

instance OptionalUser (EnvAuthed UserLogin) where
  mAuth = pure . auth
instance OptionalUser (EnvAuthed (Maybe UserLogin)) where
  mAuth = auth

data User = User
  { uuid  :: UserKey,
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
  deriving (Generic, Show)

instance FromForm UpdatePassword where
  fromForm :: Form -> Either Text UpdatePassword
  fromForm f = UpdatePassword
    <$> parseUnique "oldPassword" f
    <*> parseUnique "newPassword" f

data UserUpdate = UserUpdate
  { password :: Maybe UpdatePassword,
    group    :: Maybe Group
  }
  deriving (Generic, Show)

instance FromForm UserUpdate where
  fromForm f = do
    old <- parseMaybe "oldPassword" f
    new <- parseMaybe "newPassword" f
    group <- parseMaybe "group" f
    let password = UpdatePassword <$> old <*> new
    pure $ UserUpdate password group

data UserCreate = UserCreate
  { group    :: Group,
    email    :: Text,
    password :: Text
  }
  deriving (Generic)

instance FromForm UserCreate where
  fromForm f = UserCreate
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
  $(logDebug) "createUser"
  c <- asks conn
  userId <- liftIO nextRandom
  eUser <- liftIO $ withTransaction c $ do
    eUser <- ensureSingleInsert <$> query c "insert into user(id, email, group_name) values (?, ?, ?) returning id, email, group_name" (userId, email, group)
    case eUser of
      (Right _user) -> eUser <$ do
        hash <- hashPassword $ mkPassword password
        execute c "insert into user_login(id, hash) values (?, ?)" (userId, hash)
      _ -> pure eUser
  either throwError_ pure eUser

getUserHashIO :: Connection -> UserKey -> IO (Either Err (PasswordHash Argon2))
getUserHashIO c uid = ensureSingleResult <$> query c "select hash from user_login where id = ?" (Only uid)

getUserHash :: (CanAppM c e m) => UserKey -> m (PasswordHash Argon2)
getUserHash uid = do
  $(logDebug) $ "getUserHash " <> T.pack (show uid)
  c <- asks conn
  e <- liftIO (getUserHashIO c uid)
  liftEither_ e

getUserIO :: Connection -> UserKey -> IO (Either Err User)
getUserIO c uid = ensureSingleResult <$> query c "select id, email, group_name from user where id = ?" (Only uid)

getUser :: (CanAppM c e m) => UserKey -> m User
getUser uid = do
  $(logDebug) $ "getUser " <> T.pack (show uid)
  c <- asks conn
  e <- liftIO $ getUserIO c uid
  liftEither_ e

getUserIdIO :: Connection -> Text -> IO (Either Err UserKey)
getUserIdIO c email = ensureSingleResult <$> query c "select id from user where email = ?" (Only email)

getUserId :: (CanAppM c e m) => Text -> m UserKey
getUserId email = do
  $(logDebug) $ "getUserId " <> T.pack (show email)
  c <- asks conn
  e <- liftIO $ getUserIdIO c email
  liftEither_ e

getUserLoginIO :: Connection -> UserKey -> IO (Either Err UserLogin)
getUserLoginIO c k = ensureSingleResult <$> query c "select id, email, group_name from user where id = ?" (Only k)

getUserLogin :: CanAppM c e m => UserKey -> m UserLogin
getUserLogin k = do
  $(logDebug) $ "getUserLogin " <> T.pack (show k)
  c <- asks conn
  e <- liftIO $ getUserLoginIO c k
  liftEither_ e

updateUser :: (CanAppM c e m) => UserKey -> UserUpdate -> m User
updateUser key update = do
  $(logDebug) $ "updateUser " <> T.pack (show key)
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
  $(logDebug) $ "deleteUser " <> T.pack (show key)
  c <- asks conn
  liftIO $
    withTransaction c $
      do
        execute c "delete from user_login where id = ?" (Only key)
        execute c "delete from user where id = ?" (Only key)

getUsers :: CanAppM c e m => m [User]
getUsers = do
  $(logDebug) "getUsers"
  c <- asks conn
  liftIO $ withTransaction c $ query_ c "select id, email, group_name from user"

adminExists :: CanAppM c e m => m Bool
adminExists = do
  $(logDebug) "adminExists"
  c <- asks conn
  liftIO $ do
    l :: [User] <- query c "select id, email, group_name from user where group_name = ? limit 1" (Only Admin)
    case l of
      [] -> pure False
      _  -> pure True
