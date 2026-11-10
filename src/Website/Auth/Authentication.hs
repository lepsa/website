{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Website.Auth.Authentication where

import           Control.Monad          (Monad ((>>=)), (<=<))
import           Control.Monad.Except   (ExceptT, MonadError (throwError),
                                         liftEither, runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bifunctor         (Bifunctor (first))
import           Data.Password.Argon2   (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess),
                                         checkPassword, mkPassword)
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8')
import           Database.SQLite.Simple (Connection)
import           GHC.Generics           (Generic)
import           Prelude                (String, const, either, pure, ($), (.),
                                         (<$>), (<*>))
import           Servant.Auth.Server    (AuthResult (Authenticated, BadPassword),
                                         BasicAuthCfg,
                                         BasicAuthData (BasicAuthData),
                                         FromBasicAuthData, fromBasicAuthData)
import           Web.FormUrlEncoded     (FromForm, fromForm, parseUnique)
import           Website.Data.User      (UserKey, getUserHashIO, getUserIdIO)

data Login = Login
  { email    :: String,
    password :: String
  }
  deriving (Generic)

instance FromForm Login where
  fromForm f = Login
    <$> parseUnique "login" f
    <*> parseUnique "password" f

newtype BasicAuthCfg' = BasicAuthCfg' Connection

type instance BasicAuthCfg = BasicAuthCfg'

instance FromBasicAuthData UserKey where
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

checkUserPassword :: (MonadIO m) => Connection -> Text -> Text -> ExceptT (AuthResult UserKey) m UserKey
checkUserPassword conn email pass = do
  -- Look up the user's id by their email
  uid <- liftIO (getUserIdIO conn email) >>= liftEither . first (const BadPassword)
  -- Get the associated password hash
  hash <- liftIO (getUserHashIO conn uid) >>= liftEither . first (const BadPassword)
  -- Run the password check
  case checkPassword (mkPassword pass) hash of
    PasswordCheckFail    -> throwError BadPassword
    -- If the password was correct, fetch the user
    PasswordCheckSuccess -> pure uid
