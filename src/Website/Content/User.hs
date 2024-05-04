module Website.Content.User where

import Text.Blaze.Html
import Control.Monad.Reader
import Website.Data.User
import Website.Content.Common
import Website.Data.Env

userDisplay :: (HasEnv c, MonadReader c m) => User -> m Html
userDisplay _ = pure mempty

userDisplayFullPage :: (HasEnv c, MonadReader c m) => Authed -> User -> m Html
userDisplayFullPage auth = fmap (basicPage auth) . userDisplay

userList :: (HasEnv c, MonadReader c m) => [User] -> m Html
userList _users = do
  pure mempty