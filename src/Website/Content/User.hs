module Website.Content.User where

import Text.Blaze.Html
import Control.Monad.Reader
import Website.Data.User
import Website.Content.Common
import Website.Data.Env

userDisplay :: MonadReader Env m => User -> m Html
userDisplay _ = pure mempty

userDisplayFullPage :: MonadReader Env m => Authed -> User -> m Html
userDisplayFullPage auth = fmap (basicPage auth) . userDisplay

userList :: MonadReader Env m => [User] -> m Html
userList _users = do
  pure mempty