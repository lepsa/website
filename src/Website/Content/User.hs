module Website.Content.User where

import Text.Blaze.Html
import Control.Monad.Reader
import Website.Data.User
import Website.Content.Common
import Website.Data.Env

userDisplay :: MonadReader Env m => User -> m Html
userDisplay _ = pure mempty

userDisplayFullPage :: MonadReader Env m => User -> m Html
userDisplayFullPage = fmap basicPage . userDisplay

userList :: MonadReader Env m => [User] -> m Html
userList _users = do
  pure mempty