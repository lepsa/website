module Website.Content.User where
import Text.Blaze.Html
import Website.Types
import Control.Monad.Reader
import Website.Data.User
import Website.Content.Common
import Data.Data
import Website.Data.Env
import Website.Data.Error
import Servant
import Data.Text
import qualified Text.Blaze.Html as H
import Website.Network.API.CRUD
import Website.Network.API.Types

userDisplay :: MonadReader Env m => User -> m Html
userDisplay _ = pure mempty

userDisplayFullPage :: MonadReader Env m => User -> m Html
userDisplayFullPage = fmap basicPage . userDisplay

userList :: MonadReader Env m => [User] -> m Html
userList _users = do
  pure mempty

postUser :: (CanAppM Env Err m) => UserCreate -> m (Headers '[Header "Location" Text] H.Html)
postUser create = do
  user <- createUser create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) user.uuid
  pure $ addHeader link mempty