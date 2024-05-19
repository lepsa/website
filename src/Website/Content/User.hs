module Website.Content.User where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Data
import           Data.List                   (sortBy)
import           Data.Text
import qualified Data.Text                   as T
import           Servant
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Website.Content.Common
import           Website.Content.Htmx
import           Website.Data.Env
import           Website.Data.User
import           Website.Network.API.CRUD
import           Website.Network.API.Types
import           Website.Types

userDisplay :: (HasEnv c, MonadReader c m, MonadLogger m) => User -> m Html
userDisplay user = do
  $(logDebug) $ "userDisplay " <> T.pack (show user)
  pure $ H.div ! HA.id "user" $
    mconcat
    [ H.h3 $ toHtml user.email
    , H.p $ toHtml $ "Group " <> show user.group
    , editDeleteButtons
        "#user"
        (pure $ ": " <> user.email)
        (mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDUpdateForm UserKey))) user.uuid)
        (mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDDelete UserKey))) user.uuid)
    ]

userDisplayFullPage :: (CanAppM c e m, RequiredUser c) => User -> m Html
userDisplayFullPage u = do
  $(logDebug) $ "userDisplayFullPage " <> T.pack (show u)
  basicPage =<< userDisplay u

userList :: (CanAppM c e m, RequiredUser c) => [User] -> m Html
userList users = do
  $(logDebug) $ "userList " <> T.pack (show users)
  basicPage $ mconcat
    [ H.h2 "Users",
      newUser,
      H.ul $ mconcat $ userLink <$> sortUsers users
    ]
  where
    userLink :: User -> Html
    userLink user = H.li $ H.p $ mconcat
      [ H.a
        ! hxBoost
        ! hxOn "::config-request" "setXsrfHeader(event)"
        ! HA.href (H.textValue $ pack "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) user.uuid))
        $ toHtml user.email
      ]
    newUser :: Html
    newUser = H.a
      ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! htmlLink (Proxy @(AuthUser (CRUDCreate UserCreate))) $ "Create User"
    sortUsers = sortBy $ \a b -> compare a.email b.email
