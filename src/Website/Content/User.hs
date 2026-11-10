module Website.Content.User where

import           Control.Monad.Logger        (MonadLogger, logDebug)
import           Control.Monad.Reader        (MonadReader)
import           Data.Data                   (Proxy (Proxy))
import           Data.List                   (sortBy)
import           Data.Text                   (pack, show)
import           Prelude                     (compare, mappend, mconcat, pure,
                                              ($), (.), (<$>), (<>), (=<<))
import           Servant                     (ToHttpApiData (toUrlPiece),
                                              safeLink)
import           Text.Blaze.Html             (Html, toHtml, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Website.Content.Common      (AuthUser, basicPage,
                                              editDeleteButtons, htmlLink)
import           Website.Content.Htmx        (hxBoost, hxOn)
import           Website.Data.Env            (HasEnv)
import           Website.Data.User           (RequiredUser,
                                              User (email, group, uuid),
                                              UserCreate, UserKey)
import           Website.Network.API.CRUD    (CRUDCreate, CRUDDelete, CRUDRead,
                                              CRUDUpdateForm)
import           Website.Network.API.Types   (topAPI)
import           Website.Types               (CanAppM)

userDisplay :: (HasEnv c, MonadReader c m, MonadLogger m) => User -> m Html
userDisplay user = do
  $(logDebug) $ "userDisplay " <> show user
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
  $(logDebug) $ "userDisplayFullPage " <> show u
  basicPage =<< userDisplay u

userList :: (CanAppM c e m, RequiredUser c) => [User] -> m Html
userList users = do
  $(logDebug) $ "userList " <> show users
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
