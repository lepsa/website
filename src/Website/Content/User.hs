module Website.Content.User where

import Text.Blaze.Html
import Control.Monad.Reader
import Website.Data.User
import Website.Content.Common
import Website.Data.Env
import Website.Network.API.CRUD
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Data.Text
import Data.List (sortBy)
import Data.Data
import Servant
import Website.Network.API.Types
import Website.Types
import Website.Content.Htmx

userDisplay :: (HasEnv c, MonadReader c m) => User -> m Html
userDisplay user = pure $ H.div ! HA.id "user"
  $ mconcat
    [ H.h3 $ toHtml user.email
    , H.p $ toHtml $ "Group " <> show user.group
    , H.div
      ! HA.id "edit-delete-buttons"
      $ mconcat [edit, delete]
    ]
  where
    edit :: Html
    edit =
      H.button
        ! hxTrigger "click"
        ! hxSwap "outerHTML"
        ! hxTarget "#user"
        ! hxBoost
        ! hxOn "::config-request" "setXsrfHeader(event)"
        ! hxGet (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDUpdateForm UserKey))) user.uuid)
        $ "Edit"
    delete :: Html
    delete =
      H.button
        ! hxTrigger "click"
        ! hxSwap "outerHTML"
        ! hxTarget "#edit-delete-buttons"
        ! hxConfirm "Confirm deletion"
        ! hxBoost
        ! hxOn "::config-request" "setXsrfHeader(event)"
        ! hxDelete (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDDelete UserKey))) user.uuid)
        $ "Delete"

userDisplayFullPage :: (CanAppM c e m, RequiredUser c) => User -> m Html
userDisplayFullPage = basicPage <=< userDisplay

userList :: (CanAppM c e m, RequiredUser c) => [User] -> m Html
userList users =
  basicPage $
    mconcat
      [ H.h2 "Users",
        newUser,
        H.ul $ mconcat $ userLink <$> sortUsers users
      ]
  where
    userLink :: User -> Html
    userLink user =
      H.li $
        mconcat
          [ H.a
              ! hxBoost
              ! hxOn "::config-request" "setXsrfHeader(event)"
              ! HA.href (H.textValue $ pack "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) user.uuid))
              $ toHtml user.email
          ]
    newUser :: Html
    newUser =
      H.a 
        ! hxBoost
        ! hxOn "::config-request" "setXsrfHeader(event)"
        ! htmlLink (Proxy @(AuthUser (CRUDCreate UserCreate))) $ "Create User"
    sortUsers = sortBy $ \a b ->
      compare a.email b.email