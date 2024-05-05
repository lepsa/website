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
import Website.Data.Error

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
        ! dataAttribute "hx-trigger" "click"
        ! dataAttribute "hx-swap" "outerHTML"
        ! dataAttribute "hx-target" "#user"
        ! dataAttribute "hx-boost" "true"
        ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
        ! dataAttribute "hx-get" (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDUpdateForm UserKey))) user.uuid)
        $ "Edit"
    delete :: Html
    delete =
      H.button
        ! dataAttribute "hx-trigger" "click"
        ! dataAttribute "hx-swap" "outerHTML"
        ! dataAttribute "hx-target" "#edit-delete-buttons"
        ! dataAttribute "hx-confirm" "Confirm deletion"
        ! dataAttribute "hx-boost" "true"
        ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
        ! dataAttribute "hx-delete" (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDDelete UserKey))) user.uuid)
        $ "Delete"

userDisplayFullPage :: User -> AppM (EnvAuthed UserLogin) Err IO Html
userDisplayFullPage = withReaderT (fmap pure) . basicPage <=< userDisplay

userList :: [User] -> AppM (EnvAuthed UserLogin) Err IO Html
userList users = withReaderT (fmap pure) $
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
              ! dataAttribute "hx-boost" "true"
              ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
              ! HA.href (H.textValue $ pack "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) user.uuid))
              $ toHtml user.email
          ]
    newUser :: Html
    newUser =
      H.a 
        ! dataAttribute "hx-boost" "true"
        ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
        ! htmlLink (Proxy @(AuthUser (CRUDCreate UserCreate))) $ "Create User"
    sortUsers = sortBy $ \a b ->
      compare a.email b.email