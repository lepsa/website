module Website.Content.Entry where

import Control.Monad.Reader
import Data.List (sortBy)
import Data.Text
import Data.Time
import Servant
import Text.Blaze.Html
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Text.Blaze.Internal as H
import Website.Content.Common
import Website.Data.Entry
import Website.Network.API.CRUD
import Website.Network.API.Types
import Website.Data.Env
import Website.Data.User (OptionalUser)

-- | Display an entry, with edit and delete buttons
entryDisplay :: (HasEnv c, MonadReader c m) => Entry -> m Html
entryDisplay entry = do
  tz <- asks timeZone
  pure
    $ H.div
      ! HA.id "entry"
    $ mconcat
      [ H.h3 $ toHtml entry.title,
        H.p $ toHtml $ "Created " <> entryTimeFormat tz entry.created,
        mconcat $ H.p . toHtml <$> Prelude.lines entry.value,
        H.div
          ! HA.id "edit-delete-buttons"
          $ mconcat
            [ edit,
              delete
            ]
      ]
  where
    edit :: Html
    edit =
      H.button
        ! dataAttribute "hx-trigger" "click"
        ! dataAttribute "hx-swap" "outerHTML"
        ! dataAttribute "hx-target" "#entry"
        ! dataAttribute "hx-boost" "true"
        ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
        ! dataAttribute "hx-get" (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDUpdateForm EntryKey))) entry.key)
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
        ! dataAttribute "hx-delete" (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDDelete EntryKey))) entry.key)
        $ "Delete"

-- | As 'entryDisplay' with 'basicPage' wrapping
entryDisplayFullPage :: (HasEnv c, OptionalUser c) => MonadReader c m => Entry -> m Html
entryDisplayFullPage = basicPage <=< entryDisplay

-- | List all entries as a page
entryList :: (OptionalUser c, HasEnv c, MonadReader c m) => [Entry] -> m Html
entryList entries = do
  tz <- asks timeZone
  basicPage $
    mconcat
      [ H.h2 "Entries",
        newEntry,
        H.ul $
          mconcat $
            entryLink tz <$> sortEntries entries
      ]
  where
    entryLink :: TimeZone -> Entry -> Html
    entryLink tz entry =
      H.li $
        mconcat
          [ H.a
              ! dataAttribute "hx-boost" "true"
              ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
              ! HA.href (H.textValue $ pack "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthEntry (CRUDRead EntryKey))) entry.key))
              $ toHtml entry.title,
            toHtml $ " " <> entryTimeFormat tz entry.created
          ]
    newEntry :: Html
    newEntry =
      H.a 
        ! dataAttribute "hx-boost" "true"
        ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
        ! htmlLink (Proxy @(AuthEntry (CRUDCreate EntryCreate))) $ "Create Entry"
    sortEntries = sortBy $ \a b ->
      compare a.created b.created