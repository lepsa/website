module Website.Content.Entry where

import Control.Monad.Reader
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
import Website.Content.Htmx
import CMark (commonmarkToHtml)
import Data.Maybe (catMaybes)

-- | Display an entry, with edit and delete buttons
entryDisplay :: (HasEnv c, MonadReader c m, OptionalUser c) => Entry -> m Html
entryDisplay entry = do
  tz <- asks timeZone
  editDelete <- whenLoggedIn $ \_ -> H.div
    ! HA.id "edit-delete-buttons"
    $ mconcat
      [ edit,
        delete
      ]
  pure
    $ H.div
      ! HA.id "entry"
    $ mconcat $ catMaybes
      [ pure $ H.h3 $ toHtml entry.title,
        pure $ H.p $ toHtml $ "Created " <> entryTimeFormat tz entry.created,
        -- This isn't great, but it is the easiest way to do what I want. 
        -- We store the markdown in the DB for easy editing, but we display
        -- HTML when looking at the entry.
        -- TODO: Test this for XSS and related issues.
        --       Especially around JS.
        -- TODO: Maybe we can pre-render the HTML in the DB so we aren't
        --       processing it every time.
        pure $ preEscapedToHtml $ commonmarkToHtml [] entry.value,
        editDelete
      ]
  where
    edit :: Html
    edit =
      H.button
        ! hxTrigger "click"
        ! hxSwap "outerHTML"
        ! hxTarget "#entry"
        ! hxBoost
        ! hxOn "::config-request" "setXsrfHeader(event)"
        ! hxGet (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDUpdateForm EntryKey))) entry.key)
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
        ! hxDelete (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDDelete EntryKey))) entry.key)
        $ "Delete"

-- | As 'entryDisplay' with 'basicPage' wrapping
entryDisplayFullPage :: (HasEnv c, OptionalUser c) => MonadReader c m => Entry -> m Html
entryDisplayFullPage = basicPage <=< entryDisplay

-- | List all entries as a page
entryList :: (OptionalUser c, HasEnv c, MonadReader c m) => [Entry] -> m Html
entryList entries = do
  tz <- asks timeZone
  mNewEntry <- whenLoggedIn $ const newEntry
  basicPage $
    mconcat $ catMaybes
      [ pure $ H.h2 "Entries",
        mNewEntry,
        pure $ H.ul $
          mconcat $
            entryLink tz <$> sortEntriesByDateDesc entries
      ]
  where
    entryLink :: TimeZone -> Entry -> Html
    entryLink tz entry =
      H.li $
        mconcat
          [ H.a
              ! hxBoost
              ! hxOn "::config-request" "setXsrfHeader(event)"
              ! HA.href (H.textValue $ pack "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthEntry (CRUDRead EntryKey))) entry.key))
              $ toHtml entry.title,
            toHtml $ " " <> entryTimeFormat tz entry.created
          ]
    newEntry :: Html
    newEntry =
      H.a 
        ! hxBoost
        ! hxOn "::config-request" "setXsrfHeader(event)"
        ! htmlLink (Proxy @(AuthEntry (CRUDCreate EntryCreate))) $ "Create Entry"