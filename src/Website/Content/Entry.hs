module Website.Content.Entry where

import Data.List (sortBy)
import Data.Text
import Data.Time
import Data.Time.Format.ISO8601
import Servant
import Text.Blaze.Html
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Text.Blaze.Internal as H
import Website.Content.Common
import Website.Data.Entry
import Website.Network.API.CRUD
import Website.Network.API.Types

entryTimeFormat :: UTCTime -> String
entryTimeFormat = formatShow iso8601Format

entryUpdateForm :: Entry -> Html
entryUpdateForm = generateUpdateForm

-- Page to create a new Entry
entryCreationForm :: Html
entryCreationForm =
  basicPage $
    generateNewForm $
      Proxy @Entry

-- | Display an entry, with edit and delete buttons
entryDisplay :: Entry -> Html
entryDisplay entry =
  H.div
    ! HA.id "entry"
    $ mconcat
      [ H.h3 $ toHtml entry.title,
        H.p $ toHtml $ "Created " <> entryTimeFormat entry.created,
        H.p $ toHtml entry.value,
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
        ! dataAttribute "hx-get" (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @("entry" :> CRUDUpdateForm EntryKey)) entry.key)
        $ "Edit"
    delete :: Html
    delete =
      H.button
        ! dataAttribute "hx-trigger" "click"
        ! dataAttribute "hx-swap" "outerHTML"
        ! dataAttribute "hx-target" "#edit-delete-buttons"
        ! dataAttribute "hx-confirm" "Confirm deletion"
        ! dataAttribute "hx-delete" (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @("entry" :> CRUDDelete EntryKey)) entry.key)
        $ "Delete"

-- | As 'entryDisplay' with 'basicPage' wrapping
entryDisplayFullPage :: Entry -> Html
entryDisplayFullPage = basicPage . entryDisplay

-- | List all entries as a page
entryList :: [Entry] -> Html
entryList entries =
  basicPage $
    mconcat
      [ H.h2 "Entries",
        newEntry,
        H.ul $
          mconcat $
            entryLink <$> sortEntries entries
      ]
  where
    entryLink :: Entry -> Html
    entryLink entry =
      H.li
        $ mconcat
        [ H.a
          ! (HA.href $ H.textValue $ pack "/" <> toUrlPiece (safeLink topAPI (Proxy @("entry" :> CRUDRead EntryKey)) entry.key))
          $ toHtml entry.title,
          toHtml $ " (" <> entryTimeFormat entry.created <> ")"
        ]
    newEntry :: Html
    newEntry =
      H.a ! htmlLink (Proxy @("entry" :> CRUDCreate EntryCreate)) $ "Create Entry"
    sortEntries = sortBy $ \a b ->
      compare a.created b.created