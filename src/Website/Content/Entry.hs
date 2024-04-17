module Website.Content.Entry where

import Data.List (sortBy)
import Data.Text
import Servant
import Text.Blaze.Html
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Text.Blaze.Internal as H
import Website.Content.Common
import Website.Content.HTMX
import Website.Data.Entry
import Website.Network.API.CRUD
import Website.Network.API.Types

entryUpdateForm :: Entry -> Html
entryUpdateForm = generateUpdateForm

entryCreationForm :: Html
entryCreationForm =
  basicPage $
    generateNewForm $
      Proxy @Entry

entryDisplaySimple :: Entry -> Html
entryDisplaySimple entry =
  H.div
    ! HA.id "entry"
    $ mconcat
      [ H.h3 $ toHtml entry.title,
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
        ! htmxAttribute "hx-trigger" "click"
        ! htmxAttribute "hx-swap" "outerHTML"
        ! htmxAttribute "hx-target" "#entry"
        ! htmxAttribute "hx-get" (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @("entry" :> CRUDUpdateForm EntryKey)) entry.key)
        $ "Edit"
    delete :: Html
    delete =
      H.button
        ! htmxAttribute "hx-trigger" "click"
        ! htmxAttribute "hx-swap" "outerHTML"
        ! htmxAttribute "hx-target" "#edit-delete-buttons"
        ! htmxAttribute "hx-delete" (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @("entry" :> CRUDDelete EntryKey)) entry.key)
        $ "Delete"

entryDisplay :: Entry -> Html
entryDisplay = basicPage . entryDisplaySimple

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
      H.li $ H.a ! (HA.href $ H.textValue $ pack "/" <> toUrlPiece (safeLink topAPI (Proxy @("entry" :> CRUDRead EntryKey)) entry.key)) $ toHtml entry.title
    newEntry :: Html
    newEntry =
      H.a ! htmlLink (Proxy @("entry" :> CRUDCreate EntryCreate)) $ "Create Entry"
    sortEntries = sortBy $ \a b ->
      compare a.created b.created