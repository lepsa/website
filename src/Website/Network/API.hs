module Website.Network.API where

import Text.Blaze.Html qualified as H
import Website.Content.Common
import Website.Content.Entry
import Website.Data.Entry
import Website.Types

getIndex :: (CanAppM c e m) => m H.Html
getIndex = pure index

--
-- Entry pages
--
getEntryInitial :: (CanAppM Env Err m) => m H.Html
getEntryInitial = pure entryCreationForm

postEntry :: (CanAppM Env Err m) => EntryCreate -> m H.Html
postEntry create = entryDisplay <$> createEntry create

getEntry :: (CanAppM Env Err m) => EntryKey -> m H.Html
getEntry key = entryDisplay <$> Website.Data.Entry.getEntry key

putEntry :: (CanAppM Env Err m) => EntryKey -> EntryUpdate -> m H.Html
putEntry key value = entryDisplaySimple <$> updateEntry key value

getEntryForUpdate :: (CanAppM Env Err m) => EntryKey -> m H.Html
getEntryForUpdate key = entryUpdateForm <$> Website.Data.Entry.getEntry key

deleteEntry :: (CanAppM Env Err m) => EntryKey -> m H.Html
deleteEntry key = do
  Website.Data.Entry.deleteEntry key
  pure $ H.toHtml @String "Deleted"

getEntries :: (CanAppM Env Err m) => m H.Html
getEntries = entryList <$> Website.Data.Entry.getEntries