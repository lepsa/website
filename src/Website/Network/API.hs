{-# LANGUAGE OverloadedRecordDot #-}

module Website.Network.API where

import Text.Blaze.Html qualified as H
import Website.Content.Common
import Website.Content.Entry
import Website.Data.Entry
import Website.Types
import Servant
    ( addHeader,
      safeLink,
      Proxy(Proxy),
      ToHttpApiData(toUrlPiece),
      Header,
      Headers
    )
import Website.Network.API.CRUD
import Data.Text (Text)
import Website.Network.API.Types

getIndex :: (CanAppM c e m) => m H.Html
getIndex = pure index

--
-- Entry pages
--

-- Get an empty Entry creation form
getEntryInitial :: (CanAppM Env Err m) => m H.Html
getEntryInitial = entryCreationForm

-- Create an Entry and get its value back as Html
postEntry :: (CanAppM Env Err m) => EntryCreate -> m (Headers '[Header "Location" Text] H.Html)
postEntry create = do
  entry <- createEntry create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDRead EntryKey))) entry.key
  pure $ addHeader link mempty

-- Get a given Entry
getEntry :: (CanAppM Env Err m) => EntryKey -> m H.Html
getEntry key = entryDisplayFullPage =<< Website.Data.Entry.getEntry key

-- Update a given entry and get the new value back
putEntry :: (CanAppM Env Err m) => EntryKey -> EntryUpdate -> m H.Html
putEntry key value = entryDisplay =<< updateEntry key value

-- Get an Entry update form with the values pre-populated.
getEntryForUpdate :: (CanAppM Env Err m) => EntryKey -> m H.Html
getEntryForUpdate key = entryUpdateForm =<< Website.Data.Entry.getEntry key

-- Delete a given Entry, and get a confirmation response
deleteEntry :: (CanAppM Env Err m) => EntryKey -> m H.Html
deleteEntry key = do
  Website.Data.Entry.deleteEntry key
  pure $ H.toHtml @String "Deleted"

-- Get all of the Entries as a list
getEntries :: (CanAppM Env Err m) => m H.Html
getEntries = entryList =<< Website.Data.Entry.getEntries