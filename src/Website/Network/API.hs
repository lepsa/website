{-# LANGUAGE OverloadedRecordDot #-}

module Website.Network.API where

import Text.Blaze.Html qualified as H
import Website.Content.Common
import Website.Content.Entry
import Website.Data.Entry
import Website.Types
import Servant
import Website.Network.API.CRUD
import Data.Text (Text)
import Website.Network.API.Types
import Website.Data.Env
import Website.Data.Error
import Website.Content.User (userDisplayFullPage, userDisplay)
import Control.Monad
import Website.Data.User
import Control.Monad.IO.Class
import Servant.Auth.Server

getIndex :: (CanAppM c e m) => m H.Html
getIndex = pure index

--
-- Entry pages
--

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

-- Delete a given Entry, and get a confirmation response
deleteEntry :: (CanAppM Env Err m) => EntryKey -> m H.Html
deleteEntry key = do
  Website.Data.Entry.deleteEntry key
  pure $ H.toHtml @String "Deleted"

-- Get all of the Entries as a list
getEntries :: (CanAppM Env Err m) => m H.Html
getEntries = entryList =<< Website.Data.Entry.getEntries

--
-- User Pages
--
postUser :: (CanAppM Env Err m) => UserCreate -> m (Headers '[Header "Location" Text] H.Html)
postUser create = do
  user <- createUser create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) user.uuid
  pure $ addHeader link mempty

getUser :: CanAppM Env Err m => UserKey -> m H.Html
getUser key = userDisplayFullPage =<< Website.Data.User.getUser key

putUser :: CanAppM Env Err m => UserKey -> UserUpdate -> m H.Html
putUser key = userDisplay <=< updateUser key

deleteUser :: CanAppM Env Err m => UserKey -> m H.Html
deleteUser key = do
  Website.Data.User.deleteUser key
  pure $ H.toHtml @String "Delete"

register :: CanAppM Env Err m => CookieSettings -> JWTSettings -> UserCreate -> m (SetLoginCookies NoContent)
register cookieSettings jwtSettings cUser = do
  user <- createUser cUser
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user.uuid
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) user.uuid
  case mApplyCookies of
    Nothing -> throwError $ Other "Could not build login cookies"
    Just cookies -> pure $ cookies $ addHeader link NoContent