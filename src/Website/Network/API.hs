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
import Website.Data.Permission (checkPermission)
import Website.Auth.Authorisation (Access(Read, Write))

getIndex :: (CanAppM c e m) => m H.Html
getIndex = pure index

--
-- Entry pages
--

-- Create an Entry and get its value back as Html
postEntry :: (CanAppM Env Err m) => UserKey -> EntryCreate -> m (Headers '[Header "Location" Text] H.Html)
postEntry user create = do
  checkPermission user "POST entry" Write
  entry <- createEntry create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDRead EntryKey))) entry.key
  pure $ addHeader link mempty

-- Get a given Entry
getEntry :: (CanAppM Env Err m) => UserKey -> EntryKey -> m H.Html
getEntry user key = do
  checkPermission user "GET entry" Read
  entryDisplayFullPage =<< Website.Data.Entry.getEntry key

-- Update a given entry and get the new value back
putEntry :: (CanAppM Env Err m) => UserKey -> EntryKey -> EntryUpdate -> m H.Html
putEntry user key value = do
  checkPermission user "PUT entry" Write
  entryDisplay =<< updateEntry key value

-- Delete a given Entry, and get a confirmation response
deleteEntry :: (CanAppM Env Err m) => UserKey -> EntryKey -> m H.Html
deleteEntry user key = do
  checkPermission user "DELETE entry" Write
  Website.Data.Entry.deleteEntry key
  pure $ H.toHtml @String "Deleted"

-- Get all of the Entries as a list
getEntries :: (CanAppM Env Err m) => UserKey -> m H.Html
getEntries user = do
  checkPermission user "GET entries" Read
  entryList =<< Website.Data.Entry.getEntries

--
-- User Pages
--
postUser :: (CanAppM Env Err m) => UserKey -> UserCreate -> m (Headers '[Header "Location" Text] H.Html)
postUser userId create = do
  checkPermission userId "POST user" Write
  user <- createUser create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) user.uuid
  pure $ addHeader link mempty

getUser :: CanAppM Env Err m => UserKey -> UserKey -> m H.Html
getUser userId key = do
  checkPermission userId "GET user" Read
  userDisplayFullPage =<< Website.Data.User.getUser key

putUser :: CanAppM Env Err m => UserKey -> UserKey -> UserUpdate -> m H.Html
putUser userId key update = do
  checkPermission userId "PUT user" Write
  userDisplay =<< updateUser key update

deleteUser :: CanAppM Env Err m => UserKey -> UserKey -> m H.Html
deleteUser userId key = do
  checkPermission userId "DELETE user" Write
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