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
import Website.Data.Error
import Website.Content.User (userDisplayFullPage, userDisplay)
import Website.Data.User
import Control.Monad.IO.Class
import Servant.Auth.Server
import Website.Data.Permission (checkPermission)
import Website.Auth.Authorisation (Access(Read, Write))

getIndex :: (CanAppM c e m) => Authed -> m H.Html
getIndex = pure . index

--
-- Entry pages
--

-- Create an Entry and get its value back as Html
postEntry :: (CanAppM c e m) => UserLogin -> EntryCreate -> m (Headers '[Header "Location" Text] H.Html)
postEntry user create = do
  checkPermission user "POST entry" Write
  entry <- createEntry create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDRead EntryKey))) entry.key
  pure $ addHeader link mempty

-- Get a given Entry
getEntry :: (CanAppM c e m) => Authed -> UserLogin -> EntryKey -> m H.Html
getEntry auth user key = do
  checkPermission user "GET entry" Read
  entryDisplayFullPage auth =<< Website.Data.Entry.getEntry key

-- Update a given entry and get the new value back
putEntry :: (CanAppM c e m) => UserLogin -> EntryKey -> EntryUpdate -> m H.Html
putEntry user key value = do
  checkPermission user "PUT entry" Write
  entryDisplay =<< updateEntry key value

-- Delete a given Entry, and get a confirmation response
deleteEntry :: (CanAppM c e m) => UserLogin -> EntryKey -> m H.Html
deleteEntry user key = do
  checkPermission user "DELETE entry" Write
  Website.Data.Entry.deleteEntry key
  pure $ H.toHtml @String "Deleted"

-- Get all of the Entries as a list
getEntries :: (CanAppM c e m) => Authed -> UserLogin -> m H.Html
getEntries auth user = do
  checkPermission user "GET entries" Read
  entryList auth =<< Website.Data.Entry.getEntries

--
-- User Pages
--
postUser :: (CanAppM c e m) => UserLogin -> UserCreate -> m (Headers '[Header "Location" Text] H.Html)
postUser userId create = do
  checkPermission userId "POST user" Write
  user <- createUser create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) user.uuid
  pure $ addHeader link mempty

getUser :: CanAppM c e m => Authed -> UserLogin -> UserKey -> m H.Html
getUser auth userId key = do
  checkPermission userId "GET user" Read
  userDisplayFullPage auth =<< Website.Data.User.getUser key

putUser :: CanAppM c e m => UserLogin -> UserKey -> UserUpdate -> m H.Html
putUser userId key update = do
  checkPermission userId "PUT user" Write
  userDisplay =<< updateUser key update

deleteUser :: CanAppM c e m => UserLogin -> UserKey -> m H.Html
deleteUser userId key = do
  checkPermission userId "DELETE user" Write
  Website.Data.User.deleteUser key
  pure $ H.toHtml @String "Delete"

register :: CanAppM c e m => Authed -> CookieSettings -> JWTSettings -> UserCreate -> m (SetLoginCookies NoContent)
register _auth cookieSettings jwtSettings cUser = do
  user <- createUser cUser
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user.uuid
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) user.uuid
  case mApplyCookies of
    Nothing -> throwError_ $ Other "Could not build login cookies"
    Just cookies -> pure $ cookies $ addHeader link NoContent