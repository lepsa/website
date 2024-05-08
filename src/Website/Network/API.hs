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
import Website.Content.User (userDisplayFullPage, userDisplay, userList)
import Website.Data.User
import Control.Monad
import Control.Monad.IO.Class
import Servant.Auth.Server
import Website.Data.Permission (checkPermission)
import Website.Auth.Authorisation (Access(Read, Write))
import Website.Data.Env (auth)
import Control.Monad.Reader
import Website.Content.Index

getIndex :: (OptionalUser c, CanAppM c e m) => m H.Html
getIndex = index

--
-- Entry pages
--

-- Create an Entry and get its value back as Html
postEntry :: (RequiredUser c, CanAppM c e m) => EntryCreate -> m (Headers '[Header "Location" Text] H.Html)
postEntry create = do
  user <- asks auth
  checkPermission user "POST entry" Write
  entry <- createEntry create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDRead EntryKey))) entry.key
  pure $ addHeader link mempty

-- Get a given Entry
getEntry :: (CanAppM c e m, OptionalUser c) => EntryKey -> m H.Html
getEntry key = do
  -- TODO: extend permission checks for unauthed users
  -- user <- asks auth
  -- checkPermission user "GET entry" Read
  e <- Website.Data.Entry.getEntry key
  entryDisplayFullPage e

-- Update a given entry and get the new value back
putEntry :: (RequiredUser c, CanAppM c e m) => EntryKey -> EntryUpdate -> m H.Html
putEntry key value = do
  user <- asks auth
  checkPermission user "PUT entry" Write
  entryDisplay =<< updateEntry key value

-- Delete a given Entry, and get a confirmation response
deleteEntry :: (RequiredUser c, CanAppM c e m) => EntryKey -> m H.Html
deleteEntry key = do
  user <- asks auth
  checkPermission user "DELETE entry" Write
  Website.Data.Entry.deleteEntry key
  pure $ H.toHtml @String "Deleted"

-- Get all of the Entries as a list
getEntries :: (OptionalUser c, CanAppM c e m) => m H.Html
getEntries = do
  -- TODO: extend permission checks for unauthed users
  -- user <- asks auth
  -- checkPermission user "GET entries" Read
  entryList =<< Website.Data.Entry.getEntries

--
-- User Pages
--
postUser :: (RequiredUser c, CanAppM c e m) => UserCreate -> m (Headers '[Header "Location" Text] H.Html)
postUser create = do
  user <- asks auth
  checkPermission user "POST user" Write
  newUser <- createUser create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) newUser.uuid
  pure $ addHeader link mempty

getUser :: (RequiredUser c, CanAppM c e m) => UserKey -> m H.Html
getUser key = do
  user <- asks auth
  checkPermission user "GET user" Read
  userDisplayFullPage =<< Website.Data.User.getUser key

putUser :: (RequiredUser c, CanAppM c e m) => UserKey -> UserUpdate -> m H.Html
putUser key update = do
  user <- asks auth
  checkPermission user "PUT user" Write
  userDisplay =<< updateUser key update

deleteUser :: (RequiredUser c, CanAppM c e m) => UserKey -> m H.Html
deleteUser key = do
  user <- asks auth
  checkPermission user "DELETE user" Write
  Website.Data.User.deleteUser key
  pure $ H.toHtml @String "Delete"

getUsers :: (RequiredUser c, CanAppM c e m) => m H.Html
getUsers = do
  user <- asks auth
  checkPermission user "GET users" Read
  userList =<< Website.Data.User.getUsers

register :: CanAppM c e m => CookieSettings -> JWTSettings -> UserCreate -> m (SetLoginCookies NoContent)
register cookieSettings jwtSettings cUser = do
  adminExists >>= flip when (throwError_ Unauthorised)
  user <- createUser cUser
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user.uuid
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) user.uuid
  case mApplyCookies of
    Nothing -> throwError_ $ Other "Could not build login cookies"
    Just cookies -> pure $ cookies $ addHeader link NoContent