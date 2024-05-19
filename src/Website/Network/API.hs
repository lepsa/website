{-# LANGUAGE OverloadedRecordDot #-}

module Website.Network.API where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger       (logDebug)
import           Data.Int
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Servant
import           Servant.Auth.Server
import           Servant.Multipart
import qualified Text.Blaze.Html            as H
import           Website.Auth.Authorisation (Access (Read, Write))
import           Website.Content.Common
import           Website.Content.Entry
import qualified Website.Content.File       as Website.Data.File
import           Website.Content.File
import           Website.Content.Index
import           Website.Content.User       (userDisplay, userDisplayFullPage, userList)
import           Website.Data.Entry
import           Website.Data.Error
import           Website.Data.File          (FileId)
import           Website.Data.Permission    (checkPermission)
import           Website.Data.User
import           Website.Network.API.CRUD
import           Website.Network.API.Types
import           Website.Types

getIndex :: (OptionalUser c, CanAppM c e m) => m H.Html
getIndex = do
  $(logDebug) "getIndex"
  index

--
-- Entry pages
--

-- Create an Entry and get its value back as Html
postEntry :: (RequiredUser c, CanAppM c e m) => EntryCreate -> m (Headers '[Header "Location" Text] H.Html)
postEntry create = do
  $(logDebug) $ "postEntry " <> T.pack (show create)
  checkPermission "POST entry" Write
  entry <- createEntry create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDRead EntryKey))) entry.key
  pure $ addHeader link mempty

-- Get a given Entry
getEntry :: (CanAppM c e m, OptionalUser c) => EntryKey -> m H.Html
getEntry key = do
  $(logDebug) $ "getEntry " <> T.pack (show key)
  checkPermission "GET entry" Read
  e <- Website.Data.Entry.getEntry key
  entryDisplayFullPage e

-- Update a given entry and get the new value back
putEntry :: (RequiredUser c, CanAppM c e m) => EntryKey -> EntryUpdate -> m H.Html
putEntry key value = do
  $(logDebug) $ "putEntry " <> T.pack (show key) <> " " <> T.pack (show value)
  checkPermission "PUT entry" Write
  entryDisplay =<< updateEntry key value

-- Delete a given Entry, and get a confirmation response
deleteEntry :: (RequiredUser c, CanAppM c e m) => EntryKey -> m H.Html
deleteEntry key = do
  $(logDebug) $ "deleteEntry " <> T.pack (show key)
  checkPermission "DELETE entry" Write
  Website.Data.Entry.deleteEntry key
  pure $ H.toHtml @String "Deleted"

-- Get all of the Entries as a list
getEntries :: (OptionalUser c, CanAppM c e m) => m H.Html
getEntries = do
  $(logDebug) "getEntries"
  checkPermission "GET entries" Read
  entryList =<< Website.Data.Entry.getEntries

getEntryByName :: (OptionalUser c, CanAppM c e m) => String -> m H.Html
getEntryByName name = do
  $(logDebug) $ "getEntryByName " <> T.pack (show name)
  checkPermission "GET entry" Read
  e <- Website.Data.Entry.getEntryByName name
  entryDisplayFullPage e

--
-- User Pages
--
postUser :: (RequiredUser c, CanAppM c e m) => UserCreate -> m (Headers '[Header "Location" Text] H.Html)
postUser create = do
  $(logDebug) "postUser"
  checkPermission "POST user" Write
  newUser <- createUser create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) newUser.uuid
  pure $ addHeader link mempty

getUser :: (RequiredUser c, CanAppM c e m) => UserKey -> m H.Html
getUser key = do
  $(logDebug) $ "getUser " <> T.pack (show key)
  checkPermission "GET user" Read
  userDisplayFullPage =<< Website.Data.User.getUser key

putUser :: (RequiredUser c, CanAppM c e m) => UserKey -> UserUpdate -> m H.Html
putUser key update = do
  $(logDebug) $ "putUser " <> T.pack (show key)
  checkPermission "PUT user" Write
  userDisplay =<< updateUser key update

deleteUser :: (RequiredUser c, CanAppM c e m) => UserKey -> m H.Html
deleteUser key = do
  $(logDebug) $ "deleteUser " <> T.pack (show key)
  checkPermission "DELETE user" Write
  Website.Data.User.deleteUser key
  pure $ H.toHtml @String "Delete"

getUsers :: (RequiredUser c, CanAppM c e m) => m H.Html
getUsers = do
  $(logDebug) "getUsers"
  checkPermission "GET users" Read
  userList =<< Website.Data.User.getUsers

--
-- File pages
--
getFiles :: (OptionalUser c, CanAppM c e m) => m H.Html
getFiles = do
  $(logDebug) "getFiles"
  checkPermission "GET files" Read
  Website.Content.File.getFiles

postFile :: forall c e m. (RequiredUser c, CanAppM c e m) => MultipartData Tmp -> m (Headers '[Header "Location" Text] H.Html)
postFile m = do
  $(logDebug) "postFiles"
  checkPermission "POST file" Write
  Website.Data.File.uploadFile m

getFile :: (OptionalUser c, CanAppM c e m) => FileId -> m (Headers '[Header "Content-Length" Int64] WithCT)
getFile k = do
  $(logDebug) $ "getFile " <> T.pack (show k)
  checkPermission "GET file" Read
  Website.Data.File.getFile k

deleteFile :: (RequiredUser c, CanAppM c e m) => FileId -> m H.Html
deleteFile k = do
  $(logDebug) $ "deleteFile " <> T.pack (show k)
  checkPermission "DELETE file" Write
  Website.Data.File.deleteFile k

register :: CanAppM c e m => CookieSettings -> JWTSettings -> UserCreate -> m (SetLoginCookies NoContent)
register cookieSettings jwtSettings cUser = do
  $(logDebug) "register"
  adminExists >>= flip when (throwError_ Unauthorised)
  user <- createUser cUser
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user.uuid
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDRead UserKey))) user.uuid
  case mApplyCookies of
    Nothing      -> throwError_ $ Other "Could not build login cookies"
    Just cookies -> pure $ cookies $ addHeader link NoContent
