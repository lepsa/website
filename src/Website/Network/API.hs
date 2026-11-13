{-# LANGUAGE OverloadedRecordDot #-}

module Website.Network.API where

import           Control.Monad               (when, (>>=))
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Control.Monad.Logger        (logDebug)
import           Control.Monad.Reader        (asks)
import           Data.Int                    (Int64)
import           Data.Text                   (Text, show)
import qualified Data.Text                   as T
import           Prelude                     (Maybe (Just, Nothing), String,
                                              const, either, flip, mappend,
                                              mempty, pure, ($), (.), (<>),
                                              (=<<))
import           Servant                     (Get, Header, Headers,
                                              NoContent (NoContent),
                                              Proxy (Proxy),
                                              ToHttpApiData (toUrlPiece),
                                              addHeader, safeLink, (:>))
import           Servant.Auth.Server         (CookieSettings, JWTSettings,
                                              acceptLogin, clearSession)
import           Servant.HTML.Blaze          (HTML)
import           Servant.Multipart           (MultipartData, Tmp)
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html             (Html)
import           Website.Auth.Authentication (Login (Login), checkUserPassword)
import           Website.Auth.Authorisation  (Access (Read, Write))
import           Website.Content.Common      (AuthEntry, AuthUser, basicPage,
                                              linkText, loginForm)
import           Website.Content.Entry       (entryDisplay,
                                              entryDisplayFullPage, entryList)
import           Website.Content.File        (deleteFile, getFile, getFiles,
                                              uploadFile)
import           Website.Content.Forms       (fileCreationForm, generateNewForm,
                                              generateUpdateForm)
import           Website.Content.Index       (index)
import           Website.Content.User        (userDisplay, userDisplayFullPage,
                                              userList)
import           Website.Data.Entry          (Entry (key), EntryCreate,
                                              EntryKey, EntryUpdate,
                                              createEntry, deleteEntry,
                                              getEntries, getEntry,
                                              getEntryByName, updateEntry)
import           Website.Data.Env            (conn)
import           Website.Data.Error          (Err (Other, Unauthenticated, Unauthorised),
                                              throwError_)
import           Website.Data.File           (FileId)
import           Website.Data.Permission     (checkPermission)
import           Website.Data.User           (OptionalUser, RequiredUser,
                                              User (uuid), UserCreate, UserKey,
                                              UserUpdate, adminExists,
                                              createUser, deleteUser, getUser,
                                              getUsers, updateUser)
import           Website.Network.API.CRUD    (CRUDFormRead, WithCT)
import           Website.Network.API.Types   (AppAuth, Index, SetLoginCookies, SetLogoutCookies,
                                              topAPI)
import           Website.Types               (CanAppM)

indexGet :: (OptionalUser c, CanAppM c e m) => m H.Html
indexGet = do
  $(logDebug) "getIndex"
  index

loginGet :: (CanAppM c e m, OptionalUser c) => m H.Html
loginGet = loginForm

loginPost :: (CanAppM c e m) => CookieSettings -> JWTSettings -> Login -> m (SetLoginCookies NoContent)
loginPost cookieSettings jwtSettings (Login user pass) = do
  $(logDebug) $ "login " <> show user
  c <- asks conn
  eUserId <- runExceptT $ checkUserPassword c (T.pack user) (T.pack pass)
  userId <- either (const $ throwError_ Unauthenticated) pure eUserId
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings userId
  case mApplyCookies of
    Nothing      -> throwError_ Unauthenticated
    Just cookies -> pure $ cookies $ addHeader root NoContent
  where
    root = linkText (Proxy @(AppAuth :> Get '[HTML] Html))

logoutPost :: CanAppM c e m => CookieSettings -> m (SetLogoutCookies NoContent)
logoutPost cookieSettings = do
  $(logDebug) $ "logout"
  pure
    . clearSession cookieSettings
    . addHeader root
    $ addHeader root NoContent
  where
    root = linkText (Proxy @(AppAuth :> Index))

--
-- Entry pages
--

-- Create an Entry and get its value back as Html
entryCreatePost :: (RequiredUser c, CanAppM c e m) => EntryCreate -> m (Headers '[Header "Location" Text] H.Html)
entryCreatePost create = do
  $(logDebug) $ "entryCreatePost " <> show create
  checkPermission "POST entry" Write
  entry <- createEntry create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDFormRead EntryKey))) entry.key
  pure $ addHeader link mempty

entryCreateGet :: (RequiredUser c, CanAppM c e m) => m H.Html
entryCreateGet = do
  $(logDebug) $ "entryCreateGet"
  checkPermission "GET new entry" Write
  basicPage =<< generateNewForm (Proxy @Entry)

-- Get a given Entry
entryGet :: (CanAppM c e m, OptionalUser c) => EntryKey -> m H.Html
entryGet key = do
  $(logDebug) $ "entryGet " <> show key
  checkPermission "GET entry" Read
  entryDisplayFullPage =<< getEntry key

-- Update a given entry and get the new value back
entryUpdatePut :: (RequiredUser c, CanAppM c e m) => EntryKey -> EntryUpdate -> m H.Html
entryUpdatePut key value = do
  $(logDebug) $ "entryUpdatePut " <> show key <> " " <> show value
  checkPermission "PUT entry" Write
  entryDisplay =<< updateEntry key value

entryUpdateGet :: (RequiredUser c, CanAppM c e m) => EntryKey -> m H.Html
entryUpdateGet key = do
  $(logDebug) $ "entryUpdateGet " <> show key <> " " <> show key
  checkPermission "GET update entry" Read
  generateUpdateForm =<< getEntry key

-- Delete a given Entry, and get a confirmation response
entryDelete :: (RequiredUser c, CanAppM c e m) => EntryKey -> m H.Html
entryDelete key = do
  $(logDebug) $ "entryDelete " <> show key
  checkPermission "DELETE entry" Write
  deleteEntry key
  pure $ H.toHtml @String "Deleted"

-- Get all of the Entries as a list
entriesGet :: (OptionalUser c, CanAppM c e m) => m H.Html
entriesGet = do
  $(logDebug) "entriesGet"
  checkPermission "GET entries" Read
  entryList =<< getEntries

entryByNameGet :: (OptionalUser c, CanAppM c e m) => Text -> m H.Html
entryByNameGet name = do
  $(logDebug) $ "getEntryByName " <> show name
  checkPermission "GET entry" Read
  entryDisplayFullPage =<< getEntryByName name

--
-- User Pages
--
userCreatePost :: (RequiredUser c, CanAppM c e m) => UserCreate -> m (Headers '[Header "Location" Text] H.Html)
userCreatePost create = do
  $(logDebug) "userCreatePost"
  checkPermission "POST user" Write
  newUser <- createUser create
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDFormRead UserKey))) newUser.uuid
  pure $ addHeader link mempty

userCreateGet :: (RequiredUser c, CanAppM c e m) => m H.Html
userCreateGet = do
  $(logDebug) "userCreateGet"
  checkPermission "GET new user" Read
  basicPage =<< generateNewForm (Proxy @User)

userGet :: (RequiredUser c, CanAppM c e m) => UserKey -> m H.Html
userGet key = do
  $(logDebug) $ "userGet " <> show key
  checkPermission "GET user" Read
  userDisplayFullPage =<< getUser key

userUpdatePut :: (RequiredUser c, CanAppM c e m) => UserKey -> UserUpdate -> m H.Html
userUpdatePut key update = do
  $(logDebug) $ "userUpdatePut " <> show key
  checkPermission "PUT user" Write
  userDisplay =<< updateUser key update

userUpdateGet :: (RequiredUser c, CanAppM c e m) => UserKey -> m H.Html
userUpdateGet key = do
  checkPermission "GET update user" Read
  generateUpdateForm =<< getUser key

userDelete :: (RequiredUser c, CanAppM c e m) => UserKey -> m H.Html
userDelete key = do
  $(logDebug) $ "userDelete " <> show key
  checkPermission "DELETE user" Write
  deleteUser key
  pure $ H.toHtml @String "Delete"

usersGet :: (RequiredUser c, CanAppM c e m) => m H.Html
usersGet = do
  $(logDebug) "usersGet"
  checkPermission "GET users" Read
  userList =<< getUsers

--
-- File pages
--

filesGet :: (OptionalUser c, CanAppM c e m) => m H.Html
filesGet = do
  $(logDebug) "filesGet"
  checkPermission "GET files" Read
  getFiles

createFilePost :: forall c e m. (RequiredUser c, CanAppM c e m) => MultipartData Tmp -> m (Headers '[Header "Location" Text] H.Html)
createFilePost m = do
  $(logDebug) "createFilePost"
  checkPermission "POST file" Write
  uploadFile m

createFileGet :: (CanAppM c e m, RequiredUser c) => m H.Html
createFileGet = fileCreationForm

fileGet :: (OptionalUser c, CanAppM c e m) => FileId -> m (Headers '[Header "Content-Length" Int64] WithCT)
fileGet k = do
  $(logDebug) $ "getFile " <> show k
  checkPermission "GET file" Read
  getFile k

fileDelete :: (RequiredUser c, CanAppM c e m) => FileId -> m H.Html
fileDelete k = do
  $(logDebug) $ "deleteFile " <> show k
  checkPermission "DELETE file" Write
  deleteFile k

-- User signup

register :: CanAppM c e m => CookieSettings -> JWTSettings -> UserCreate -> m (SetLoginCookies NoContent)
register cookieSettings jwtSettings cUser = do
  $(logDebug) "register"
  adminExists >>= flip when (throwError_ Unauthorised)
  user <- createUser cUser
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user.uuid
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthUser (CRUDFormRead UserKey))) user.uuid
  case mApplyCookies of
    Nothing      -> throwError_ $ Other "Could not build login cookies"
    Just cookies -> pure $ cookies $ addHeader link NoContent
