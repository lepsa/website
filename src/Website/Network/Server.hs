{-# LANGUAGE ScopedTypeVariables #-}

module Website.Network.Server where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Text                   as T
import           Data.UUID                   ()
import           Servant
import           Servant.Auth.Server
import           Servant.HTML.Blaze
import           System.FilePath
import           Text.Blaze.Html
import           Website.Auth.Authentication
import           Website.Content.Common
import           Website.Content.Forms
import           Website.Data.Entry          (EntryCreate, EntryKey,
                                              EntryUpdate)
import           Website.Data.Env
import           Website.Data.Error
import           Website.Data.File           (FileId)
import           Website.Data.User           (OptionalUser, UserCreate, UserKey,
                                              UserLogin, UserUpdate,
                                              getUserLogin)
import           Website.Network.API
import           Website.Network.API.CRUD
import           Website.Network.API.Types
import           Website.Types

server :: CookieSettings -> JWTSettings -> FilePath -> ServerT TopAPI (AppM Env Err IO)
server cookieSettings jwtSettings currentDirectory = api
  where
    api a =
      unprotected a getIndex
        :<|> unprotected a getLogin
        :<|> unprotected a . login
        :<|> unprotected a . register cookieSettings jwtSettings
        :<|> crudUser a
        :<|> protected a getUsers
        :<|> crudEntry a
        :<|> unprotected a getEntries
        :<|> crudFile a
        :<|> unprotected a getFiles
        :<|> serveDirectoryWebApp (currentDirectory </> "static")

    getLogin :: (OptionalUser c, CanAppM c e m) => m Html
    getLogin = loginForm

    login :: (CanAppM c e m) => Login -> m (SetLoginCookies NoContent)
    login (Login user pass) = do
      c <- asks conn
      eUserId <- runExceptT $ checkUserPassword c (T.pack user) (T.pack pass)
      userId <- either (const $ throwError_ Unauthenticated) pure eUserId
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings userId
      case mApplyCookies of
        Nothing      -> throwError_ Unauthenticated
        Just cookies -> pure $ cookies $ addHeader root NoContent
      where
        root = linkText (Proxy @(AuthLogin :> Get '[HTML] Html))

    crudEntry :: Authed -> ServerT (CRUDForm EntryCreate EntryUpdate EntryKey) (AppM Env Err IO)
    crudEntry a =
      (protected a . postEntry)
        :<|> protected a entryCreationForm
        -- Unprotected, as we want the internet at large to be able to read this.
        :<|> (unprotected a . getEntry)
        :<|> (\ek -> protected a . putEntry ek)
        :<|> (protected a . getEntryForUpdate)
        :<|> (protected a . deleteEntry)

    crudUser :: Authed -> ServerT (CRUDForm UserCreate UserUpdate UserKey) (AppM Env Err IO)
    crudUser a =
      hoistServer (Proxy @(CRUDForm UserCreate UserUpdate UserKey)) (protected a) $
        postUser
          :<|> userCreationForm
          :<|> getUser
          :<|> putUser
          :<|> getUserForUpdate
          :<|> deleteUser

    crudFile :: Authed -> ServerT (CRUDForm' FileUpload FileUpload FileId) (AppM Env Err IO)
    crudFile a =
      protected a . postFile
        :<|> protected a fileCreationForm
        :<|> unprotected a . getFile
        :<|> protected a . deleteFile

    -- When using a user token do a couple of things.
    -- 1) Ensure that the user actually exists in the database.
    --    We don't want users who have had their account deleted
    --    still being able to access protected APIs.
    -- 2) Gather up all of the information what we might want to
    --    use when customising pages for the user
    protected ::
      (MonadIO m) =>
      Authed ->
      AppM (EnvAuthed UserLogin) Err m b ->
      AppM Env Err m b
    protected a m = case a of
      Authenticated user -> go user
      _                  -> throwError_ Unauthenticated
      where
        go key = do
          l <- withError userLookupErrors $ getUserLogin key
          withReaderT (mkEnvAuthed l) m

    -- Similar to `withProtected` but doesn't require that the user
    -- is authenticated. If an authenticated user ID doesn't exist
    -- in the database however, that is an issue.
    unprotected ::
      (MonadIO m) =>
      Authed ->
      AppM (EnvAuthed (Maybe UserLogin)) Err m b ->
      AppM Env Err m b
    unprotected (Authenticated userKey) m = do
      userLogin <- withError userLookupErrors $ getUserLogin userKey
      withReaderT (mkEnvAuthed $ Just userLogin) m
    unprotected _ m = withReaderT (mkEnvAuthed Nothing) m

    -- User lookups can fail in a couple of ways,
    -- but these are the ones we really care to catch.
    -- A missing user at this stage means that the auth
    -- token was valid as far as the crypto was concerned
    -- but the ID it encoded no longer exists, so the
    -- user shouldn't be authenticated any more.
    userLookupErrors (DbError NotFound)       = Unauthenticated
    userLookupErrors (DbError TooManyResults) = Unauthenticated
    userLookupErrors e                        = e
