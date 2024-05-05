{-# LANGUAGE ScopedTypeVariables #-}

module Website.Network.Server where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text qualified as T
import Data.UUID ()
import Servant
import Servant.Auth.Server
import Website.Auth.Authentication
import Website.Network.API
import Website.Network.API.Types
import Website.Types
import Website.Data.Env
import Website.Content.Forms
import Text.Blaze.Html
import Website.Content.Common
import Servant.HTML.Blaze
import Website.Data.Error
import Website.Network.API.CRUD
import Website.Data.User (UserKey, getUserLogin, UserLogin, UserUpdate, UserCreate)
import Website.Data.Entry (EntryCreate, EntryUpdate, EntryKey)
import qualified Debug.Trace as T

server :: CookieSettings -> JWTSettings -> FilePath -> ServerT TopAPI (AppM Env Err IO)
server cookieSettings jwtSettings currentDirectory = api
  where
    api a = protected a :<|> unprotected a

    protected :: Authed -> ServerT Protected (AppM Env Err IO)
    protected (Authenticated user) =
      crudEntry user
        :<|> withProtected user getEntries
        :<|> crudUser user
        :<|> withProtected user getUsers
    protected _ =
      crudUnauthed
        :<|> throwError_ Unauthenticated
        :<|> crudUnauthed
        :<|> throwError_ Unauthenticated

    crudUnauthed :: ServerT (CRUDForm x y z) (AppM Env Err IO)
    crudUnauthed =
      (\_ -> throwError_ Unauthenticated)
        :<|> throwError_ Unauthenticated
        :<|> (\_ -> throwError_ Unauthenticated)
        :<|> (\_ _ -> throwError_ Unauthenticated)
        :<|> (\_ -> throwError_ Unauthenticated)
        :<|> (\_ -> throwError_ Unauthenticated)

    unprotected :: Authed -> ServerT Unprotected (AppM Env Err IO)
    unprotected a =
      withUnprotected a getIndex
        :<|> withUnprotected a getLogin
        :<|> login
        :<|> register a cookieSettings jwtSettings
        :<|> serveDirectoryWebApp currentDirectory

    getLogin :: AppM (EnvAuthed (Maybe UserLogin)) Err IO Html
    getLogin = loginForm

    login :: Login -> AppM Env Err IO (SetLoginCookies NoContent)
    login (Login user pass) = do
      c <- asks conn
      eUserId <- runExceptT $ checkUserPassword c (T.pack user) (T.pack pass)
      userId <- either (const $ throwError_ Unauthenticated) pure eUserId
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings userId
      case mApplyCookies of
        Nothing -> throwError_ Unauthenticated
        Just cookies -> pure $ cookies $ addHeader root NoContent
      where
        root = linkText (Proxy @(AuthLogin :> Get '[HTML] Html))

    crudEntry :: UserKey -> ServerT (CRUDForm EntryCreate EntryUpdate EntryKey) (AppM Env Err IO)
    crudEntry key =
             (withProtected key . postEntry)
        :<|> withProtected key entryCreationForm
        :<|> (withProtected key . getEntry)
        :<|> (\ek -> withProtected key . putEntry ek)
        :<|> (withProtected key . getEntryForUpdate)
        :<|> (withProtected key . deleteEntry)

    crudUser :: UserKey -> ServerT (CRUDForm UserCreate UserUpdate UserKey) (AppM Env Err IO)
    crudUser key =
             (withProtected key . postUser)
        :<|> withProtected key userCreationForm
        :<|> (withProtected key . getUser)
        :<|> (\uk -> withProtected key . putUser uk)
        :<|> (withProtected key . getUserForUpdate)
        :<|> (withProtected key . deleteUser)

    -- When using a user token do a couple of things.
    -- 1) Ensure that the user actually exists in the database.
    --    We don't want users who have had their account deleted
    --    still being able to access protected APIs.
    -- 2) Gather up all of the information what we might want to
    --    use when customising pages for the user
    withProtected
      :: MonadIO m'
      => UserKey
      -> ReaderT (EnvAuthed UserLogin) (ExceptT Err m') b
      -> AppM Env Err m' b
    withProtected key m = do
      l <- withError userLookupErrors $ getUserLogin key
      withReaderT (mkEnvAuthed l) m

    -- Similar to `withProtected` but doesn't require that the user
    -- is authenticated. If an authenticated user ID doesn't exist
    -- in the database however, that is an issue.
    withUnprotected
      :: MonadIO m'
      => AuthResult UserKey
      -> ReaderT (EnvAuthed (Maybe UserLogin)) (ExceptT Err m') b
      -> ReaderT Env (ExceptT Err m') b
    withUnprotected (Authenticated userKey) m = do
      userLogin <- withError userLookupErrors $ getUserLogin userKey
      T.traceShowM userLogin
      withReaderT (mkEnvAuthed $ Just userLogin) m
    withUnprotected _ m = withReaderT (mkEnvAuthed Nothing) m
  
    -- User lookups can fail in a couple of ways,
    -- but these are the ones we really care to catch.
    -- A missing user at this stage means that the auth
    -- token was valid as far as the crypto was concerned
    -- but the ID it encoded no longer exists, so the
    -- user shouldn't be authenticated any more.
    userLookupErrors (DbError NotFound) = Unauthenticated
    userLookupErrors (DbError TooManyResults) = Unauthenticated
    userLookupErrors e = e