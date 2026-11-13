{-# LANGUAGE ScopedTypeVariables #-}

module Website.Network.Server where

import           Control.Monad.Except      (withError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (withReaderT)
import           Data.UUID                 ()
import           Prelude                   (IO, Maybe (Just, Nothing), ($), (.))
import           Servant                   (HasServer (ServerT), Proxy (Proxy),
                                            hoistServer, serveDirectoryWebApp,
                                            (:<|>) ((:<|>)))
import           Servant.Auth.Server       (AuthResult (Authenticated),
                                            CookieSettings, JWTSettings)
import           System.FilePath           (FilePath, (</>))
import           Website.Content.Common    (Authed)
import           Website.Data.Entry        (EntryCreate, EntryKey, EntryUpdate)
import           Website.Data.Env          (Env, EnvAuthed, mkEnvAuthed)
import           Website.Data.Error        (Err (Unauthenticated), throwError_,
                                            userLookupErrors)
import           Website.Data.File         (FileId)
import           Website.Data.User         (UserCreate, UserKey, UserLogin,
                                            UserUpdate, getUserLogin)
import qualified Website.Network.API       as API
import           Website.Network.API.CRUD  (CRUDFile, CRUDForm)
import           Website.Network.API.Types (FileUpload, TopAPI)
import           Website.Types             (AppM)

server :: CookieSettings -> JWTSettings -> FilePath -> ServerT TopAPI (AppM Env Err IO)
server cookieSettings jwtSettings currentDirectory = api
  where
    api a = unprotected a API.indexGet
       :<|> unprotected a API.loginGet
       :<|> unprotected a . API.loginPost cookieSettings jwtSettings
       :<|> protected a (API.logoutPost cookieSettings)
       :<|> unprotected a . API.register cookieSettings jwtSettings
       :<|> crudUser a
       :<|> protected a API.usersGet
       :<|> crudEntry a
       :<|> unprotected a . API.entryByNameGet
       :<|> unprotected a API.entriesGet
       :<|> crudFile a
       :<|> unprotected a API.filesGet
       :<|> serveDirectoryWebApp (currentDirectory </> "static")

crudEntry :: Authed -> ServerT (CRUDForm EntryCreate EntryUpdate EntryKey) (AppM Env Err IO)
crudEntry a =
       protected a . API.entryCreatePost
  :<|> protected a API.entryCreateGet
  -- Unprotected, as q want the internet at large to be able to read this.
  :<|> unprotected a . API.entryGet
  :<|> (\key -> protected a . API.entryUpdatePut key)
  :<|> protected a . API.entryUpdateGet
  :<|> protected a . API.entryDelete

crudUser :: Authed -> ServerT (CRUDForm UserCreate UserUpdate UserKey) (AppM Env Err IO)
crudUser a = hoistServer (Proxy @(CRUDForm UserCreate UserUpdate UserKey)) (protected a) $
       API.userCreatePost
  :<|> API.userCreateGet
  :<|> API.userGet
  :<|> API.userUpdatePut
  :<|> API.userUpdateGet
  :<|> API.userDelete

crudFile :: Authed -> ServerT (CRUDFile FileUpload FileUpload FileId) (AppM Env Err IO)
crudFile a =
       protected a . API.createFilePost
  :<|> protected a API.createFileGet
  :<|> unprotected a . API.fileGet
  -- :<|> protected a undefined
  -- :<|> protected a undefined
  :<|> protected a . API.fileDelete

-- When using a user token do a couple of things.
-- 1) Ensure that the user actually exists in the database.
--    We don't want users who have had their account deleted
--    still being able to access protected APIs.
-- 2) Gather up all of the information what we might want to
--    use when customising pages for the user
protected
  :: MonadIO m
  => Authed
  -> AppM (EnvAuthed UserLogin) Err m b
  -> AppM Env Err m b
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
unprotected
  :: MonadIO m
  => Authed
  -> AppM (EnvAuthed (Maybe UserLogin)) Err m b
  -> AppM Env Err m b
unprotected (Authenticated userKey) m = do
  userLogin <- withError userLookupErrors $ getUserLogin userKey
  withReaderT (mkEnvAuthed $ Just userLogin) m
unprotected _ m = withReaderT (mkEnvAuthed Nothing) m
