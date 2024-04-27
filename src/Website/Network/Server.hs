module Website.Network.Server where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text qualified as T
import Data.UUID ()
import Servant
import Servant.Auth.Server
import Website.Auth.Authentication
import Website.Data.User
import Website.Network.API
import Website.Network.API.Types
import Website.Types

server :: CookieSettings -> JWTSettings -> FilePath -> ServerT TopAPI (AppM Env ServerError IO)
server cookieSettings jwtSettings currentDirectory =
  protected :<|> unprotected
  where
    mapServerErrors =
      asks . runReaderT
        >=> ReaderT . const . withExceptT errToServerError

    protected (Authenticated _user) =
      crudEntry
        :<|> mapServerErrors getEntries
    protected _ = throwAll err401

    unprotected =
      getIndex
        :<|> login
        :<|> register
        :<|> serveDirectoryWebApp currentDirectory

    login :: Login -> AppM Env ServerError IO (SetCookies NoContent)
    login (Login user pass) = do
      c <- asks conn
      userId <- lift $ withExceptT (const err401) $ checkUserPassword c (T.pack user) (T.pack pass)
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings userId
      case mApplyCookies of
        Nothing -> throwError err401
        Just cookies -> pure $ cookies NoContent
    
    register :: CreateUser -> AppM Env ServerError IO (SetCookies NoContent)
    register cUser = do
      user <- mapServerErrors $ createUser cUser
      mApplyCookies <- mapServerErrors $ liftIO $ acceptLogin cookieSettings jwtSettings user.userId
      case mApplyCookies of
        Nothing -> throwError err500
        Just cookies -> pure $ cookies NoContent

    crudEntry =
      (mapServerErrors . postEntry)
        :<|> mapServerErrors getEntryInitial
        :<|> (mapServerErrors . getEntry)
        :<|> (\k -> mapServerErrors . putEntry k)
        :<|> (mapServerErrors . getEntryForUpdate)
        :<|> (mapServerErrors . deleteEntry)