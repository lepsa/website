module Website.Network.Server where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text qualified as T
import Data.UUID ()
import Servant
import Servant.Auth.Server
import Website.Auth.Authentication
import Website.Data.User (UserCreate, User(uuid), createUser)
import Website.Network.API
import Website.Network.API.Types
import Website.Types
import Website.Data.Env
import Website.Data.Error
import Website.Content.Forms

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
        :<|> crudUser
    protected _ = throwAll err401

    unprotected =
      getIndex
        :<|> login
        :<|> mapServerErrors . register cookieSettings jwtSettings
        :<|> serveDirectoryWebApp currentDirectory

    login :: Login -> AppM Env ServerError IO (SetCookies NoContent)
    login (Login user pass) = do
      c <- asks conn
      userId <- lift $ withExceptT (const err401) $ checkUserPassword c (T.pack user) (T.pack pass)
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings userId
      case mApplyCookies of
        Nothing -> throwError err401
        Just cookies -> pure $ cookies NoContent
    
    crudEntry =
      (mapServerErrors . postEntry)
        :<|> mapServerErrors entryCreationForm
        :<|> (mapServerErrors . getEntry)
        :<|> (\k -> mapServerErrors . putEntry k)
        :<|> (mapServerErrors . getEntryForUpdate)
        :<|> (mapServerErrors . deleteEntry)

    crudUser =
      (mapServerErrors . postUser)
        :<|> mapServerErrors userCreationForm
        :<|> (mapServerErrors . getUser)
        :<|> (\k -> mapServerErrors . putUser k)
        :<|> (mapServerErrors . getUserForUpdate)
        :<|> (mapServerErrors . deleteUser)