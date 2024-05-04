module Website.Network.Server where

import Control.Monad
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
import Website.Content.Error
import Website.Content.Forms
import Text.Blaze.Html
import Website.Content.Common
import Servant.HTML.Blaze

server :: CookieSettings -> JWTSettings -> FilePath -> ServerT TopAPI (AppM Env ServerError IO)
server cookieSettings jwtSettings currentDirectory = api
  where
    api auth = protected auth :<|> unprotected auth
    mapServerErrors auth =
      asks . runReaderT
        >=> ReaderT . const . withExceptT (errToServerError auth)

    protected auth@(Authenticated user) =
      crudEntry auth user
        :<|> mapServerErrors auth (getEntries auth user)
        :<|> crudUser auth user
    protected auth = throwAll $ unauthentricated auth

    unprotected auth =
      getIndex auth
        :<|> getLogin auth
        :<|> login auth
        :<|> mapServerErrors auth . register auth cookieSettings jwtSettings
        :<|> serveDirectoryWebApp currentDirectory

    getLogin :: Authed -> AppM Env ServerError IO Html
    getLogin = pure . loginForm

    login :: Authed -> Login -> AppM Env ServerError IO (SetLoginCookies NoContent)
    login auth (Login user pass) = do
      c <- asks conn
      userId <- lift $ withExceptT (const $ unauthentricated auth) $ checkUserPassword c (T.pack user) (T.pack pass)
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings userId
      case mApplyCookies of
        Nothing -> throwError $ unauthentricated auth
        Just cookies -> pure $ cookies $ addHeader root NoContent
      where
        root = linkText (Proxy @(AuthLogin :> Get '[HTML] Html))
    
    crudEntry auth user =
      (mapServerErrors auth . postEntry user)
        :<|> mapServerErrors auth (entryCreationForm auth user)
        :<|> (mapServerErrors auth . getEntry auth user)
        :<|> (\k -> mapServerErrors auth . putEntry user k)
        :<|> (mapServerErrors auth . getEntryForUpdate user)
        :<|> (mapServerErrors auth . deleteEntry user)

    crudUser auth user =
      (mapServerErrors auth . postUser user)
        :<|> mapServerErrors auth (userCreationForm auth user)
        :<|> (mapServerErrors auth . getUser auth user)
        :<|> (\k -> mapServerErrors auth . putUser user k)
        :<|> (mapServerErrors auth . getUserForUpdate user)
        :<|> (mapServerErrors auth . deleteUser user)