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
server cookieSettings jwtSettings currentDirectory =
  protected :<|> unprotected
  where
    mapServerErrors =
      asks . runReaderT
        >=> ReaderT . const . withExceptT errToServerError

    protected (Authenticated user) =
      crudEntry user
        :<|> mapServerErrors (getEntries user)
        :<|> crudUser user
    protected _ = throwAll unauthentricated

    unprotected =
      getIndex
        :<|> getLogin
        :<|> login
        :<|> mapServerErrors . register cookieSettings jwtSettings
        :<|> serveDirectoryWebApp currentDirectory

    getLogin :: AppM Env ServerError IO Html
    getLogin = pure loginForm

    login :: Login -> AppM Env ServerError IO (SetLoginCookies NoContent)
    login (Login user pass) = do
      c <- asks conn
      userId <- lift $ withExceptT (const unauthentricated) $ checkUserPassword c (T.pack user) (T.pack pass)
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings userId
      case mApplyCookies of
        Nothing -> throwError unauthentricated
        Just cookies -> pure $ cookies $ addHeader root NoContent
      where
        root = linkText (Proxy @(Get '[HTML] Html))
    
    crudEntry user =
      (mapServerErrors . postEntry user)
        :<|> mapServerErrors (entryCreationForm user)
        :<|> (mapServerErrors . getEntry user)
        :<|> (\k -> mapServerErrors . putEntry user k)
        :<|> (mapServerErrors . getEntryForUpdate user)
        :<|> (mapServerErrors . deleteEntry user)

    crudUser user =
      (mapServerErrors . postUser user)
        :<|> mapServerErrors (userCreationForm user)
        :<|> (mapServerErrors . getUser user)
        :<|> (\k -> mapServerErrors . putUser user k)
        :<|> (mapServerErrors . getUserForUpdate user)
        :<|> (mapServerErrors . deleteUser user)