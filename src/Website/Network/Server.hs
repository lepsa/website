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

server :: forall c e m. CanAppM c e m => CookieSettings -> JWTSettings -> FilePath -> ServerT TopAPI m
server cookieSettings jwtSettings currentDirectory = api
  where
    api auth = protected auth :<|> unprotected auth

    protected :: Authed -> ServerT Protected m
    protected auth@(Authenticated user) =
      crudEntry auth user
        :<|> getEntries auth user
        :<|> crudUser auth user
    protected _ =
      crudUnauthed
        :<|> throwError_ Unauthenticated
        :<|> crudUnauthed
    
    crudUnauthed :: ServerT (CRUDForm x y z) m
    crudUnauthed =
      (\_ -> throwError_ Unauthenticated)
        :<|> throwError_ Unauthenticated
        :<|> (\_ -> throwError_ Unauthenticated)
        :<|> (\_ _ -> throwError_ Unauthenticated)
        :<|> (\_ -> throwError_ Unauthenticated)
        :<|> (\_ -> throwError_ Unauthenticated)

    unprotected :: Authed -> ServerT Unprotected m
    unprotected auth =
      getIndex auth
        :<|> getLogin auth
        :<|> login
        :<|> register auth cookieSettings jwtSettings
        :<|> serveDirectoryWebApp currentDirectory

    getLogin :: Authed -> m Html
    getLogin = pure . loginForm

    login :: Login -> m (SetLoginCookies NoContent)
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
    
    crudEntry auth user =
      postEntry user
        :<|> entryCreationForm auth user
        :<|> getEntry auth user
        :<|> putEntry user
        :<|> getEntryForUpdate user
        :<|> deleteEntry user

    crudUser auth user =
      postUser user
        :<|> userCreationForm auth user
        :<|> getUser auth user
        :<|> putUser user
        :<|> getUserForUpdate user
        :<|> deleteUser user