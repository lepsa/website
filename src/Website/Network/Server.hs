module Website.Network.Server where

import Website.Network.API
import Website.Network.API.Types
import Website.Types
import Servant
import Servant.Auth.Server (AuthResult(Authenticated), CookieSettings, JWTSettings, acceptLogin, throwAll)
import Control.Exception (throw)
import Control.Monad.Reader
import Website.Data.User
import Website.Auth.Authentication
import Data.UUID.V4 (nextRandom)
import Data.UUID ()
import Website.Auth.Authorisation hiding (User)
import Control.Monad.Except (withExceptT)
import Website.Data.Entry (EntryCreate, EntryUpdate, EntryKey)
import Website.Network.API.CRUD

server :: CookieSettings -> JWTSettings -> FilePath -> ServerT TopAPI (AppM Env ServerError IO)
server cookieSettings jwtSettings currentDirectory =
  protected :<|> unprotected
  where
    mapServerErrors :: Monad m => AppM c Err m a -> AppM c ServerError m a
    mapServerErrors m = do
      c <- ask
      let m' = runReaderT m c
      ReaderT $ const $ withExceptT errToServerError m'
    protected (Authenticated _user) =
      crudEntry :<|>
      mapServerErrors getEntries
    protected _ = throwAll err401
    unprotected =
      getIndex :<|>
      login :<|>
      serveDirectoryWebApp currentDirectory
    login (Login user pass) =
      if user == "user" && pass == "pass"
      then do
        userId <- liftIO nextRandom
        let loggedInUser = User userId Admin ""
        mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings loggedInUser
        case mApplyCookies of
          Nothing -> throwError err401
          Just cookies -> pure $ cookies NoContent
      else throw err401
    crudEntry :: ServerT (CRUD EntryCreate EntryUpdate EntryKey) (AppM Env ServerError IO)
    crudEntry =
           (mapServerErrors . postEntry)
      :<|>  mapServerErrors getEntryInitial
      :<|> (mapServerErrors . getEntry)
      :<|> (\k -> mapServerErrors . putEntry k)
      :<|> (mapServerErrors . getEntryForUpdate)
      :<|> (mapServerErrors . deleteEntry)