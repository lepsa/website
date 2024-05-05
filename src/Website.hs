module Website where

import Control.Monad
import Crypto.JOSE (JWK)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Proxy
import Data.Time
import Database.SQLite.Simple hiding ((:.))
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant.Auth.Server
import Servant.Server
import System.Directory
import Website.Data.Schema
import Website.Network.API.Types
import Website.Network.Server
import Website.Types
import Data.Kind
import Website.Data.Env
import Website.Auth.Authentication
import Website.Data.Error
import Website.Content.Error
import Control.Monad.Reader

-- GHC gets upset when trying to add a type signature here, even if it comes from HLS.
-- It compiles without it, so something is clearly being infered correctly so I'm going
-- to leave it as is.

startServer'
  :: (HasServer (api :: Type) '[BasicAuthCfg', CookieSettings, JWTSettings])
  => IO ()
  -> Proxy api
  -> (CookieSettings -> JWTSettings -> FilePath -> ServerT api (AppM Env Err IO))
  -> String
  -> Port
  -> IO ()
startServer' onStartup api serverM dbPath port = do
  putStrLn "Starting server"
  currentDirectory <- getCurrentDirectory
  conf <-
    Env
      <$> open dbPath
      <*> getCurrentTimeZone
  -- Do all the steps to get our database up and running as
  -- we expect it to be.
  either (error . show @Err) pure <=< runAppM conf $ do
    -- Pragma and feature support
    setupDatabase
    -- Table schema
    createSchema
    -- Update the schema to what the application wants
    runMigrations
  jwtKey <- getJwtKey conf
  let -- TODO: Find a way to check if the user identified
      -- by the JWT/Cookie still exists, as it is possible that a
      -- user accound is deleted while the JWT is valid, meaning
      -- that the website will accept a non-associated UUID as
      -- user
      jwtSettings = defaultJWTSettings jwtKey
      cookieSettings = defaultCookieSettings
        { cookieXsrfSetting = pure defaultXsrfCookieSettings
          { xsrfExcludeGet = True
          }
        , cookieMaxAge = pure $ 7 * 24 * 60 * 60 -- 7 days to seconds
        }
      -- Basic auth checks the user/password each time, so it already
      -- handles a user being deleted between user requests.
      cfg = BasicAuthCfg' (conn conf) :. cookieSettings :. jwtSettings :. EmptyContext
      warpSettings = setBeforeMainLoop onStartup $ setPort port defaultSettings
  let tls = defaultTlsSettings
  runTLS tls warpSettings $
    serveWithContext api cfg $
      hoistServerWithContext api
        (Proxy @'[BasicAuthCfg', CookieSettings, JWTSettings])
        (runAppMToHandler (flip runReaderT (mkEnvAuthed Nothing conf) . errToServerError) conf) $
        serverM cookieSettings jwtSettings currentDirectory

startServer :: String -> Int -> IO ()
startServer = startServer' (pure ()) topAPI server

getJwtKey :: HasEnv c => c -> IO JWK
getJwtKey conf = do
  withTransaction c $ do
    jsons <- query_ c getJWK
    case jsons of
      [] -> do
        putStrLn "No JWK found, making a new one."
        jwk <- generateKey
        execute c insertJWK (Only $ BSL8.unpack $ encode jwk)
        pure jwk
      [Only json] -> do
        putStrLn "Found a JWK entry, decoding"
        case eitherDecode $ BSL8.pack json of
          Right jwk -> pure jwk
          Left s -> error s
      _ -> error "Too many JWKs in the database"
  where
    c = conn conf