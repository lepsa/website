module Website where

import Control.Monad
import Crypto.JOSE (JWK)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Proxy
import Data.Time
import Database.SQLite.Simple hiding ((:.))
import Network.Wai.Handler.Warp
import Servant.Auth.Server
import Servant.Server
import System.Directory
import Website.Data.Schema
import Website.Data.User
import Website.Network.API.Types
import Website.Network.Server
import Website.Types
import Data.Kind
import Website.Data.Env

-- GHC gets upset when trying to add a type signature here, even if it comes from HLS.
-- It compiles without it, so something is clearly being infered correctly so I'm going
-- to leave it as is.

startServer'
  :: (HasServer (api :: Type) '[BasicAuthCfg', CookieSettings, JWTSettings])
  => Proxy api
  -> (CookieSettings -> JWTSettings -> FilePath -> ServerT api (AppM Env ServerError IO))
  -> String
  -> Port
  -> IO ()
startServer' api serverM dbPath port = do
  putStrLn "Starting server"
  currentDirectory <- getCurrentDirectory
  env <-
    Env
      <$> open dbPath
      <*> getCurrentTimeZone
  -- Do all the steps to get our database up and running as
  -- we expect it to be.
  either (error . show) pure <=< runAppM env $ do
    -- Pragma and feature support
    setupDatabase
    -- Table schema
    createSchema
    -- Update the schema to what the application wants
    runMigrations
  jwtKey <- getJwtKey env
  let jwtSettings = defaultJWTSettings jwtKey
      cfg = BasicAuthCfg' (conn env) :. defaultCookieSettings :. jwtSettings :. EmptyContext
  run port $
    serveWithContext api cfg $
      hoistServerWithContext api (Proxy @'[BasicAuthCfg', CookieSettings, JWTSettings]) (runAppMToHandler env) $
        serverM defaultCookieSettings jwtSettings currentDirectory

startServer :: String -> Int -> IO ()
startServer = startServer' topAPI server

getJwtKey :: Env -> IO JWK
getJwtKey env = do
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
    c = conn env