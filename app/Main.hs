module Main where

import Control.Monad
import Data.Time
import Database.SQLite.Simple hiding ((:.))
import Network.Wai.Handler.Warp
import Servant.Server
import System.Directory
import Website.Data.Schema
import Website.Network.API.Types
import Website.Data.User
import Website.Network.Server
import Website.Types
import Crypto.JOSE (JWK)
import Servant.Auth.Server
import Data.Proxy
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL8

main :: IO ()
main = do
  putStrLn "Starting server"
  currentDirectory <- getCurrentDirectory
  env <-
    Env
      <$> open "db.sqlite"
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
  run 8080 $
    serveWithContext topAPI cfg $
      hoistServerWithContext topAPI (Proxy @'[BasicAuthCfg', CookieSettings, JWTSettings]) (runAppMToHandler env) $
        server defaultCookieSettings jwtSettings currentDirectory

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