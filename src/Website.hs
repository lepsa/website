{-# LANGUAGE TemplateHaskell #-}

module Website where

import           Control.Monad
import           Crypto.JOSE                 (JWK)
import           Data.Aeson                  (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8  as BSL8
import           Data.Kind
import           Data.Proxy
import           Data.Time
import           Database.SQLite.Simple      hiding ((:.))
import           Network.Wai.Handler.Warp
import           Servant.Auth.Server
import           Servant.Server
import           System.Directory
import           Website.Auth.Authentication
import           Website.Content.Error
import           Website.Data.Env
import           Website.Data.Error
import           Website.Data.Schema
import           Website.Network.API.Types
import           Website.Network.Server
import           Website.Types
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import System.IO
import System.Log.Logger
import System.Log.Handler.Syslog
import Control.Monad.Logger

#ifdef TLS
import           Network.Wai.Handler.WarpTLS
import           System.FilePath
#endif

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
  s <- openlog "Website" [PID] USER DEBUG
  getRootLogger >>= saveGlobalLogger . setLevel DEBUG . addHandler s . removeHandler
  currentDirectory <- getCurrentDirectory
  conf <-
    Env
      <$> open dbPath
      <*> getCurrentTimeZone
  void $ runAppM conf $ $(logInfo) "Starting server"
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
  let jwtSettings = defaultJWTSettings jwtKey
      cookieSettings = defaultCookieSettings
        { cookieXsrfSetting = pure $ defaultXsrfCookieSettings
          { xsrfExcludeGet = True
          }
        , cookieMaxAge = pure $ 7 * 24 * 60 * 60 -- 7 days to seconds
        }
      -- Basic auth checks the user/password each time, so it already
      -- handles a user being deleted between user requests.
      cfg = BasicAuthCfg' (conn conf) :. cookieSettings :. jwtSettings :. EmptyContext
      warpSettings = setBeforeMainLoop onStartup
        $ setHost "*6"
        $ setPort port defaultSettings
  withFile "requests.log" AppendMode $ \requestHandle -> do
    requestLogger <- mkRequestLogger $ def
      { outputFormat = Apache FromSocket
      , destination = Handle requestHandle
      }
#if defined(TLS)
    let tls = tlsSettings
                (currentDirectory </> "certificates" </> "certificate.pem")
                (currentDirectory </> "certificates" </> "key.pem")
    runTLS tls
#else
    runSettings
#endif
      warpSettings $
      requestLogger $
      gzip defaultGzipSettings $
      serveWithContext api cfg $
        hoistServerWithContext api
          (Proxy @'[BasicAuthCfg', CookieSettings, JWTSettings])
          (runAppMToHandler errToServerError conf) $
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
          Left s    -> error s
      _ -> error "Too many JWKs in the database"
  where
    c = conn conf
