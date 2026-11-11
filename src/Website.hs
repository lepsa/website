module Website where

import           Control.Monad                        (void, (<=<))
import           Control.Monad.Identity               (Identity)
import           Control.Monad.Logger                 (NoLoggingT (runNoLoggingT),
                                                       logInfo)
import           Control.Monad.Reader                 (ReaderT, mapReaderT)
import           Crypto.JOSE                          (JWK)
import           Data.Aeson                           (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8           as BSL8
import           Data.Kind                            (Type)
import           Data.List.NonEmpty                   (NonEmpty ((:|)),
                                                       nonEmpty)
import           Data.Maybe                           (Maybe (Just, Nothing))
import           Data.Proxy                           (Proxy (Proxy))
import           Data.Time                            (getCurrentTimeZone)
import           Database.SQLite.Simple               (Only (Only), execute,
                                                       open, query_,
                                                       withTransaction)
import           Network.Wai                          (Request (pathInfo))
import           Network.Wai.Handler.Warp             (Port, defaultSettings,
                                                       runSettings,
                                                       setBeforeMainLoop,
                                                       setHost, setPort)
import           Network.Wai.Middleware.Gzip          (defaultGzipSettings,
                                                       gzip)
import           Network.Wai.Middleware.Prometheus    (PrometheusSettings (prometheusInstrumentApp),
                                                       def,
                                                       instrumentHandlerValue,
                                                       prometheus)
import           Network.Wai.Middleware.RequestLogger (Destination (Handle),
                                                       IPAddrSource (FromSocket),
                                                       OutputFormat (Apache),
                                                       RequestLoggerSettings (destination, outputFormat),
                                                       defaultRequestLoggerSettings,
                                                       mkRequestLogger)
import           Prelude                              (Bool (False, True),
                                                       Either (Left, Right),
                                                       Int, String, either,
                                                       error, filter, foldl,
                                                       mappend, mempty, pure,
                                                       show, ($), (*), (.),
                                                       (/=), (<$>), (<*>), (<>),
                                                       (>>=))
import           Servant.Auth.Server                  (CookieSettings (cookieMaxAge, cookieXsrfSetting),
                                                       JWTSettings,
                                                       XsrfCookieSettings (xsrfExcludeGet),
                                                       defaultCookieSettings,
                                                       defaultJWTSettings,
                                                       defaultXsrfCookieSettings,
                                                       generateKey)
import           Servant.Server                       (Context (EmptyContext, (:.)),
                                                       HasServer (ServerT, hoistServerWithContext),
                                                       ServerError,
                                                       serveWithContext)
import           System.Directory                     (getCurrentDirectory)
import           System.IO                            (FilePath, IO,
                                                       IOMode (AppendMode),
                                                       putStrLn, withFile)
import           System.Log.Handler.Syslog            (Facility (USER),
                                                       Option (PID), openlog)
import           System.Log.Logger                    (Priority (DEBUG),
                                                       addHandler,
                                                       getRootLogger,
                                                       removeHandler,
                                                       saveGlobalLogger,
                                                       setLevel)
import           Website.Auth.Authentication          (BasicAuthCfg' (BasicAuthCfg'))
import           Website.Content.Error                (errToServerError)
import           Website.Data.Env                     (Env (Env), HasEnv (conn))
import           Website.Data.Error                   (Err)
import           Website.Data.Schema                  (createSchema, getJWK,
                                                       insertJWK, runMigrations,
                                                       setupDatabase)
import           Website.Network.API.Types            (topAPI)
import           Website.Network.Server               (server)
import           Website.Types                        (AppM, runAppM,
                                                       runAppMToHandler)

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
    requestLogger <- mkRequestLogger $ defaultRequestLoggerSettings
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
      prometheus def { prometheusInstrumentApp = False } $
      instrumentHandlerValue reqHandler $
      requestLogger $
      gzip defaultGzipSettings $
      serveWithContext api cfg $
        hoistServerWithContext api
          (Proxy @'[BasicAuthCfg', CookieSettings, JWTSettings])
          (runAppMToHandler runErr conf) $
          serverM cookieSettings jwtSettings currentDirectory
  where
    runErr :: Err -> ReaderT Env Identity ServerError
    runErr = mapReaderT runNoLoggingT . errToServerError
    reqHandler req =
      let parts = filter (/= mempty) $ pathInfo req
      in mappend "/" $ case nonEmpty parts of
         Nothing          -> mempty
         Just (seg:|segs) -> foldl (\z p -> z <> "/" <> p) seg segs

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
