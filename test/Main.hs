module Main (main) where

import           Control.Concurrent
import           Hedgehog
import           Network.Connection      (TLSSettings (..))
import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Client.TLS as H
import qualified Network.HTTP.Types      as H
import           Test.Api                (testTopAPI, testTopServer)
import           Test.StateMachine
import           Test.Types
import           Website

main :: IO Bool
main = do
  -- Print a tree of the routes
  -- let dumpRoutes :: (HasServer TestTopAPI '[BasicAuthCfg', CookieSettings, JWTSettings]) => IO ()
  --     dumpRoutes = T.putStrLn
  --       $ layoutWithContext (Proxy @TestTopAPI)
  --       $ BasicAuthCfg' undefined :. defaultCookieSettings :. defaultJWTSettings undefined :. EmptyContext
  -- dumpRoutes

  ready <- newEmptyMVar
  let onStart = putMVar ready ()
  serverThread <- forkIO $ startServer' onStart testTopAPI testTopServer "./db/test_db.sqlite" port

  -- Wait for the server to start
  takeMVar ready

  mgr <- H.newTlsManagerWith $ H.mkManagerSettings (TLSSettingsSimple True undefined undefined) Nothing
  let baseUrl = "https://localhost:" <> show port
      env = TestEnv mgr baseUrl
      reset = do
        -- This request here isn't type checked, but as it
        -- is in the test suite and is _extremely_ simple,
        -- I'm ok with it for the time being.
        req <- H.parseRequest $ baseUrl <> "/reset"
        let req' = req { H.method = H.methodPost }
        res <- H.httpNoBody req' mgr
        pure $ res.responseStatus == H.status204
  results <- checkParallel $ Group "API Tests"
    [ ("API State Machine", propApiTests env reset)
    ]
  killThread serverThread
  pure results
  where
    port = 8081
