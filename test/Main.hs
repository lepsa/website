module Main (main) where

import Control.Concurrent
import Hedgehog
import Network.HTTP.Client qualified as H
import Network.HTTP.Types qualified as H
import Test.Api (testTopAPI, testTopServer)
import Test.StateMachine
import Test.StateMachine.Types
import Website

main :: IO Bool
main = do
  -- Print a tree of the routes
  -- let dumpRoutes :: (HasServer TestTopAPI '[BasicAuthCfg', CookieSettings, JWTSettings]) => IO ()
  --     dumpRoutes = T.putStrLn
  --       $ layoutWithContext (Proxy @TestTopAPI)
  --       $ BasicAuthCfg' undefined :. defaultCookieSettings :. defaultJWTSettings undefined :. EmptyContext
  -- dumpRoutes

  serverThread <- forkIO $ startServer' testTopAPI testTopServer "test_db.sqlite" port
  
  -- TODO
  -- find some way to have the server tell us when it is up and stable.
  -- This 3 second delay is janky and _will_ break sooner than later when
  -- the server has to start doing large migrations or actually tring to
  -- do anything more than trying to open the DB and host HTTP routes.
  threadDelay $ 3 * 1000 * 1000
  
  mgr <- H.newManager H.defaultManagerSettings
  let baseUrl = "http://localhost:" <> show port
      env = TestEnv mgr baseUrl
      reset = do
        -- This request here isn't type checked, but as it
        -- is in the test suite and is _extremely_ simple,
        -- I'm ok with it for the time being.
        req <- H.parseRequest $ baseUrl <> "/reset"
        let req' = req { H.method = H.methodPost }
        res <- H.httpNoBody req' mgr
        pure $ res.responseStatus == H.status204
  results <- checkSequential $ Group "API Tests"
    [ ("API State Machine", propApiTests env reset)
    ]
  killThread serverThread
  pure results
  where
    port = 8080
