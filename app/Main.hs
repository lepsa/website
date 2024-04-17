module Main where

import Control.Monad
import Database.SQLite.Simple
import Website.Data.Schema
import Website.Network.API.Types
import Website.Network.Server
import Website.Types
import Network.Wai.Handler.Warp
import Servant.Server
import System.Directory

main :: IO ()
main = do
  putStrLn "Starting server"
  currentDirectory <- getCurrentDirectory
  env <-
    Env
      <$> open "db.sqlite"
  either (error . show) pure <=< runAppM env $ do
    createSchema
    runMigrations
  run 8080 $
    serve topAPI $
      hoistServer topAPI (runAppMToHandler env) $
        server currentDirectory