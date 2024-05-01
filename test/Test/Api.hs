module Test.Api where

import Website.Network.API.Types (TopAPI)
import Data.Data
import Website.Network.Server
import Servant.Auth.Server
import Servant
import Website.Types
import Test.Db.Entry (getAllEntries)
import Test.Db.User (getAllUsers)
import Website.Data.User (User)
import Website.Data.Entry (Entry)
import Control.Monad.Reader
import Test.Db
import Website.Data.Env

-- Top level API for testing. This is the main api with a set of test specific routes bolted onto the side.
-- These test APIs only exist in the test suite.
-- TestAPI must go _before_ TopAPI because TopAPI contains a Raw route for handling CSS and JS files.
-- This is a problem because it will match everything and then stop our test routes from working.
type TestTopAPI = TestAPI :<|> TopAPI

-- Top level server for testing. This runs the main server and also bolts some extra functionality on the side.
testTopServer :: CookieSettings -> JWTSettings -> FilePath -> ServerT TestTopAPI (AppM Env ServerError IO)
testTopServer cs jwt fp = testServer :<|> server cs jwt fp

testTopAPI :: Proxy TestTopAPI
testTopAPI = Proxy

-- The API for adding tests to the server for peeking and poking at the database
type TestAPI =
       "reset" :> PostNoContent
  :<|> "getUsers" :> Get '[JSON] [User]
  :<|> "getEntries" :> Get '[JSON] [Entry]

testServer :: ServerT TestAPI (AppM Env ServerError IO)
testServer = reset :<|> getUsers :<|> getEntries

reset :: AppM Env ServerError IO NoContent
reset = do
  asks conn >>= liftIO . resetDb

getUsers :: AppM Env ServerError IO [User]
getUsers = asks conn >>= liftIO . getAllUsers

getEntries :: AppM Env ServerError IO [Entry]
getEntries = asks conn >>= liftIO . getAllEntries