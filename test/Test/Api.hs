module Test.Api where

import           Control.Monad.Reader
import           Data.Data
import           Servant
import           Servant.Auth.Server
import           Test.Db
import           Test.Db.Entry             (getAllEntries)
import           Test.Db.File              (getAllFiles)
import           Test.Db.User              (getAllUsers)
import           Website.Data.Entry        (Entry)
import           Website.Data.Env
import           Website.Data.Error
import           Website.Data.File
import           Website.Data.User         (User)
import           Website.Network.API.Types (TopAPI)
import           Website.Network.Server
import           Website.Types

-- Top level API for testing. This is the main api with a set of test specific routes bolted onto the side.
-- These test APIs only exist in the test suite.
-- TestAPI must go _before_ TopAPI because TopAPI contains a Raw route for handling CSS and JS files.
-- This is a problem because it will match everything and then stop our test routes from working.
type TestTopAPI = TestAPI :<|> TopAPI

-- Top level server for testing. This runs the main server and also bolts some extra functionality on the side.
testTopServer :: CookieSettings -> JWTSettings -> FilePath -> ServerT TestTopAPI (AppM Env Err IO)
testTopServer cs jwt fp = testServer :<|> server cs jwt fp

testTopAPI :: Proxy TestTopAPI
testTopAPI = Proxy

-- The API for adding tests to the server for peeking and poking at the database
type TestAPI =
       "reset" :> PostNoContent
  :<|> "getUsers" :> Get '[JSON] [User]
  :<|> "getEntries" :> Get '[JSON] [Entry]
  :<|> "getFiles" :> Get '[JSON] [File]

testServer :: ServerT TestAPI (AppM Env Err IO)
testServer = reset :<|> getUsers :<|> getEntries :<|> getFiles

reset :: CanAppM c e m => m NoContent
reset = asks conn >>= liftIO . resetDb

getUsers :: CanAppM c e m => m [User]
getUsers = asks conn >>= liftIO . getAllUsers

getEntries :: CanAppM c e m => m [Entry]
getEntries = asks conn >>= liftIO . getAllEntries

getFiles :: CanAppM c e m => m [File]
getFiles = asks conn >>= liftIO . getAllFiles
