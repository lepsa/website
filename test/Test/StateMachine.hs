{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.StateMachine where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString qualified as BS
import Data.CaseInsensitive (mk)
import Data.List
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.UUID (UUID)
import GHC.Generics
import Hedgehog hiding (Group)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client qualified as H
import Network.HTTP.Types
import Test.Db.Entry ()
import Test.Db.User ()
import Website.Data.Entry (Entry (..), EntryKey(..))
import Website.Data.User (User (..))
import Website.Auth.Authorisation (Group)

-- Reference the following QFPL blog posts to refresh yourself.
-- https://qfpl.io/posts/intro-to-state-machine-testing-1/
-- https://qfpl.io/posts/intro-to-state-machine-testing-2/
-- https://qfpl.io/posts/intro-to-state-machine-testing-3/

-- What we think that the state of the world should look like.
-- This will often end up mirroring the database in some way, as
-- we want to track the same information, to ensure that the server
-- is doing what we expect.
data ApiState v = ApiState
  { users   :: [TestUser v],
    logins  :: [TestLogin v],
    entries :: [TestEntry v]
  }
  deriving (Generic)
instance FunctorB ApiState
instance TraversableB ApiState

initialState :: ApiState v
initialState =
  ApiState
    { users = []
    , logins = []
    , entries = []
    }

data TestEnv = TestEnv
  { manager :: H.Manager,
    baseUrl :: String
  }

--
-- Main api commands
--

newtype TestEntryKey v = TestEntryKey
  { testKey :: Var Int v
  }
  deriving (Generic)
instance FunctorB TestEntryKey
instance TraversableB TestEntryKey

data TestEntry v = TestEntry
  { testKey     :: TestEntryKey v,
    testCreated :: Var UTCTime v,
    testTitle   :: Var String v,
    testValue   :: Var String v
  }
  deriving (Generic)
instance FunctorB TestEntry
instance TraversableB TestEntry

toEntry :: TestEntry Concrete -> Entry
toEntry e = Entry
  (EntryKey $ concrete e.testKey.testKey)
  (concrete e.testCreated)
  (concrete e.testTitle)
  (concrete e.testValue)

data TestUser v = TestUser
  { testUserId    :: Var UUID v,
    testUserEmail :: Var Text v,
    testUserGroup :: Var Group v
  }
  deriving (Generic)
instance FunctorB TestUser
instance TraversableB TestUser

toUser :: TestUser Concrete -> User
toUser u = User
  (concrete u.testUserId)
  (concrete u.testUserEmail)
  (concrete u.testUserGroup)

-- Log into the API with the given email and password
data TestLogin v = TestLogin
  { testUser :: Var Text v
  , testPass :: Var Text v
  } deriving (Eq, Show, Generic)
instance FunctorB TestLogin
instance TraversableB TestLogin

-- Get the entries from the API
data GetEntries v = GetEntries
  deriving (Eq, Show, Generic)
instance FunctorB GetEntries
instance TraversableB GetEntries

--
-- Side channel commands
--

-- Reset the database
data Reset v = Reset
  deriving (Eq, Show, Generic)
instance FunctorB Reset
instance TraversableB Reset

-- Get the users from the database
data GetTestUsers v = GetTestUsers
  deriving (Eq, Show, Generic)
instance FunctorB GetTestUsers
instance TraversableB GetTestUsers

-- Get the entries from the database
data GetTestEntries v = GetTestEntries
  deriving (Eq, Show, Generic)
instance FunctorB GetTestEntries
instance TraversableB GetTestEntries

--
-- Generators
--

genMkLogin :: (MonadGen m) => m (Text, Text)
genMkLogin = (,)
  <$> Gen.text (Range.linear 1 100) Gen.unicode
  <*> Gen.text (Range.linear 1 100) Gen.unicode

--
-- Main api commands
--

type CanStateM gen m = (MonadGen gen, MonadFail m, MonadThrow m, MonadIO m, MonadTest m)

cLogin :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cLogin env = Command gen execute
  -- Check that the state is in a position where we can run this command
  [ Require $ \state _input -> not (null state.logins),
    -- Update the state as needed to reflect the new reality
    Update $ \state _input _output -> state,
    -- Test that the state updates and input/output matches what we are
    -- expecting to have occured.
    Ensure $ \_oldState _newState _input output -> do
      output.responseStatus === status204
      let cookies = filter (\(k, v) -> k == mk "Set-Cookie" && not (BS.null v)) output.responseHeaders
      length cookies === 1
  ]
  where
    -- From our state, generate a value to be used for the test.
    -- In this case, pick a user to log in from the list of users.
    gen :: ApiState Symbolic -> Maybe (gen (TestLogin Symbolic))
    gen apiState = if null apiState.logins
      then Nothing
      else Just $ Gen.element apiState.logins
    -- What we want to do with this value
    execute (TestLogin user pass) = do
      req <- H.parseRequest (env.baseUrl <> "/login")
      let user' = encodeUtf8 $ concrete user
          pass' = encodeUtf8 $ concrete pass
          req' = H.applyBasicAuth user' pass' $ req { H.method = methodPost }
      liftIO $ H.httpNoBody req' env.manager

--
-- Side channel commands.
-- These are used to check that the server is doing what we expect in
-- multiple ways. Things like checking what the DB thinks the world
-- looks like vs what the API thinks the world looks like.
--

cTestReset :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cTestReset env = Command gen execute
  [ Update $ \_state _ _ -> initialState
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (Reset Symbolic))
    gen _apiState = Just $ pure Reset
    execute :: Reset Concrete -> m ()
    execute _reset = do
      req <- H.parseRequest (env.baseUrl <> "/reset")
      let req' = req {H.method = methodPost}
      annotate $ show req
      res <- liftIO $ H.httpNoBody req' env.manager
      annotate $ show res
      res.responseStatus === status204


cTestGetUsers :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cTestGetUsers env = Command gen execute
  [ Ensure $ \_oldState newState _input output -> sortBy ordUser (toUser <$> newState.users) === sortBy ordUser output
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetTestUsers Symbolic))
    gen _ = Just $ pure GetTestUsers
    execute :: GetTestUsers Concrete -> m [User]
    execute _ = do
      req <- H.parseRequest (env.baseUrl <> "/getUsers")
      let req' = req {H.method = methodGet}
      res <- liftIO $ H.httpLbs req' env.manager
      assert $ res.responseStatus == status200
      either fail pure $ eitherDecode @[User] res.responseBody 
    ordUser uA uB = compare uA.userId uB.userId

--
-- The prop test machine
--

propApiTests :: TestEnv -> IO Bool -> Property
propApiTests env reset = withTests 100 . property $ do
  worked <- evalIO reset
  if worked then pure () else fail "Could not reset the API"
  let commands = ($ env) <$> [cLogin, cTestGetUsers, cTestReset]
  actions <- forAll $ Gen.sequential (Range.linear 1 1000) initialState commands
  executeSequential initialState actions
