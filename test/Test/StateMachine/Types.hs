{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.StateMachine.Types where

import Data.Text (Text)
import Data.Time
import GHC.Generics
import Hedgehog hiding (Group)
import Network.HTTP.Client qualified as H
import Test.Db.Entry ()
import Test.Db.User ()
import Website.Data.Entry (Entry (..), EntryKey(..))
import Website.Auth.Authorisation qualified as Auth
import Website.Data.User

-- What we think that the state of the world should look like.
-- This will often end up mirroring the database in some way, as
-- we want to track the same information, to ensure that the server
-- is doing what we expect.
data ApiState v = ApiState
  { users   :: [TestUser v],
    entries :: [TestEntry v]
  }
  deriving (Generic)
instance FunctorB ApiState
instance TraversableB ApiState

initialState :: ApiState v
initialState =
  ApiState
    { users = []
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
  { testKey :: Int
  }
  deriving (Generic)
instance FunctorB TestEntryKey
instance TraversableB TestEntryKey

data TestEntry v = TestEntry
  { testKey     :: TestEntryKey v,
    testCreated :: UTCTime,
    testTitle   :: String,
    testValue   :: String
  }
  deriving (Generic)
instance FunctorB TestEntry
instance TraversableB TestEntry

toEntry :: TestEntry Concrete -> Entry
toEntry e = Entry
  (EntryKey e.testKey.testKey)
  e.testCreated
  e.testTitle
  e.testValue

data TestUser v = TestUser
  { testUserEmail    :: Text
  , testUserPassword :: Text
  , testUserGroup    :: Auth.Group
  }
  deriving (Generic, Show)
instance FunctorB TestUser
instance TraversableB TestUser

-- Log into the API with the given email and password
data TestLogin v = TestLogin
  { testUser :: Text
  , testPass :: Text
  } deriving (Eq, Show, Generic)
instance FunctorB TestLogin
instance TraversableB TestLogin

toCreateUser :: TestUser v -> CreateUser
toCreateUser r = CreateUser r.testUserGroup r.testUserEmail r.testUserPassword

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