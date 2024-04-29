module Test.StateMachine.Types where

import Data.Text (Text)
import GHC.Generics
import Hedgehog hiding (Group)
import Network.HTTP.Client qualified as H
import Test.Db.Entry ()
import Test.Db.User ()
import Website.Auth.Authorisation qualified as Auth
import Website.Data.User
import Web.FormUrlEncoded
import Servant (ToHttpApiData(..))
import GHC.Exts (IsList(fromList))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

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

data TestEntry v = TestEntry
  { testKey     :: Var BS8.ByteString v,
    -- testCreated :: UTCTime,
    testTitle   :: String,
    testValue   :: String
  }
  deriving (Generic)
instance FunctorB TestEntry
instance TraversableB TestEntry

data TestUser v = TestUser
  { testUserEmail    :: Text
  , testUserPassword :: Text
  , testUserGroup    :: Auth.Group
  , testUserJwt      :: Maybe (Var ByteString v)
  }
  deriving (Eq, Generic, Show)
instance FunctorB TestUser
instance TraversableB TestUser

-- Log into the API with the given email and password
data BadLoginType = Random | BadUser | BadPassword
  deriving (Eq, Ord, Show)

data TestLoginBad v = TestLoginBad
  { testType :: BadLoginType
  , testUser :: Text
  , testPass :: Text
  } deriving (Eq, Show, Generic)
instance FunctorB TestLoginBad
instance TraversableB TestLoginBad

instance ToForm (TestLoginBad v) where
  toForm (TestLoginBad _ user pass) = fromList
    [ ("login", toQueryParam user)
    , ("password", toQueryParam pass)
    ]

data TestLogin v = TestLogin
  { testUser :: Text
  , testPass :: Text
  } deriving (Eq, Show, Generic)
instance FunctorB TestLogin
instance TraversableB TestLogin

instance ToForm (TestLogin v) where
  toForm (TestLogin user pass) = fromList
    [ ("login", toQueryParam user)
    , ("password", toQueryParam pass)
    ]

toCreateUser :: TestUser v -> CreateUser
toCreateUser r = CreateUser r.testUserGroup r.testUserEmail r.testUserPassword

-- Get the entries from the API
newtype GetEntries v = GetEntries
  { getEntriesUser :: TestUser v
  } deriving (Show, Generic)
instance FunctorB GetEntries
instance TraversableB GetEntries

data GetEntriesNoAuth v = GetEntriesNoAuth
  deriving (Show, Generic)
instance FunctorB GetEntriesNoAuth
instance TraversableB GetEntriesNoAuth

data GetEntry v = GetEntry
  { getEntryAuth :: TestUser v
  , getEntryId :: Var BS8.ByteString v
  } deriving (Show, Generic)
instance FunctorB GetEntry
instance TraversableB GetEntry

newtype GetEntryNoAuth v = GetEntryNoAuth
  { getEntryId :: Var BS8.ByteString v
  } deriving (Show, Generic)
instance FunctorB GetEntryNoAuth
instance TraversableB GetEntryNoAuth

data CreateEntry v = CreateEntry
  { createEntryAuth :: TestUser v
  , createEntryTitle :: String
  , createEntryValue :: String
  } deriving (Eq, Show, Generic)
instance FunctorB CreateEntry
instance TraversableB CreateEntry

instance ToForm (CreateEntry v) where
  toForm (CreateEntry _ title value) = fromList
    [ ("title", toQueryParam title)
    , ("value", toQueryParam value)
    ]

data CreateEntryNoAuth v = CreateEntryNoAuth
  { createEntryTitle :: String
  , createEntryValue :: String
  } deriving (Eq, Show, Generic)
instance FunctorB CreateEntryNoAuth
instance TraversableB CreateEntryNoAuth

instance ToForm (CreateEntryNoAuth v) where
  toForm (CreateEntryNoAuth title value) = fromList
    [ ("title", toQueryParam title)
    , ("value", toQueryParam value)
    ]

data DeleteEntry v = DeleteEntry
  { deleteEntryAuth :: TestUser v
  , deleteEntryId :: Var BS8.ByteString v
  } deriving (Show, Generic)
instance FunctorB DeleteEntry
instance TraversableB DeleteEntry

newtype DeleteEntryNoAuth v = DeleteEntryNoAuth
  { deleteEntryId :: Var BS8.ByteString v
  } deriving (Show, Generic)
instance FunctorB DeleteEntryNoAuth
instance TraversableB DeleteEntryNoAuth

data UpdateEntry v = UpdateEntry
  { updateEntryAuth :: TestUser v
  , updateEntryId :: Var BS8.ByteString v
  , updateEntryTitle :: String
  , updateEntryValue :: String
  } deriving (Show, Generic)
instance FunctorB UpdateEntry
instance TraversableB UpdateEntry

instance ToForm (UpdateEntry v) where
  toForm (UpdateEntry _ _ title value) = fromList
    [ ("title", toQueryParam title)
    , ("value", toQueryParam value)
    ]

data UpdateEntryNoAuth v = UpdateEntryNoAuth
  { updateEntryId :: Var BS8.ByteString v
  , updateEntryTitle :: String
  , updateEntryValue :: String
  } deriving (Show, Generic)
instance FunctorB UpdateEntryNoAuth
instance TraversableB UpdateEntryNoAuth

instance ToForm (UpdateEntryNoAuth v) where
  toForm (UpdateEntryNoAuth _ title value) = fromList
    [ ("title", toQueryParam title)
    , ("value", toQueryParam value)
    ]

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