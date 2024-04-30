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
  { key     :: Var BS8.ByteString v,
    -- created :: UTCTime,
    title   :: String,
    value   :: String
  }
  deriving (Generic)
instance FunctorB TestEntry
instance TraversableB TestEntry

data TestUser v = TestUser
  { email    :: Text
  , password :: Text
  , group    :: Auth.Group
  , jwt      :: Maybe (Var ByteString v)
  }
  deriving (Eq, Generic, Show)
instance FunctorB TestUser
instance TraversableB TestUser

-- Log into the API with the given email and password
data BadLoginType = Random | BadUser | BadPassword
  deriving (Eq, Ord, Show)

data TestLoginBad v = TestLoginBad
  { type_ :: BadLoginType
  , user  :: Text
  , pass  :: Text
  } deriving (Eq, Show, Generic)
instance FunctorB TestLoginBad
instance TraversableB TestLoginBad

instance ToForm (TestLoginBad v) where
  toForm (TestLoginBad _ user pass) = fromList
    [ ("login", toQueryParam user)
    , ("password", toQueryParam pass)
    ]

data TestLogin v = TestLogin
  { user :: Text
  , pass :: Text
  } deriving (Eq, Show, Generic)
instance FunctorB TestLogin
instance TraversableB TestLogin

instance ToForm (TestLogin v) where
  toForm (TestLogin user pass) = fromList
    [ ("login", toQueryParam user)
    , ("password", toQueryParam pass)
    ]

toUserCreate :: TestUser v -> UserCreate
toUserCreate r = UserCreate r.group r.email r.password

-- Get the entries from the API
newtype GetEntries v = GetEntries
  { user :: TestUser v
  } deriving (Show, Generic)
instance FunctorB GetEntries
instance TraversableB GetEntries

newtype GetEntriesBadAuth v = GetEntriesBadAuth
  { user :: Maybe (TestUser v)
  }
  deriving (Show, Generic)
instance FunctorB GetEntriesBadAuth
instance TraversableB GetEntriesBadAuth

data GetEntry v = GetEntry
  { user :: TestUser v
  , key :: Var BS8.ByteString v
  } deriving (Show, Generic)
instance FunctorB GetEntry
instance TraversableB GetEntry

data GetEntryBadAuth v = GetEntryBadAuth
  { key :: Var BS8.ByteString v
  , user :: Maybe (TestUser v)
  } deriving (Show, Generic)
instance FunctorB GetEntryBadAuth
instance TraversableB GetEntryBadAuth

data CreateEntry v = CreateEntry
  { user :: TestUser v
  , title :: String
  , value :: String
  } deriving (Eq, Show, Generic)
instance FunctorB CreateEntry
instance TraversableB CreateEntry

instance ToForm (CreateEntry v) where
  toForm (CreateEntry _ title value) = fromList
    [ ("title", toQueryParam title)
    , ("value", toQueryParam value)
    ]

data CreateEntryBadAuth v = CreateEntryBadAuth
  { title :: String
  , value :: String
  , user  :: Maybe (TestUser v)
  } deriving (Eq, Show, Generic)
instance FunctorB CreateEntryBadAuth
instance TraversableB CreateEntryBadAuth

instance ToForm (CreateEntryBadAuth v) where
  toForm (CreateEntryBadAuth title value _) = fromList
    [ ("title", toQueryParam title)
    , ("value", toQueryParam value)
    ]

data DeleteEntry v = DeleteEntry
  { user :: TestUser v
  , key :: Var BS8.ByteString v
  } deriving (Show, Generic)
instance FunctorB DeleteEntry
instance TraversableB DeleteEntry

data DeleteEntryBadAuth v = DeleteEntryBadAuth
  { key :: Var BS8.ByteString v
  , user :: Maybe (TestUser v)
  } deriving (Show, Generic)
instance FunctorB DeleteEntryBadAuth
instance TraversableB DeleteEntryBadAuth

data UpdateEntry v = UpdateEntry
  { user :: TestUser v
  , key :: Var BS8.ByteString v
  , title :: String
  , value :: String
  } deriving (Show, Generic)
instance FunctorB UpdateEntry
instance TraversableB UpdateEntry

instance ToForm (UpdateEntry v) where
  toForm (UpdateEntry _ _ title value) = fromList
    [ ("title", toQueryParam title)
    , ("value", toQueryParam value)
    ]

data UpdateEntryBadAuth v = UpdateEntryBadAuth
  { key :: Var BS8.ByteString v
  , title :: String
  , value :: String
  , user :: Maybe (TestUser v)
  } deriving (Show, Generic)
instance FunctorB UpdateEntryBadAuth
instance TraversableB UpdateEntryBadAuth

instance ToForm (UpdateEntryBadAuth v) where
  toForm (UpdateEntryBadAuth _ title value _) = fromList
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