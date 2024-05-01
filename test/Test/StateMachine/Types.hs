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
import qualified Data.ByteString.Char8 as BS8
import Website.Auth.Authorisation (Group)
import Data.Maybe

-- What we think that the state of the world should look like.
-- This will often end up mirroring the database in some way, as
-- we want to track the same information, to ensure that the server
-- is doing what we expect.
data ApiState v = ApiState
  { users   :: [TestUser v],
    entries :: [TestEntry v]
  } deriving (Generic)
instance FunctorB ApiState
instance TraversableB ApiState

initialState :: ApiState v
initialState =
  ApiState
    { users = mempty
    , entries = mempty
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
    title   :: Text,
    value   :: Text
  }
  deriving (Generic)
instance FunctorB TestEntry
instance TraversableB TestEntry

data TestUser v = TestUser
  { key      :: Var BS8.ByteString v
  , email    :: Text
  , password :: Text
  , group    :: Auth.Group
  }
  deriving (Eq, Generic, Show)
instance FunctorB TestUser
instance TraversableB TestUser

data Auth v
  = Normal (TestUser v)
  | Bad (Maybe (TestUser v))
  deriving (Eq, Generic, Show)
instance FunctorB Auth
instance TraversableB Auth

-- Log into the API with the given email and password
data LoginType = Good | Random | BadUser | BadPassword
  deriving (Eq, Ord, Show)

data TestLogin v = TestLogin
  { type_ :: LoginType
  , user  :: Text
  , pass  :: Text
  } deriving (Eq, Show, Generic)
instance FunctorB TestLogin
instance TraversableB TestLogin

instance ToForm (TestLogin v) where
  toForm (TestLogin _ user pass) = fromList
    [ ("login", toQueryParam user)
    , ("password", toQueryParam pass)
    ]

toUserCreate :: TestUser v -> UserCreate
toUserCreate r = UserCreate r.group r.email r.password

-- Get the entries from the API
newtype GetEntries v = GetEntries
  { auth :: Auth v
  } deriving (Show, Generic)
instance FunctorB GetEntries
instance TraversableB GetEntries

data EntryAccess v = EntryAccess
  { auth :: Auth v
  , key :: Var BS8.ByteString v
  } deriving (Show, Generic)
instance FunctorB EntryAccess
instance TraversableB EntryAccess

data CreateEntry v = CreateEntry
  { auth :: Auth v
  , title :: Text
  , value :: Text
  } deriving (Eq, Show, Generic)
instance FunctorB CreateEntry
instance TraversableB CreateEntry

instance ToForm (CreateEntry v) where
  toForm (CreateEntry _ title value) = fromList
    [ ("title", toQueryParam title)
    , ("value", toQueryParam value)
    ]

data UpdateEntry v = UpdateEntry
  { auth :: Auth v
  , key :: Var BS8.ByteString v
  , title :: Text
  , value :: Text
  } deriving (Show, Generic)
instance FunctorB UpdateEntry
instance TraversableB UpdateEntry

instance ToForm (UpdateEntry v) where
  toForm (UpdateEntry _ _ title value) = fromList
    [ ("title", toQueryParam title)
    , ("value", toQueryParam value)
    ]

data RegisterUser v = RegisterUser
  { email :: Text
  , password :: Text
  , group :: Group
  } deriving (Show, Generic)
instance FunctorB RegisterUser
instance TraversableB RegisterUser

instance ToForm (RegisterUser v) where
  toForm (RegisterUser email password group) = fromList
    [ ("email", toQueryParam email)
    , ("password", toQueryParam password)
    , ("group", toQueryParam group)
    ]

data RegisterOutput = RegisterOutput
  { cookie :: BS8.ByteString
  , key :: BS8.ByteString
  } deriving (Show, Generic)

data CreateUser v = CreateUser
  { auth :: Auth v
  , group :: Group
  , email :: Text
  , password :: Text
  } deriving (Show, Generic)
instance FunctorB CreateUser
instance TraversableB CreateUser
instance ToForm (CreateUser v) where
  toForm (CreateUser _ group email password) = fromList
    [ ("group", toQueryParam group)
    , ("email", toQueryParam email)
    , ("password", toQueryParam password)
    ]

data PasswordUpdate v = PasswordUpdate
  { oldPassword :: Text
  , newPassword :: Text
  } deriving (Show, Generic)
instance FunctorB PasswordUpdate
instance TraversableB PasswordUpdate

data UpdateUser v = UpdateUser
  { password :: Maybe (PasswordUpdate v)
  , group :: Maybe Group
  , auth :: Auth v
  , key :: Var BS8.ByteString v
  } deriving (Show, Generic)
instance FunctorB UpdateUser
instance TraversableB UpdateUser
instance ToForm (UpdateUser v) where
  toForm update = fromList
    [ ("oldPassword", toQueryParam $ (.oldPassword) <$> update.password)
    , ("newPassword", toQueryParam $ (.newPassword) <$> update.password)
    , ("group", toQueryParam update.group)
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