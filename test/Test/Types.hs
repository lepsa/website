{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Test.Types where

import Data.Text
import GHC.Generics
import Hedgehog hiding (Group)
import Network.HTTP.Client qualified as H
import Website.Auth.Authentication ()
import Test.Db.Entry ()
import Test.Db.User ()
import Website.Auth.Authorisation qualified as Auth
import Website.Data.User (UserCreate(UserCreate))
import Web.FormUrlEncoded
import Servant
import GHC.Exts
import Website.Auth.Authorisation (Group)
import Data.Map (Map)
import Data.Map qualified as M
import Control.Lens
import Data.Kind

-- What we think that the state of the world should look like.
-- This will often end up mirroring the database in some way, as
-- we want to track the same information, to ensure that the server
-- is doing what we expect.
type Key = Var String

class HasEmail t where
  email :: Lens' t Text
class HasPassword t where
  password :: Lens' t Text
class HasKey (t :: (Type -> Type) -> Type) where
  key :: Lens' (t v) (Key v)
class HasAuth (t :: (Type -> Type) -> Type) where
  auth :: Lens' (t v) (Auth v)

data TestEntry v = TestEntry
  { -- created :: UTCTime,
    _teTitle   :: Text,
    _teValue   :: Text
  }
  deriving (Generic)
instance FunctorB TestEntry
instance TraversableB TestEntry

data TestUser v = TestUser
  { _tuEmail    :: Text
  , _tuPassword :: Text
  , _tuGroup    :: Auth.Group
  , _tuJwt      :: Maybe (Var String v)
  }
  deriving (Eq, Generic, Show)
instance FunctorB TestUser
instance TraversableB TestUser

data ApiState v = ApiState
  { _users   :: Map (Key v) (TestUser v),
    _entries :: Map (Key v) (TestEntry v)
  } deriving (Generic)

initialState :: ApiState v
initialState = ApiState M.empty M.empty

data TestEnv = TestEnv
  { manager :: H.Manager,
    baseUrl :: String
  }

--
-- Main api commands
--

data AuthKey v = AuthKey
  { _akKey :: Key v
  , _akUser :: TestUser v
  } deriving (Eq, Generic, Show)
instance FunctorB AuthKey
instance TraversableB AuthKey

data Auth v
  = Normal (AuthKey v)
  | Bad (Maybe (AuthKey v))
  deriving (Eq, Generic, Show)
instance FunctorB Auth
instance TraversableB Auth

-- Log into the API with the given email and password
data LoginType = Good | BadUser | BadPassword
  deriving (Eq, Ord, Show)

data TestLogin v = TestLogin
  { _tlKey  :: Key v 
  , _tlType :: LoginType
  , _tlUser :: Text
  , _tlPass :: Text
  } deriving (Eq, Show, Generic)
instance FunctorB TestLogin
instance TraversableB TestLogin

instance ToForm (TestLogin v) where
  toForm u = fromList
    [ ("login", toQueryParam $ _tlUser u)
    , ("password", toQueryParam $ _tlPass u)
    ]

toUserCreate :: TestUser v -> UserCreate
toUserCreate u = UserCreate
  (_tuGroup u)
  (_tuEmail u)
  (_tuPassword u)

-- Get the entries from the API
newtype GetEntries v = GetEntries
  { _getEntriesAuth :: Auth v
  } deriving (Show, Generic)
instance FunctorB GetEntries
instance TraversableB GetEntries

data GetEntry v = GetEntry
  { _geKey :: Key v
  , _geAuth :: Auth v
  } deriving (Show, Generic)
instance FunctorB GetEntry
instance TraversableB GetEntry

data GetEntryMissing v = GetEntryMissing
  { _gemKey :: String
  , _gemAuth :: Auth v
  } deriving (Show, Generic)
instance FunctorB GetEntryMissing
instance TraversableB GetEntryMissing

data CreateEntry v = CreateEntry
  { _ceAuth :: Auth v
  , _ceTitle :: Text
  , _ceValue :: Text
  } deriving (Eq, Show, Generic)
instance FunctorB CreateEntry
instance TraversableB CreateEntry

instance ToForm (CreateEntry v) where
  toForm (CreateEntry _ title value) = fromList
    [ ("title", toQueryParam title)
    , ("value", toQueryParam value)
    ]

data UpdateEntry v = UpdateEntry
  { _ueAuth :: Auth v
  , _ueKey :: Key v
  , _ueTitle :: Text
  , _ueValue :: Text
  } deriving (Show, Generic)
instance FunctorB UpdateEntry
instance TraversableB UpdateEntry

data RegisterUser v = RegisterUser
  { _ruEmail :: Text
  , _ruPassword :: Text
  , _ruGroup :: Group
  } deriving (Show, Generic)
instance FunctorB RegisterUser
instance TraversableB RegisterUser

data CreateUser v = CreateUser
  { _cuAuth :: Auth v
  , _cuGroup :: Group
  , _cuEmail :: Text
  , _cuPassword :: Text
  } deriving (Show, Generic)
instance FunctorB CreateUser
instance TraversableB CreateUser

data DeleteUser v = DeleteUser
  { _duAuth :: Auth v
  , _duKey :: Key v
  } deriving (Show, Generic)
instance FunctorB DeleteUser
instance TraversableB DeleteUser

data GetUser v = GetUser
  { _guAuth :: Auth v
  , _guKey :: Key v
  } deriving (Show, Generic)
instance FunctorB GetUser
instance TraversableB GetUser

data PasswordUpdate v = PasswordUpdate
  { _oldPassword :: Text
  , _newPassword :: Text
  } deriving (Show, Generic)
instance FunctorB PasswordUpdate
instance TraversableB PasswordUpdate

data UpdateUser v = UpdateUser
  { _uuKey :: Key v
  , _uuPassword :: Maybe (PasswordUpdate v)
  , _uuGroup :: Maybe Group
  , _uuAuth :: Auth v
  } deriving (Show, Generic)
instance FunctorB UpdateUser
instance TraversableB UpdateUser

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

makeLenses ''TestEnv
makeLenses ''TestEntry
makeLenses ''TestUser
makeLenses ''ApiState
makeLenses ''AuthKey
makePrisms ''Auth
makeLenses ''LoginType
makeLenses ''TestLogin
makeWrapped ''GetEntries
makeLenses ''GetEntry
makeLenses ''GetEntryMissing
makeLenses ''CreateEntry
makeLenses ''UpdateEntry
makeLenses ''RegisterUser
makeLenses ''CreateUser
makeLenses ''DeleteUser
makeLenses ''GetUser
makeLenses ''PasswordUpdate
makeLenses ''UpdateUser

instance HasEmail (TestUser v) where
  email = tuEmail
instance HasPassword (TestUser v) where
  password = tuPassword

instance HasKey AuthKey where
  key = akKey

instance HasKey TestLogin where
  key = tlKey
instance HasEmail (TestLogin v) where
  email = tlUser
instance HasPassword (TestLogin v) where
  password = tlPass

instance HasAuth GetEntries where
  auth = _Wrapped

instance HasAuth GetEntry where
  auth = geAuth

instance HasAuth GetEntryMissing where
  auth = gemAuth

instance HasAuth CreateEntry where
  auth = ceAuth

instance HasAuth UpdateEntry where
  auth = ueAuth

instance HasEmail (RegisterUser v) where
  email = ruEmail
instance HasPassword (RegisterUser v) where
  password = ruPassword

instance HasEmail (CreateUser v) where
  email = cuEmail

instance HasPassword (CreateUser v) where
  password = cuPassword

instance HasAuth UpdateUser where
  auth = uuAuth

instance HasKey GetEntry where
  key = geKey

instance HasKey UpdateEntry where
  key = ueKey

instance HasAuth CreateUser where
  auth = cuAuth

instance HasKey UpdateUser where
  key = uuKey

instance HasAuth DeleteUser where
  auth = duAuth

instance HasKey DeleteUser where
  key = duKey

instance HasAuth GetUser where
  auth = guAuth

instance HasKey GetUser where
  key = guKey

instance ToForm (UpdateEntry v) where
  toForm u = fromList
    [ ("title", toQueryParam $ u ^. ueTitle)
    , ("value", toQueryParam $ u ^. ueValue)
    ]

instance ToForm (RegisterUser v) where
  toForm u = fromList
    [ ("email", toQueryParam $ u ^. ruEmail)
    , ("password", toQueryParam $ u ^. ruPassword)
    , ("group", toQueryParam $ u ^. ruGroup)
    ]

instance ToForm (CreateUser v) where
  toForm u = fromList
    [ ("group", toQueryParam $ u ^. cuGroup)
    , ("email", toQueryParam $ u ^. cuEmail)
    , ("password", toQueryParam $ u ^. cuPassword)
    ]

instance ToForm (UpdateUser v) where
  toForm update = fromList
    [ ("oldPassword", toQueryParam $ update ^? uuPassword . _Just . oldPassword)
    , ("newPassword", toQueryParam $ update ^? uuPassword . _Just . newPassword)
    , ("group", toQueryParam $ update ^. uuGroup)
    ]
