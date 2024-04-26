module Website.Auth.Authorisation where
import Data.Aeson
import GHC.Generics

--
-- Authorisation based on Unix style permissions.
--

data Group
  = Admin
  | User
  deriving (Eq, Show, Read, Ord, Generic)
instance ToJSON Group
instance FromJSON Group

data Permission
  = Read
  | Write
  | Execute
  deriving (Eq, Show, Read, Ord)