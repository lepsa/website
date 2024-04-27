module Website.Auth.Authorisation where

import Data.Aeson
import GHC.Generics
import Database.SQLite.Simple.FromField
import Text.Read (readMaybe)
import Control.Monad
import Database.SQLite.Simple.ToField

--
-- Authorisation based on Unix style permissions.
--

data Group
  = Admin
  | User
  deriving (Eq, Show, Read, Ord, Generic)
instance ToJSON Group
instance FromJSON Group
instance FromField Group where
  fromField = fromField >=> maybe (fail "Could not parse Group") pure . readMaybe
instance ToField Group where
  toField = toField . show

data Permission
  = Read
  | Write
  | Execute
  deriving (Eq, Show, Read, Ord)
instance FromField Permission where
  fromField = fromField >=> maybe (fail "Could not parse Permission") pure . readMaybe
instance ToField Permission where
  toField = toField . show