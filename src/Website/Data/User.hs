module Website.Data.User where

import Data.UUID
import Website.Auth.Authorisation ( Group(Admin) )
import Data.Text
import GHC.Generics
import Data.Aeson
import Servant.Auth.JWT
import Servant.Auth.Server
import Data.UUID.V4 (nextRandom)
import Website.Auth.Authentication (checkUserPass)

data User = User
  { userId :: UUID
  , userGroup :: Group
  , userEmail :: Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

instance FromBasicAuthData User where
  fromBasicAuthData (BasicAuthData user pass) _cfg = do
    ok <- checkUserPass user pass
    if ok
    then do
      uuid <- nextRandom
      pure $ Authenticated (User uuid Admin "")
    else pure BadPassword