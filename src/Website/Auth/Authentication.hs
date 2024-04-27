module Website.Auth.Authentication where
import GHC.Generics
import Web.FormUrlEncoded

data Login = Login
  { email :: String
  , password :: String
  } deriving Generic
instance FromForm Login