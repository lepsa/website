module Website.Auth.Authentication where
import Data.ByteString
import GHC.Generics
import Web.FormUrlEncoded

data Login = Login
  { user :: String
  , pass :: String
  } deriving Generic
instance FromForm Login

checkUserPass :: ByteString -> ByteString -> IO Bool
checkUserPass user pass = do
  if user == "user" && pass == "pass"
  then pure True
  else pure False