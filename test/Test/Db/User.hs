{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Db.User where

import           Data.Aeson                 (FromJSON, ToJSON, parseJSON,
                                             toJSON)
import           Database.SQLite.Simple     (Connection, query_)
import           GHC.Exts                   (fromList)
import           Prelude                    (IO, fmap, show, (.))
import           Web.FormUrlEncoded         (ToForm, toForm)
import           Web.HttpApiData            (ToHttpApiData (toQueryParam))
import           Website.Auth.Authorisation (Group)
import           Website.Data.User          (User, UserCreate (UserCreate),
                                             UserKey (UserKey, unUserKey))

-- Orphan instances are here as we don't want nor need them in the main server
instance ToJSON Group
instance FromJSON Group
instance ToHttpApiData Group where
  toQueryParam = toQueryParam . show

instance ToJSON UserKey where
  toJSON = toJSON . unUserKey

instance FromJSON UserKey where
  parseJSON = fmap UserKey . parseJSON

instance ToForm UserCreate where
  toForm (UserCreate group email password) = fromList
    [ ("group", toQueryParam group)
    , ("email", toQueryParam email)
    , ("password", toQueryParam password)
    ]

instance ToJSON User
instance FromJSON User

getAllUsers :: Connection -> IO [User]
getAllUsers c = query_ c "select id, email, group_name from user"
