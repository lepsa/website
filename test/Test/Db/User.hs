{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Db.User where

import           Data.Aeson
import           Database.SQLite.Simple
import           GHC.Exts                   (fromList)
import           Web.FormUrlEncoded
import           Web.HttpApiData
import           Website.Auth.Authorisation (Group)
import           Website.Data.User

-- Orphan instances are here as we don't want nor need them in the main server
instance ToJSON Group
instance FromJSON Group
instance ToHttpApiData Group where
  toQueryParam = toQueryParam . show

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
