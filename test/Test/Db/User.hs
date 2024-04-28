{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Db.User where

import Website.Data.User
import Database.SQLite.Simple
import Data.Aeson
import Website.Auth.Authorisation (Group)
import Web.HttpApiData
import Web.FormUrlEncoded
import GHC.Exts (fromList)

-- Orphan instances are here as we don't want nor need them in the main server
instance ToJSON Group
instance FromJSON Group
instance ToHttpApiData Group where
  toQueryParam = toQueryParam . show

instance ToForm CreateUser where
  toForm (CreateUser group email password) = fromList
    [ ("group", toQueryParam group)
    , ("email", toQueryParam email)
    , ("password", toQueryParam password)
    ]

instance ToJSON User
instance FromJSON User

getAllUsers :: Connection -> IO [User]
getAllUsers c = query_ c "select id, email, group_name from user"