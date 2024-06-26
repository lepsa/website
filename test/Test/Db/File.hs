{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Db.File where

import           Data.Aeson
import           Data.Base64.Types
import qualified Data.ByteString.Base64.URL      as B64U
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.ByteString.Lazy.Base64.URL as BL64U
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Database.SQLite.Simple
import           Website.Data.File

instance ToJSON FileId
instance FromJSON FileId
-- instance ToJSON FileId where
--   toJSON (FileId u) = toJSON u
-- instance FromJSON FileId where
--   parseJSON v = FileId <$> parseJSON v

instance ToJSON BSL.ByteString where
  toJSON = toJSON . extractBase64 . BL64U.encodeBase64
instance FromJSON BSL.ByteString where
  parseJSON = withText "Base64Url" $ \t ->
    either (fail . T.unpack) (pure . BSL.fromStrict) . B64U.decodeBase64Untyped $ T.encodeUtf8 t

instance ToJSON File
instance FromJSON File
-- instance ToJSON File where
--   toJSON f = object
--     [ "id" .= fileId f
--     , "name" .= fileName f
--     , "data" .= fileData f
--     , "type" .= fileType f
--     , "created" .= fileCreated f
--     , "updated" .= fileUpdated f
--     ]
-- instance FromJSON File where
--   parseJSON = withObject "File" $ \o -> File
--     <$> o .: "id"
--     <*> o .: "name"
--     <*> o .: "data"
--     <*> o .: "type"
--     <*> o .: "created"
--     <*> o .: "updated"

getAllFiles :: Connection -> IO [File]
getAllFiles c = query_ c "select id, name, data, type, created, updated from file"
