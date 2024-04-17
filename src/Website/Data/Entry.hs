module Website.Data.Entry where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Website.Data.Common
import Website.Network.API.CRUD
import Website.Network.API.Types
import Website.Types
import GHC.Generics
import Servant
import Web.FormUrlEncoded

--
-- What an Entry is, and the various derived types that
-- are needed to help run the API and DB in typesafe ways
--

newtype EntryKey = EntryKey
  { key :: Int
  }
  deriving (Eq, Ord, Show, Generic, FromField, ToField, ToHttpApiData, FromHttpApiData)

instance FromRow EntryKey

data EntryCreate = EntryCreate
  { title :: String,
    value :: String
  }
  deriving (Eq, Ord, Show, Generic)

instance FromForm EntryCreate

data EntryUpdate = EntryUpdate
  { title :: String,
    value :: String
  }
  deriving (Eq, Ord, Show, Generic)

instance FromForm EntryUpdate

data Entry = Entry
  { key :: EntryKey,
    created :: UTCTime,
    title :: String,
    value :: String
  }
  deriving (Eq, Ord, Show)

--
-- What an Entry should look like in HTML
--
instance GenerateForm Entry where
  newForm _ =
    FormData
      { title = "Create Entry",
        createUrl = pure $ unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @("entry" :> CRUDCreate EntryCreate))),
        updateUrl = Nothing,
        fields =
          [ FieldData
              { label = "Title",
                name = "title",
                type_ = "text",
                value = Nothing
              },
            FieldData
              { label = "Value",
                name = "value",
                type_ = "text",
                value = Nothing
              }
          ]
      }
  updateForm entry =
    FormData
      { title = "Update Entry",
        createUrl = Nothing,
        updateUrl = pure $ unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @("entry" :> CRUDUpdate EntryUpdate EntryKey)) entry.key),
        fields =
          [ FieldData
              { label = "Title",
                name = "title",
                type_ = "text",
                value = pure entry.title
              },
            FieldData
              { label = "Value",
                name = "value",
                type_ = "text",
                value = pure entry.value
              }
          ]
      }

--
-- SQL for managing entries in the DB
--

instance FromRow Entry where
  fromRow =
    Entry
      <$> field
      <*> field
      <*> field
      <*> field

createEntry :: (CanAppM Env Err m) => EntryCreate -> m Entry
createEntry (EntryCreate title value) = do
  c <- asks conn
  entries <- liftIO $ withTransaction c $ query c "insert into entry(created, title, value) values (datetime(), ?, ?) returning *" (title, value)
  case entries of
    [] -> throwError NotFound
    [entry] -> pure entry
    _ -> throwError TooManyResults

getEntry :: (CanAppM Env Err m) => EntryKey -> m Entry
getEntry key = do
  c <- asks conn
  entries <- liftIO $ withTransaction c $ query c "select * from entry where key = ?" (Only key)
  case entries of
    [] -> throwError NotFound
    [entry] -> pure entry
    _ -> throwError TooManyResults

updateEntry :: (CanAppM Env Err m) => EntryKey -> EntryUpdate -> m Entry
updateEntry key (EntryUpdate title value) = do
  c <- asks conn
  entries <- liftIO $ withTransaction c $ query c "update entry set title = ?, value = ? where key = ? returning *" (title, value, key)
  case entries of
    [] -> throwError NotFound
    [entry] -> pure entry
    _ -> throwError TooManyResults

deleteEntry :: (CanAppM Env e m) => EntryKey -> m ()
deleteEntry key = do
  c <- asks conn
  liftIO $ withTransaction c $ execute c "delete from entry where key = ?" (Only key)

getEntries :: (CanAppM Env e m) => m [Entry]
getEntries = do
  c <- asks conn
  liftIO $ withTransaction c $ query c "select * from entry" ()