module Website.Data.Entry where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import GHC.Generics
import Servant
import Web.FormUrlEncoded
import Website.Types
import Website.Data.Env
import Website.Data.Error

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
instance FromForm EntryCreate where
  fromForm f = EntryCreate
    <$> parseUnique "title" f
    <*> parseUnique "value" f

data EntryUpdate = EntryUpdate
  { title :: String,
    value :: String
  }
  deriving (Eq, Ord, Show, Generic)

instance FromForm EntryUpdate where
  fromForm f = EntryUpdate
    <$> parseUnique "title" f
    <*> parseUnique "value" f

data Entry = Entry
  { key :: EntryKey,
    created :: UTCTime,
    title :: String,
    value :: String
  }
  deriving (Eq, Ord, Show, Generic)

entryTimeFormat :: TimeZone -> UTCTime -> String
entryTimeFormat tz = formatTime defaultTimeLocale "%e %B, %Y" . utcToZonedTime tz

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

createEntry :: (CanAppM c e m) => EntryCreate -> m Entry
createEntry (EntryCreate title value) = do
  c <- asks conn
  entries <- liftIO $ withTransaction c $ query c "insert into entry(created, title, value) values (datetime(), ?, ?) returning key, created, title, value" (title, value)
  case entries of
    [] -> throwError $ err $ DbError NotFound
    [entry] -> pure entry
    _ -> throwError $ err $ DbError TooManyResults

getEntry :: (CanAppM c e m) => EntryKey -> m Entry
getEntry key = do
  c <- asks conn
  entries <- liftIO $ withTransaction c $ query c "select key, created, title, value from entry where key = ?" (Only key)
  case entries of
    [] -> throwError $ err $ DbError NotFound
    [entry] -> pure entry
    _ -> throwError $ err $ DbError TooManyResults

updateEntry :: (CanAppM c e m) => EntryKey -> EntryUpdate -> m Entry
updateEntry key (EntryUpdate title value) = do
  c <- asks conn
  entries <- liftIO $ withTransaction c $ query c "update entry set title = ?, value = ? where key = ? returning key, created, title, value" (title, value, key)
  case entries of
    [] -> throwError $ err $ DbError NotFound
    [entry] -> pure entry
    _ -> throwError $ err $ DbError TooManyResults

deleteEntry :: (CanAppM c e m) => EntryKey -> m ()
deleteEntry key = do
  c <- asks conn
  liftIO $ withTransaction c $ execute c "delete from entry where key = ?" (Only key)

getEntries :: (CanAppM c e m) => m [Entry]
getEntries = do
  c <- asks conn
  liftIO $ withTransaction c $ query_ c "select key, created, title, value from entry"