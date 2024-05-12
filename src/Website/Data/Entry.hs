module Website.Data.Entry where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List                        (sortBy)
import           Data.Text
import           Data.Time
import           Data.UUID                        (UUID)
import           Data.UUID.V4                     (nextRandom)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           GHC.Generics
import           Servant
import           Web.FormUrlEncoded
import           Website.Data.Env
import           Website.Data.Error
import           Website.Data.Util                ()
import           Website.Types

--
-- What an Entry is, and the various derived types that
-- are needed to help run the API and DB in typesafe ways
--

newtype EntryKey = EntryKey
  { key :: UUID
  }
  deriving (Eq, Ord, Show, Generic, FromField, ToField, ToHttpApiData, FromHttpApiData)

instance FromRow EntryKey

data EntryCreate = EntryCreate
  { title :: Text,
    value :: Text
  }
  deriving (Eq, Ord, Show, Generic)
instance FromForm EntryCreate where
  fromForm f = EntryCreate
    <$> parseUnique "title" f
    <*> parseUnique "value" f

data EntryUpdate = EntryUpdate
  { title :: Text,
    value :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromForm EntryUpdate where
  fromForm f = EntryUpdate
    <$> parseUnique "title" f
    <*> parseUnique "value" f

data Entry = Entry
  { key     :: EntryKey,
    created :: UTCTime,
    title   :: Text,
    value   :: Text,
    updated :: Maybe UTCTime
  }
  deriving (Eq, Ord, Show, Generic)

--
-- SQL for managing entries in the DB
--

instance FromRow Entry where
  fromRow = Entry
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field

createEntry :: (CanAppM c e m) => EntryCreate -> m Entry
createEntry (EntryCreate title value) = do
  c <- asks conn
  uuid <- liftIO nextRandom
  entries <- liftIO $ withTransaction c $ query c "insert into entry(key, created, title, value, updated) values (?, datetime(), ?, ?, null) returning key, created, title, value, updated" (uuid, title, value)
  case entries of
    []      -> throwError $ fromErr $ DbError NotFound
    [entry] -> pure entry
    _       -> throwError $ fromErr $ DbError TooManyResults

getEntry :: (CanAppM c e m) => EntryKey -> m Entry
getEntry key = do
  c <- asks conn
  entries <- liftIO $ withTransaction c $ query c "select key, created, title, value, updated from entry where key = ?" (Only key)
  case entries of
    []      -> throwError $ fromErr $ DbError NotFound
    [entry] -> pure entry
    _       -> throwError $ fromErr $ DbError TooManyResults

updateEntry :: (CanAppM c e m) => EntryKey -> EntryUpdate -> m Entry
updateEntry key (EntryUpdate title value) = do
  c <- asks conn
  entries <- liftIO $ withTransaction c $ query c "update entry set title = ?, value = ?, updated = datetime() where key = ? returning key, created, title, value, updated" (title, value, key)
  case entries of
    []      -> throwError $ fromErr $ DbError NotFound
    [entry] -> pure entry
    _       -> throwError $ fromErr $ DbError TooManyResults

deleteEntry :: (CanAppM c e m) => EntryKey -> m ()
deleteEntry key = do
  c <- asks conn
  liftIO $ withTransaction c $ execute c "delete from entry where key = ?" (Only key)

getEntries :: (CanAppM c e m) => m [Entry]
getEntries = do
  c <- asks conn
  liftIO $ withTransaction c $ query_ c "select key, created, title, value, updated from entry"

getRecentEntries :: CanAppM c e m => Int -> m [Entry]
getRecentEntries num = do
  c <- asks conn
  liftIO $ withTransaction c $ query c "select key, created, title, value, updated from entry order by created desc limit ?" (Only num)

sortEntriesByDateAsc :: [Entry] -> [Entry]
sortEntriesByDateAsc = sortBy $ \a b ->
  compare a.created b.created

sortEntriesByDateDesc :: [Entry] -> [Entry]
sortEntriesByDateDesc = sortBy $ \a b ->
  compare b.created a.created
