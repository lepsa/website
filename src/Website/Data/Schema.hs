module Website.Data.Schema where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.List (sortBy)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (toField)
import Website.Types

createEntry :: Query
createEntry = "create table if not exists entry (key integer primary key, created datetime not null, title text not null, value text not null)"

createVersion :: Query
createVersion = "create table if not exists schema_version (version integer not null)"

getSchemaVersion :: Query
getSchemaVersion = "select version from schema_version"

setSchema :: Query
setSchema = "update schema_version set version = ?"

createSchema :: (CanAppM Env e m) => m ()
createSchema = do
  c <- asks conn
  liftIO $
    traverse_
      (execute_ c)
      [ createVersion,
        createEntry
      ]

newtype Version = Version Int
  deriving (Eq, Ord, Show, Read, Num)

instance FromRow Version where
  fromRow = Version <$> field

instance ToRow Version where
  toRow (Version v) = pure $ toField v

runMigrations :: (CanAppM Env Err m) => m ()
runMigrations = do
  c <- asks conn
  versions <- liftIO $ withTransaction c $ query_ c getSchemaVersion
  currentVersion <- case versions of
    [] -> pure 0
    [version] -> pure version
    _ -> throwError TooManyResults
  liftIO $ putStrLn $ "Schema: " <> show currentVersion
  let migrationsToRun =
        filter (\(v, _) -> v >= currentVersion) $
          sortBy comp migrations
  -- Iterate over all of the schema bumps that we have
  liftIO $
    traverse_
      -- For each schema bump, run a transaction where
      -- if anything fails, the transaction aborts and
      -- the error propagates up, stopping the rest of
      -- the changes and the server starting.
      (withExclusiveTransaction c . runMigration c currentVersion . snd)
      migrationsToRun
  where
    comp (a, _) (b, _) = compare a b

runMigration :: Connection -> Version -> [Query] -> IO ()
runMigration c currentVersion queries = do
  traverse_ (execute_ c) queries
  execute c setSchema $ currentVersion + 1

migrateSchemaV0 :: [Query]
migrateSchemaV0 = []

migrations :: [(Version, [Query])]
migrations =
  [ (0, migrateSchemaV0)
  ]