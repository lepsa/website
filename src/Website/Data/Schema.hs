module Website.Data.Schema where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.List
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Website.Types
import Servant

-- Create the Entry table
createEntry :: Query
createEntry = "create table if not exists entry (key integer primary key, created datetime not null, title text not null, value text not null)"

-- Create a table for tracking the schema version
createVersion :: Query
createVersion = "create table if not exists schema_version (version integer not null)"

-- Set an initial value for schema versions, as otherwise the table will be empty and break
-- the other schema version bumping
initialVersion :: Query
initialVersion = "insert into schema_version (version) values (0)"

getSchemaVersion :: Query
getSchemaVersion = "select version from schema_version"

setSchema :: Query
setSchema = "update schema_version set version = ?"

getJWK :: Query
getJWK = "select value from jwk"

insertJWK :: Query
insertJWK = "insert into jwk (value) values (?)"

-- This is run on application start to ensure that the schema_version table exists
createSchema :: (CanAppM Env e m) => m ()
createSchema = do
  c <- asks conn
  liftIO $ execute_ c createVersion

newtype Version = Version Int
  deriving (Eq, Ord, Show, Read, Num)

instance FromRow Version where
  fromRow = Version <$> field

instance ToRow Version where
  toRow (Version v) = pure $ toField v

runMigrations :: (CanAppM Env ServerError m) => m ()
runMigrations = do
  c <- asks conn
  versions <- liftIO $ withTransaction c $ query_ c getSchemaVersion
  currentVersion <- case versions of
    [] -> pure 0 -- When there is no result from the schema_version table set a minimum value to start the process
    [version] -> pure version
    _ -> throwError err404
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
      (withExclusiveTransaction c . runMigration c)
      migrationsToRun
  where
    comp (a, _) (b, _) = compare a b

runMigration :: Connection -> (Version, [Query]) -> IO ()
runMigration c (version, queries) = do
  -- Run the migration queries
  traverse_ (execute_ c) queries
  -- Bump the schema version for the current migration.
  execute c setSchema $ version + 1
  putStrLn $ "Ran migration for schema: " <> show version

-- Set the initial version of the schema to "0" so that other
-- schema bumps have something to work against. When this migration
-- is run, it will be immediately followed by a schema version bump
-- so the rest of the migration code can run properly.
migrateSchemaV0 :: [Query]
migrateSchemaV0 =
  [ initialVersion
  ]

migrateSchemaV1 :: [Query]
migrateSchemaV1 =
  [ createEntry
  ]

migrateSchemaV2 :: [Query]
migrateSchemaV2 =
  [ "create table if not exists jwk (value text not null)"
  ]

migrations :: [(Version, [Query])]
migrations =
  [ (0, migrateSchemaV0),
    (1, migrateSchemaV1),
    (2, migrateSchemaV2)
  ]