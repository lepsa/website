module Website.Data.Schema where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.List
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Website.Types
import Website.Data.Env
import Website.Data.Error (DbErr(NotFound), Err (DbError), throwError_)

-- Create a table for tracking the schema version
createVersion :: Query
createVersion = "create table if not exists schema_version (version integer primary key not null)"

getSchemaVersion :: Query
getSchemaVersion = "select version from schema_version"

setSchema :: Query
setSchema = "update schema_version set version = ?"

getJWK :: Query
getJWK = "select value from jwk"

insertJWK :: Query
insertJWK = "insert into jwk (value) values (?)"

-- This is run on application start to ensure that the schema_version table exists
createSchema :: (CanAppM c e m) => m ()
createSchema = do
  c <- asks conn
  liftIO $ execute_ c createVersion

newtype Version = Version Int
  deriving (Eq, Ord, Show, Read, Num)

instance FromRow Version where
  fromRow = Version <$> field

instance ToRow Version where
  toRow (Version v) = pure $ toField v

setupDatabase :: (CanAppM c e m) => m ()
setupDatabase = do
  c <- asks conn
  -- Explicitly enable foreign keys, as SQLite doesn't turn
  -- them on by default. If the database doesn't support the
  -- features we are expecting, exit the server reporting the
  -- error so that we can try again later with changes made.
  liftIO $ do
    execute_ c "PRAGMA foreign_keys = ON"
    [Only (enabled :: Bool)] <- query_ c "PRAGMA foreign_keys"
    unless enabled $ error "Foreign keys aren't supported in this version of SQLite. Please use a version with foreign key support."

runMigrations :: (CanAppM c e m) => m ()
runMigrations = do
  c <- asks conn
  versions <- liftIO $ withTransaction c $ query_ c getSchemaVersion
  currentVersion <- case versions of
    [] -> pure 0 -- When there is no result from the schema_version table set a minimum value to start the process
    [version] -> pure version
    _ -> throwError_ $ DbError NotFound
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
  [ "insert into schema_version (version) values (0)"
  ]

migrateSchemaV1 :: [Query]
migrateSchemaV1 =
  [ "create table if not exists entry (key integer primary key not null, created datetime not null, title text not null, value text not null)"
  ]

migrateSchemaV2 :: [Query]
migrateSchemaV2 =
  [ "create table if not exists jwk (value text not null)"
  ]

migrateSchemaV3 :: [Query]
migrateSchemaV3 =
  [ "create table if not exists user(id text not null primary key, email text not null unique, group_name text not null)",
    "create table if not exists user_login(id text not null primary key, hash text not null, foreign key (id) references user(id))"
  ]

migrateSchemaV4 :: [Query]
migrateSchemaV4 =
  [ "create table if not exists permission(name text not null primary key, group_name text not null, access text not null)",
    "insert into permission(name, group_name, access) values ('GET new user', 'Admin', 'Write')",
    "insert into permission(name, group_name, access) values ('GET update user', 'Admin', 'Write')",
    "insert into permission(name, group_name, access) values ('GET new entry', 'User', 'Write')",
    "insert into permission(name, group_name, access) values ('GET update entry', 'User', 'Write')",
    "insert into permission(name, group_name, access) values ('POST entry', 'User', 'Write')",
    "insert into permission(name, group_name, access) values ('GET entry', 'User', 'Read')",
    "insert into permission(name, group_name, access) values ('PUT entry', 'User', 'Write')",
    "insert into permission(name, group_name, access) values ('DELETE entry', 'User', 'Write')",
    "insert into permission(name, group_name, access) values ('GET entries', 'User', 'Read')",
    "insert into permission(name, group_name, access) values ('POST user', 'Admin', 'Write')",
    "insert into permission(name, group_name, access) values ('GET user', 'Admin', 'Read')",
    "insert into permission(name, group_name, access) values ('PUT user', 'Admin', 'Write')",
    "insert into permission(name, group_name, access) values ('DELETE user', 'Admin', 'Write')",
    "insert into permission(name, group_name, access) values ('GET users', 'Admin', 'Read')"
  ]

migrateSchemaV5 :: [Query]
migrateSchemaV5 =
  [ "alter table entry add column updated datetime"
  ]

migrations :: [(Version, [Query])]
migrations =
  [ (0, migrateSchemaV0),
    (1, migrateSchemaV1),
    (2, migrateSchemaV2),
    (3, migrateSchemaV3),
    (4, migrateSchemaV4),
    (5, migrateSchemaV5)
  ]