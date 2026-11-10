module Website.Data.File where
import           Control.Monad.Logger             (logDebug)
import           Control.Monad.Reader             (MonadIO (liftIO), asks)
import qualified Data.ByteString.Lazy             as BSL
import           Data.Text                        (Text, show)
import           Data.Time                        (UTCTime)
import           Data.UUID                        (UUID)
import           Data.UUID.V4                     (nextRandom)
import           Database.SQLite.Simple           (FromRow, Only (Only),
                                                   execute, field, fromRow,
                                                   query, query_,
                                                   withTransaction)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField   (ToField)
import           GHC.Generics                     (Generic)
import           Prelude                          (Eq, Maybe, Ord, Show, ($),
                                                   (<$>), (<*>), (<>))
import           Servant                          (FromHttpApiData,
                                                   ToHttpApiData)
import           Website.Data.Env                 (HasEnv (conn))
import           Website.Data.Util                (ensureSingleResult)
import           Website.Types                    (CanAppM)

newtype FileId = FileId UUID
  deriving (Eq, Show, Ord, Generic, ToField, FromField, ToHttpApiData, FromHttpApiData)

data File = File
  { fileId      :: FileId
  , fileName    :: Text
  , fileData    :: BSL.ByteString
  , fileType    :: Text
  , fileCreated :: UTCTime
  , fileUpdated :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

instance FromRow File where
  fromRow = File <$> field <*> field <*> field <*> field <*> field <*> field

data FileMeta = FileMeta
  { fileMetaId      :: FileId
  , fileMetaName    :: Text
  , fileMetaCreated :: UTCTime
  , fileMetaUpdated :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

instance FromRow FileMeta where
  fromRow = FileMeta <$> field <*> field <*> field <*> field

data CreateFile = CreateFile
  { createFileName :: Text
  , createFileData :: BSL.ByteString
  , createFileType :: Text
  } deriving (Eq, Show, Generic)

data UpdateFile = UpdateFile
  { updateFileId   :: FileId
  , updateFileName :: Text
  , updateFileData :: BSL.ByteString
  , updateFileType :: Text
  } deriving (Eq, Show, Generic)

createFile :: CanAppM c e m => CreateFile -> m File
createFile cFile = do
  $(logDebug) "createFile"
  c <- asks conn
  uuid <- liftIO nextRandom
  l <- liftIO $ withTransaction c $ query c "insert into file (id, name, data, type, created, updated) values (?, ?, ?, ?, datetime(), null) returning id, name, data, type, created, updated" (uuid, cFile.createFileName, cFile.createFileData, cFile.createFileType)
  ensureSingleResult l

getFile :: CanAppM c e m => FileId -> m File
getFile fId = do
  $(logDebug) $ "getFile " <> show fId
  c <- asks conn
  l <- liftIO $ query c "select id, name, data, type, created, updated from file where id = ?" (Only fId)
  ensureSingleResult l

deleteFile :: CanAppM c e m => FileId -> m ()
deleteFile fId = do
  $(logDebug) $ "deleteFile " <> show fId
  c <- asks conn
  liftIO $ execute c "delete from file where id = ?" (Only fId)

updateFile :: CanAppM c e m => UpdateFile -> m ()
updateFile fUpdate = do
  $(logDebug) "updateFile"
  c <- asks conn
  liftIO $ execute c "update file set name = ?, data = ?, type = ?, updated = datetime() where id = ?" (fUpdate.updateFileName, fUpdate.updateFileData, fUpdate.updateFileType, fUpdate.updateFileId)

getFileMeta :: CanAppM c e m => FileId -> m FileMeta
getFileMeta fId = do
  $(logDebug) $ "getFileMeta " <> show fId
  c <- asks conn
  l <- liftIO $ query c "select id, name, created, updated from file where id = ?" (Only fId)
  ensureSingleResult l

getFileMetas :: CanAppM c e m => m [FileMeta]
getFileMetas = do
  $(logDebug) "getFileMetas"
  c <- asks conn
  liftIO $ query_ c "select id, name, created, updated from file"
