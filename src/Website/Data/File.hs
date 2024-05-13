module Website.Data.File where
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy             as BSL
import           Data.Text
import           Data.Time
import           Data.UUID
import           Data.UUID.V4                     (nextRandom)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField   (ToField)
import           GHC.Generics
import           Servant
import           Website.Data.Env
import           Website.Data.Util
import           Website.Types                    (CanAppM)
import Control.Monad.Logger
import qualified Data.Text as T

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
  $(logDebug) $ "getFile " <> T.pack (show fId)
  c <- asks conn
  l <- liftIO $ query c "select id, name, data, type, created, updated from file where id = ?" (Only fId)
  ensureSingleResult l

deleteFile :: CanAppM c e m => FileId -> m ()
deleteFile fId = do
  $(logDebug) $ "deleteFile " <> T.pack (show fId)
  c <- asks conn
  liftIO $ execute c "delete from file where id = ?" (Only fId)

updateFile :: CanAppM c e m => UpdateFile -> m ()
updateFile fUpdate = do
  $(logDebug) "updateFile"
  c <- asks conn
  liftIO $ execute c "update file set name = ?, data = ?, type = ?, updated = datetime() where id = ?" (fUpdate.updateFileName, fUpdate.updateFileData, fUpdate.updateFileType, fUpdate.updateFileId)

getFileMeta :: CanAppM c e m => FileId -> m FileMeta
getFileMeta fId = do
  $(logDebug) $ "getFileMeta " <> T.pack (show fId)
  c <- asks conn
  l <- liftIO $ query c "select id, name, created, updated from file where id = ?" (Only fId)
  ensureSingleResult l

getFileMetas :: CanAppM c e m => m [FileMeta]
getFileMetas = do
  $(logDebug) "getFileMetas"
  c <- asks conn
  liftIO $ query_ c "select id, name, created, updated from file"
