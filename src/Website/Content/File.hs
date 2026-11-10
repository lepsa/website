module Website.Content.File where

import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Control.Monad.Logger        (logDebug)
import           Control.Monad.Reader        (asks)
import qualified Data.ByteString.Lazy        as BSL
import           Data.Functor                ((<&>))
import           Data.Int                    (Int64)
import           Data.List                   (sortBy)
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Prelude                     (compare, const, mappend, mconcat,
                                              pure, show, ($), (.), (<$>), (<>),
                                              (>>=))
import           Servant                     (Header, Headers, Proxy (Proxy),
                                              ToHttpApiData (toUrlPiece),
                                              addHeader, safeLink)
import           Servant.Multipart           (FileData (fdFileCType, fdFileName, fdPayload),
                                              MultipartData (files), Tmp)
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html             (Html, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Website.Content.Common      (AuthFile, basicPage, htmlLink,
                                              whenLoggedIn)
import           Website.Content.Htmx        (hxBoost, hxConfirm, hxDelete,
                                              hxOn, hxSwap, hxTarget, hxTrigger)
import           Website.Data.Env            (HasEnv (timeZone))
import           Website.Data.Error          (Err (MissingUpload, TooManyUploads),
                                              throwError_)
import           Website.Data.File           (CreateFile (CreateFile),
                                              File (fileData, fileId, fileType),
                                              FileId,
                                              FileMeta (fileMetaCreated, fileMetaId, fileMetaName),
                                              createFile, deleteFile, getFile,
                                              getFileMeta, getFileMetas)
import           Website.Data.User           (OptionalUser, RequiredUser)
import           Website.Data.Util           (timeFormat)
import           Website.Network.API.CRUD    (CRUDCreateForm, CRUDDelete,
                                              CRUDRead', WithCT (WithCT))
import           Website.Network.API.Types   (topAPI)
import           Website.Types               (CanAppM)

uploadFile :: forall c e m. (RequiredUser c, CanAppM c e m) => MultipartData Tmp -> m (Headers '[Header "Location" Text] Html)
uploadFile formData = do
  $(logDebug) "uploadFile"
  let fds = files formData
  fd <- case fds of
    []   -> throwError_ MissingUpload
    [fd] -> pure fd
    _    -> throwError_ TooManyUploads
  f <- mkFile fd >>= createFile
  pure $
    addHeader (pack "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthFile (CRUDRead' FileId))) f.fileId))
    "Uploaded"
  where
    mkFile :: FileData Tmp -> m CreateFile
    mkFile fd = do
      data_ <- liftIO $ BSL.readFile $ fdPayload fd
      pure $ CreateFile
        (fdFileName fd)
        data_
        (fdFileCType fd)

getFile :: (OptionalUser c, CanAppM c e m) => FileId -> m (Headers '[Header "Content-Length" Int64] WithCT)
getFile fId = do
  $(logDebug) $ "getFile " <> T.pack (show fId)
  file <- Website.Data.File.getFile fId
  let fileLen = BSL.length file.fileData
  pure $ addHeader fileLen $ WithCT
    (BSL.fromStrict $ encodeUtf8 file.fileType)
    file.fileData

getFiles :: (CanAppM c e m, OptionalUser c) => m Html
getFiles = do
  $(logDebug) "getFiles"
  tz <- asks timeZone
  files <- getFileMetas
  mNewFile <- whenLoggedIn $ const newFile
  mDeleteFile <- whenLoggedIn $ const delFile
  basicPage $ mconcat $ catMaybes
    [ pure $ H.h2 "Files",
      mNewFile,
      pure $ H.ul $ mconcat $
        sortBy sortFileMeta files <&> \fm ->
          H.li $ H.p (mconcat $ catMaybes
            [ pure $ fileLink tz fm,
              pure H.br,
              ($ fm) <$> mDeleteFile
            ]
          )
    ]
  where
    newFile = H.a
      ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! htmlLink (Proxy @(AuthFile CRUDCreateForm)) $ "Create File"
    delFile m = H.button
      ! hxTrigger "click"
      ! hxSwap "outerHTML swap"
      ! hxTarget "closest li"
      ! hxConfirm (H.textValue $ "Confirm deletion: " <> m.fileMetaName)
      ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! hxDelete (H.textValue $ mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthFile (CRUDDelete FileId))) m.fileMetaId)
      $ "Delete"
    fileLink tz m = mconcat
      [ H.a
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! HA.href (H.textValue $ pack "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthFile (CRUDRead' FileId))) m.fileMetaId))
      ! HA.download (H.textValue m.fileMetaName)
      $ H.toHtml m.fileMetaName,
      H.toHtml $ " " <> timeFormat tz m.fileMetaCreated
      ]
    sortFileMeta a b = a.fileMetaName `compare` b.fileMetaName

deleteFile :: (RequiredUser c, CanAppM c e m) => FileId -> m Html
deleteFile fId = do
  $(logDebug) $ "deleteFile " <> T.pack (show fId)
  m <- getFileMeta fId
  Website.Data.File.deleteFile fId
  pure $ H.toHtml $ "Deleted " <> m.fileMetaName
