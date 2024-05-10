module Website.Content.File where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy        as BSL
import           Data.Functor
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Text
import           Data.Text.Encoding
import           Servant
import           Servant.Multipart
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html             (Html, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Website.Content.Common
import           Website.Content.Htmx
import           Website.Data.Env
import           Website.Data.Error
import           Website.Data.File
import           Website.Data.User
import           Website.Data.Util
import           Website.Network.API.CRUD
import           Website.Network.API.Types
import           Website.Types

uploadFile :: forall c e m. (RequiredUser c, CanAppM c e m) => MultipartData Tmp -> m (Headers '[Header "Location" Text] Html)
uploadFile formData = do
  files <- traverse (createFile <=< mkFile) $ files formData
  file <- case files of
    []    -> throwError $ fromErr $ Other "No files uploaded"
    (f:_) -> pure f
  let link = mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthFile (CRUDRead' FileId))) file.fileId
  pure $ addHeader link mempty
  where
    mkFile :: FileData Tmp -> m CreateFile
    mkFile fd = do
      data_ <- liftIO $ BSL.readFile $ fdPayload fd
      pure $ CreateFile
        (fdFileName fd)
        data_
        (fdFileCType fd)

uploadFileForm :: (RequiredUser c, CanAppM c e m) => m Html
uploadFileForm = do
  basicPage $ mconcat
    [ H.form
      -- ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! HA.enctype "multipart/form-data"
      ! HA.method "POST"
      ! HA.action (H.textValue $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthFile (CRUDCreate' FileUpload)))))
      $ mconcat
      [ H.label
        ! HA.for "file"
        $ "Choose a file for upload"
      , H.br
      , H.input
        ! HA.type_ "file"
        ! HA.name "file"
      , H.br
      , H.button
        ! HA.type_ "submit"
        $ "Upload"
      ]
    ]

getFile :: (OptionalUser c, CanAppM c e m) => FileId -> m (Headers '[Header "Content-Length" Int64] WithCT)
getFile fId = do
  file <- Website.Data.File.getFile fId
  let fileLen = BSL.length file.fileData
  pure
    $ addHeader fileLen
    $ WithCT
      (BSL.fromStrict $ encodeUtf8 file.fileType)
      file.fileData

getFiles :: (OptionalUser c, CanAppM c e m) => m Html
getFiles = do
  tz <- asks timeZone
  files <- getFileMetas
  mNewFile <- whenLoggedIn $ const newFile
  mDeleteFile <- whenLoggedIn $ const delFile
  basicPage $
    mconcat $ catMaybes
      [ pure $ H.h2 "Files",
        mNewFile,
        pure $ H.ul $
          mconcat $
            sortBy sortFileMeta files <&> \fm ->
              H.li (mconcat $ catMaybes
                [ pure $ fileLink tz fm,
                  pure H.br,
                  ($ fm) <$> mDeleteFile
                ]
              )
      ]
  where
    newFile =
      H.a
        ! hxBoost
        ! hxOn "::config-request" "setXsrfHeader(event)"
        ! htmlLink (Proxy @(AuthFile (CRUDCreateForm FileUpload))) $ "Create File"
    delFile m =
      H.button
        ! hxTrigger "click"
        -- ! hxSwap "outerHTML"
        -- ! hxTarget "#edit-delete-buttons"
        ! hxConfirm "Confirm deletion"
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
  Website.Data.File.deleteFile fId
  pure $ H.toHtml @String "Deleted"
