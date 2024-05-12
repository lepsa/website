module Website.Content.File where

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
import           Website.Data.File
import           Website.Data.User
import           Website.Data.Util
import           Website.Network.API.CRUD
import           Website.Network.API.Types
import           Website.Types
import Website.Data.Error

uploadFile :: forall c e m. (RequiredUser c, CanAppM c e m) => MultipartData Tmp -> m (Headers '[Header "Location" Text] Html)
uploadFile formData = do
  let fds = files formData
  fd <- case fds of
    [] -> throwError_ MissingUpload
    [fd] -> pure fd
    _ -> throwError_ TooManyUploads
  f <- mkFile fd >>= createFile
  pure
    $ addHeader (pack "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthFile (CRUDRead' FileId))) f.fileId))
    $ "Uploaded"
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
  file <- Website.Data.File.getFile fId
  let fileLen = BSL.length file.fileData
  pure
    $ addHeader fileLen
    $ WithCT
      (BSL.fromStrict $ encodeUtf8 file.fileType)
      file.fileData

getFiles :: (CanAppM c e m, OptionalUser c) => m Html
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
              H.li $ H.p (mconcat $ catMaybes
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
        ! htmlLink (Proxy @(AuthFile CRUDCreateForm)) $ "Create File"
    delFile m =
      H.button
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
  m <- getFileMeta fId
  Website.Data.File.deleteFile fId
  pure $ H.toHtml $ "Deleted " <> m.fileMetaName