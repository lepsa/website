module Website.Content.Error where

import           Control.Monad.Reader
import           Servant
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5              as H
import           Website.Content.Common
import           Website.Data.Error
import           Website.Data.User
import Control.Monad.Logger

unauthenticated :: (OptionalUser c, MonadReader c m, MonadLogger m) => m ServerError
unauthenticated = do
  $(logDebug) "unauthenticated"
  h <- basicPage $ H.p "Unauthenticated"
  pure $ err401 { errBody = H.renderHtml h }

unauthorised :: (OptionalUser c, MonadReader c m, MonadLogger m) => m ServerError
unauthorised = do
  $(logDebug) "unauthorised"
  h <- basicPage $ H.p "Unauthorised"
  pure $ err403 { errBody = H.renderHtml h }

internalServerError :: (OptionalUser c, MonadReader c m, MonadLogger m) => String -> m ServerError
internalServerError msg = do
  $(logDebug) "internalServerError"
  h <- basicPage $ H.p $ H.toHtml $ "Internal Server Error" <> msg
  pure $ err500 { errBody = H.renderHtml h }

notFound :: (MonadLogger m, OptionalUser c, MonadReader c m) => m ServerError
notFound = do
  $(logDebug) "notFound"
  h <- basicPage $ H.p "Not Found"
  pure $ err404 { errBody = H.renderHtml h }

badRequest :: (MonadLogger m, OptionalUser c, MonadReader c m) => String -> m ServerError
badRequest msg = do
  $(logDebug) "badRequest"
  h <- basicPage $ H.p $ H.toHtml msg
  pure $ err400 { errBody = H.renderHtml h }

errToServerError :: (MonadLogger m, OptionalUser c, MonadReader c m) => Err -> m ServerError
errToServerError e = case e of
  DbError e'      -> dbErrToServerError e'
  Unauthenticated -> unauthenticated
  Unauthorised    -> unauthorised
  MissingUpload   -> badRequest "Missing an upload item"
  TooManyUploads  -> badRequest "Too many upload items"
  Other msg       -> internalServerError msg

dbErrToServerError :: (MonadLogger m, OptionalUser c, MonadReader c m) => DbErr -> m ServerError
dbErrToServerError e = case e of
  NotFound             -> notFound
  TooManyResults       -> notFound
  FailedToInsertRecord -> internalServerError "Failed to Insert Record"
