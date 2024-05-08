module Website.Content.Error where

import Servant
import Website.Data.Error
import Website.Content.Common
import Text.Blaze.Html.Renderer.Utf8 qualified as H
import Text.Blaze.Html5 qualified as H
import Website.Data.User
import Control.Monad.Reader

unauthenticated :: (OptionalUser c, MonadReader c m) => m ServerError
unauthenticated = do
  h <- basicPage $ H.p "Unauthenticated"
  pure $ err401 { errBody = H.renderHtml h }

unauthorised :: (OptionalUser c, MonadReader c m) => m ServerError
unauthorised = do
  h <- basicPage $ H.p "Unauthorised"
  pure $ err403 { errBody = H.renderHtml h }

internalServerError :: (OptionalUser c, MonadReader c m) => String -> m ServerError
internalServerError msg = do
  h <- basicPage $ H.p $ H.toHtml $ "Internal Server Error" <> msg
  pure $ err500 { errBody = H.renderHtml h }

notFound :: (OptionalUser c, MonadReader c m) => m ServerError
notFound = do
  h <- basicPage $ H.p "Not Found"
  pure $ err404 { errBody = H.renderHtml h }

errToServerError :: (OptionalUser c, MonadReader c m) => Err -> m ServerError
errToServerError e = case e of
  DbError e' -> dbErrToServerError e'
  Unauthenticated -> unauthenticated
  Unauthorised -> unauthorised
  Other msg -> internalServerError msg

dbErrToServerError :: (OptionalUser c, MonadReader c m) => DbErr -> m ServerError
dbErrToServerError e = case e of
  NotFound -> notFound
  TooManyResults -> notFound
  FailedToInsertRecord -> internalServerError "Failed to Insert Record"