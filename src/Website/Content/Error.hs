module Website.Content.Error where

import Servant
import Website.Data.Error
import Website.Content.Common
import Text.Blaze.Html.Renderer.Utf8 qualified as H
import Text.Blaze.Html5 qualified as H
import Website.Data.User
import Website.Data.Env
import Control.Monad.Reader

unauthenticated :: MonadReader (EnvAuthed (Maybe UserLogin)) m => m ServerError
unauthenticated = do
  h <- basicPage $ H.p "Unauthenticated"
  pure $ err401 { errBody = H.renderHtml h }

unauthorised :: MonadReader (EnvAuthed (Maybe UserLogin)) m => m ServerError
unauthorised = do
  h <- basicPage $ H.p "Unauthorised"
  pure $ err403 { errBody = H.renderHtml h }

internalServerError :: MonadReader (EnvAuthed (Maybe UserLogin)) m => m ServerError
internalServerError = do
  h <- basicPage $ H.p "Internal Server Error"
  pure $ err500 { errBody = H.renderHtml h }

notFound :: MonadReader (EnvAuthed (Maybe UserLogin)) m => m ServerError
notFound = do
  h <- basicPage $ H.p "Not Found"
  pure $ err404 { errBody = H.renderHtml h }

errToServerError :: MonadReader (EnvAuthed (Maybe UserLogin)) m => Err -> m ServerError
errToServerError e = case e of
  DbError e' -> dbErrToServerError e'
  Unauthenticated -> unauthenticated
  Unauthorised -> unauthorised
  Other _ -> internalServerError

dbErrToServerError :: MonadReader (EnvAuthed (Maybe UserLogin)) m => DbErr -> m ServerError
dbErrToServerError e = case e of
  NotFound -> notFound
  TooManyResults -> notFound
  FailedToInsertRecord -> internalServerError