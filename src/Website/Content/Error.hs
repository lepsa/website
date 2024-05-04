module Website.Content.Error where

import Servant
import Website.Data.Error
import Website.Content.Common
import Text.Blaze.Html.Renderer.Utf8 qualified as H
import Text.Blaze.Html5 qualified as H

unauthentricated :: Authed -> ServerError
unauthentricated auth = err401
  { errBody = H.renderHtml $ basicPage auth $ H.p "Unauthenticated"
  }

unauthorised :: Authed -> ServerError
unauthorised auth = err403
  { errBody = H.renderHtml $ basicPage auth $ H.p "Unauthorised"
  }

internalServerError :: Authed -> ServerError
internalServerError auth = err500
  { errBody = H.renderHtml $ basicPage auth $ H.p "Internal Server Error"
  }

notFound :: Authed -> ServerError
notFound auth = err404
  { errBody = H.renderHtml $ basicPage auth $ H.p "Not Found"
  }

errToServerError :: Authed -> Err -> ServerError
errToServerError auth (DbError e) = dbErrToServerError auth e
errToServerError auth Unauthenticated = unauthentricated auth
errToServerError auth Unauthorised = unauthorised auth
errToServerError auth (Other _) = internalServerError auth

dbErrToServerError :: Authed -> DbErr -> ServerError
dbErrToServerError auth NotFound = notFound auth
dbErrToServerError auth TooManyResults = notFound auth
dbErrToServerError auth FailedToInsertRecord = internalServerError auth