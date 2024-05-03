module Website.Content.Error where

import Servant
import Website.Data.Error
import Website.Content.Common
import Text.Blaze.Html.Renderer.Utf8 qualified as H
import Text.Blaze.Html5 qualified as H

unauthentricated :: ServerError
unauthentricated = err401
  { errBody = H.renderHtml $ basicPage $ H.p "Unauthenticated"
  }

unauthorised :: ServerError
unauthorised = err403
  { errBody = H.renderHtml $ basicPage $ H.p "Unauthorised"
  }

internalServerError :: ServerError
internalServerError = err500
  { errBody = H.renderHtml $ basicPage $ H.p "Internal Server Error"
  }

notFound :: ServerError
notFound = err404
  { errBody = H.renderHtml $ basicPage $ H.p "Not Found"
  }

errToServerError :: Err -> ServerError
errToServerError (DbError e) = dbErrToServerError e
errToServerError Unauthenticated = unauthentricated
errToServerError Unauthorised = unauthorised
errToServerError (Other _) = internalServerError

dbErrToServerError :: DbErr -> ServerError
dbErrToServerError NotFound = notFound
dbErrToServerError TooManyResults = notFound
dbErrToServerError FailedToInsertRecord = internalServerError