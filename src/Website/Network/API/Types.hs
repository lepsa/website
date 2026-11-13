module Website.Network.API.Types where

import           Data.Text                   (Text)
import           Servant                     (Proxy (Proxy))
import           Servant.API                 (Capture, FormUrlEncoded, Get,
                                              Header, Headers, NoContent, Raw,
                                              ReqBody, StdMethod (POST), Verb,
                                              (:<|>), (:>))
import           Servant.Auth                (Auth, BasicAuth, Cookie, JWT)
import           Servant.Auth.Server         (SetCookie)
import           Servant.HTML.Blaze          (HTML)
import           Servant.Multipart           (MultipartData, MultipartForm, Tmp)
import           Text.Blaze.Html             (Html)
import           Website.Auth.Authentication (Login)
import           Website.Data.Entry          (EntryCreate, EntryKey,
                                              EntryUpdate)
import           Website.Data.File           (FileId)
import           Website.Data.User           (UserCreate, UserKey, UserUpdate)
import           Website.Network.API.CRUD    (CRUDFile, CRUDForm)

--
-- Top level API. The type is used when writing servers, and the value is used to generate API type safe links.
--

topAPI :: Proxy TopAPI
topAPI = Proxy

type AuthTypes = '[BasicAuth, Cookie, JWT]
type AppAuth = Auth AuthTypes UserKey
type TopAPI = AppAuth :> API

-- These write to a tmp file. This file _must_ be read
-- in its entirety before the handler returns, as the
-- tmp file will be cleaned up shortly after the handler
-- completes.
type FileUpload = MultipartForm Tmp (MultipartData Tmp)

type EntryTitleGet = "title" :> Capture "title" Text :> Get '[HTML] Html

type LoginResponse = Verb 'POST 303 '[HTML] (SetLoginCookies NoContent)
type LogoutResponse = Verb 'POST 200 '[HTML] (SetLogoutCookies NoContent)
type Index = Get '[HTML] Html

type SetLoginCookies a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie, Header "Location" Text] a
type SetLogoutCookies a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie, Header "Location" Text, Header "HX-Redirect" Text] a
type API =
  Index
    :<|> "login" :> Get '[HTML] Html
    :<|> "login" :> ReqBody '[FormUrlEncoded] Login :> LoginResponse
    :<|> "logout" :> LogoutResponse
    :<|> "register" :> ReqBody '[FormUrlEncoded] UserCreate :> Verb 'POST 204 '[HTML] (SetLoginCookies NoContent)
    :<|> "user" :> CRUDForm UserCreate UserUpdate UserKey
    :<|> "users" :> Get '[HTML] Html
    :<|> "entry" :> CRUDForm EntryCreate EntryUpdate EntryKey
    :<|> "entry" :> EntryTitleGet
    :<|> "entries" :> Get '[HTML] Html
    :<|> "file" :> CRUDFile FileUpload FileUpload FileId
    :<|> "files" :> Get '[HTML] Html
    :<|> "static" :> Raw
