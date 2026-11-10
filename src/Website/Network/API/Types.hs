module Website.Network.API.Types where

import           Data.Text                   (Text)
import           Prelude                     (String)
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
import           Website.Network.API.CRUD    (CRUDForm, CRUDForm')

--
-- Top level API. The type is used when writing servers, and the value is used to generate API type safe links.
--

topAPI :: Proxy TopAPI
topAPI = Proxy

type Auths = '[BasicAuth, Cookie, JWT]

type TopAPI = Auth Auths UserKey :> API

-- These write to a tmp file. This file _must_ be read
-- in its entirety before the handler returns, as the
-- tmp file will be cleaned up shortly after the handler
-- completes.
type FileUpload = MultipartForm Tmp (MultipartData Tmp)

type SetLoginCookies a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie, Header "Location" Text] a
type API =
  Get '[HTML] Html
    :<|> "login" :> Get '[HTML] Html
    :<|> "login" :> ReqBody '[FormUrlEncoded] Login :> Verb 'POST 303 '[HTML] (SetLoginCookies NoContent)
    :<|> "register" :> ReqBody '[FormUrlEncoded] UserCreate :> Verb 'POST 204 '[HTML] (SetLoginCookies NoContent)
    :<|> "user" :> CRUDForm UserCreate UserUpdate UserKey
    :<|> "users" :> Get '[HTML] Html
    :<|> "entry" :> CRUDForm EntryCreate EntryUpdate EntryKey
    :<|> "entry" :> Capture "title" String :> Get '[HTML] Html
    :<|> "entries" :> Get '[HTML] Html
    :<|> "file" :> CRUDForm' FileUpload FileUpload FileId
    :<|> "files" :> Get '[HTML] Html
    :<|> "static" :> Raw
