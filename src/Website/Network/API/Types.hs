module Website.Network.API.Types where

import           Data.Text                   (Text)
import           Servant                     (Proxy (..))
import           Servant.API                 hiding (BasicAuth)
import           Servant.Auth
import           Servant.Auth.Server
import           Servant.HTML.Blaze
import           Servant.Multipart
import           Text.Blaze.Html
import           Website.Auth.Authentication
import           Website.Data.Entry
import           Website.Data.File
import           Website.Data.User
import           Website.Network.API.CRUD

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
