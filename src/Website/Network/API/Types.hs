module Website.Network.API.Types where

import Servant (Proxy (..))
import Servant.HTML.Blaze
import Text.Blaze.Html
import Website.Data.Entry
import Website.Data.User
import Website.Network.API.CRUD
import Servant.Auth
import Website.Auth.Authentication
import Servant.Auth.Server
import Servant.API hiding (BasicAuth)
import Data.Text (Text)

--
-- Top level API. The type is used when writing servers, and the value is used to generate API type safe links.
--

topAPI :: Proxy TopAPI
topAPI = Proxy

type Auths = '[BasicAuth, Cookie, JWT]

type TopAPI = TopAPI' Auths
type TopAPI' auths = Auth auths UserKey :> (Protected :<|> Unprotected)

type SetLoginCookies a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie, Header "Location" Text] a
type Unprotected =
  Get '[HTML] Html
    :<|> "login" :> Get '[HTML] Html
    :<|> "login" :> ReqBody '[FormUrlEncoded] Login :> Verb 'POST 303 '[HTML] (SetLoginCookies NoContent)
    :<|> "register" :> ReqBody '[FormUrlEncoded] UserCreate :> Verb 'POST 204 '[HTML] (SetLoginCookies NoContent)
    :<|> Raw

type Protected =
  "entry" :> CRUDForm EntryCreate EntryUpdate EntryKey
    :<|> "entries" :> Get '[HTML] Html
    :<|> "user" :> CRUDForm UserCreate UserUpdate UserKey
    :<|> "users" :> Get '[HTML] Html