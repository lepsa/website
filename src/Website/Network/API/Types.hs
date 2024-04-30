module Website.Network.API.Types where

import Servant (Proxy (..))
import Servant.HTML.Blaze
import Text.Blaze.Html
import {-# SOURCE #-} Website.Data.Entry
import Website.Network.API.CRUD
import Servant.Auth
import Website.Data.User
import Website.Auth.Authentication
import Servant.Auth.Server
import Servant.API hiding (BasicAuth)
import Data.Text (Text)

--
-- Top level API. The type is used when writing servers, and the value is used to generate API type safe links.
--

topAPI :: Proxy TopAPI
topAPI = Proxy

type Auths = '[BasicAuth, JWT]

type TopAPI = TopAPI' Auths
type TopAPI' auths = Auth auths UserId :> Protected :<|> Unprotected

type SetCookies a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] a
type SetLoginCookies a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie, Header "Location" Text] a
type Unprotected =
  Get '[HTML] Html
    :<|> "login" :> ReqBody '[FormUrlEncoded] Login :> Verb 'POST 204 '[HTML] (SetCookies NoContent)
    :<|> "register" :> ReqBody '[FormUrlEncoded] UserCreate :> Verb 'POST 204 '[HTML] (SetCookies NoContent)
    :<|> Raw

type Protected =
  "entry" :> CRUD EntryCreate EntryUpdate EntryKey
    :<|> "entries" :> Get '[HTML] Html
    :<|> "user" :> CRUD UserCreate UserUpdate UserId