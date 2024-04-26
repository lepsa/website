module Website.Network.API.Types where

import Servant (Proxy (..))
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html
import {-# SOURCE #-} Website.Data.Entry
import Website.Network.API.CRUD
import Servant.Auth
import Website.Data.User (User)
import Website.Auth.Authentication (Login)
import Servant.Auth.Server
import Servant.API hiding (BasicAuth)

--
-- Top level API. The type is used when writing servers, and the value is used to generate API type safe links.
--

topAPI :: Proxy TopAPI
topAPI = Proxy

type Auths = '[BasicAuth, JWT]
type TopAPI = TopAPI' Auths
type TopAPI' auths = Auth auths User :> Protected :<|> Unprotected
type Unprotected =
  Get '[HTML] Html
    :<|> "login" :> ReqBody '[FormUrlEncoded] Login :> Verb 'POST 204 '[HTML] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> Raw
type Protected =
  "entry" :> CRUD EntryCreate EntryUpdate EntryKey
    :<|> "entries" :> Get '[HTML] Html