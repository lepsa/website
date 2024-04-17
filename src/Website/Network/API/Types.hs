module Website.Network.API.Types where

import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html
import {-# SOURCE #-} Website.Data.Entry
import Website.Network.API.CRUD

--
-- Top level API. The type is used when writing servers, and the value is used to generate API type safe links.
--

topAPI :: Proxy TopAPI
topAPI = Proxy

type TopAPI =
  Get '[HTML] Html
    :<|> "entry" :> CRUD EntryCreate EntryUpdate EntryKey
    :<|> "entries" :> Get '[HTML] Html
    :<|> Raw