module Website.Network.API.Types where

import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html
import {-# SOURCE #-} Website.Data.Entry
import Website.Network.API.CRUD

topAPI :: Proxy TopAPI
topAPI = Proxy

type TopAPI =
  Get '[HTML] Html
    :<|> "entry" :> CRUD EntryCreate EntryUpdate EntryKey
    :<|> "entries" :> Get '[HTML] Html
    :<|> Raw