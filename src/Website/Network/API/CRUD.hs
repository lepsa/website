module Website.Network.API.CRUD where

import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html

-- This should always be paired with CRUDCreate so that we can get the initial form
-- for the CRUD creation, before posting to the CRUDCreate route, and getting the
-- response back
type CRUDCreate create = ReqBody '[FormUrlEncoded] create :> Post '[HTML] Html

type CRUDCreateForm create = Get '[HTML] Html

-- Read a specific resource
type CRUDRead key = Capture "key" key :> Get '[HTML] Html

-- This should always be paired with CRUDUpdate so that we can get the initial form
-- for the CRUD update, before posting to the CRUDUpdate route, and getting the
-- response back
type CRUDUpdate update key = Capture "key" key :> "update" :> ReqBody '[FormUrlEncoded] update :> Put '[HTML] Html

type CRUDUpdateForm key = Capture "key" key :> "update" :> Get '[HTML] Html

-- Delete a specific resource
type CRUDDelete key = Capture "key" key :> "delete" :> Delete '[HTML] Html

type CRUD create update key =
  CRUDCreate create
    :<|> CRUDCreateForm create
    :<|> CRUDRead key
    :<|> CRUDUpdate update key
    :<|> CRUDUpdateForm key
    :<|> CRUDDelete key