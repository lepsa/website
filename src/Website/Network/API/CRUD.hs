module Website.Network.API.CRUD where

import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html
import Data.Text

-- This should always be paired with CRUDCreate so that we can get the initial form
-- for the CRUD creation, before posting to the CRUDCreate route, and getting the
-- response back
type CRUDCreateForm create = Get '[HTML] Html
type CRUDCreate create = ReqBody '[FormUrlEncoded] create :> Verb 'POST 303 '[HTML] (Headers '[Header "Location" Text] Html)

-- Read a specific resource
type CRUDRead key = Capture "key" key :> Get '[HTML] Html

-- This should always be paired with CRUDUpdate so that we can get the initial form
-- for the CRUD update, before posting to the CRUDUpdate route, and getting the
-- response back
type CRUDUpdateForm key = Capture "key" key :> "update" :> Get '[HTML] Html
type CRUDUpdate update key = Capture "key" key :> "update" :> ReqBody '[FormUrlEncoded] update :> Put '[HTML] Html

-- Delete a specific resource
type CRUDDelete key = Capture "key" key :> "delete" :> Delete '[HTML] Html

-- A template for creating CRUD servers for specific types.
-- It will ensure that pages are provided so that users can perform the required actions
type CRUD create update key =
  CRUDCreate create
    :<|> CRUDCreateForm create
    :<|> CRUDRead key
    :<|> CRUDUpdate update key
    :<|> CRUDUpdateForm
     key
    :<|> CRUDDelete key