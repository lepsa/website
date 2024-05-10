module Website.Network.API.CRUD where

import qualified Data.ByteString.Lazy     as BSL
import           Data.Data
import           Data.Int                 (Int64)
import           Data.Text
import           Servant
import           Servant.API.ContentTypes
import           Servant.HTML.Blaze
import           Text.Blaze.Html

data WithCT = WithCT {header :: BSL.ByteString, content :: BSL.ByteString}

instance AllCTRender '[DOWNLOAD] WithCT where
  handleAcceptH _ _ (WithCT h c) = Just (h, c)

data DOWNLOAD deriving (Typeable)

instance MimeRender DOWNLOAD BSL.ByteString where
  mimeRender _ content = content

instance Accept DOWNLOAD where
  contentType _ = "*/*"

-- This should always be paired with CRUDCreate so that we can get the initial form
-- for the CRUD creation, before posting to the CRUDCreate route, and getting the
-- response back
type CRUDCreateForm create = Get '[HTML] Html

type CRUDCreate create = CRUDCreate' (ReqBody '[FormUrlEncoded] create)
type CRUDCreate' create = create :> Verb 'POST 303 '[HTML] (Headers '[Header "Location" Text] Html)
type CRUDCreateFile create = create :> Verb 'POST 201 '[HTML] Html

-- Read a specific resource
type CRUDRead key = Capture "key" key :> Get '[HTML] Html

type CRUDRead' key = Capture "key" key :> Get '[DOWNLOAD] (Headers '[Header "Content-Length" Int64] WithCT)

-- This should always be paired with CRUDUpdate so that we can get the initial form
-- for the CRUD update, before posting to the CRUDUpdate route, and getting the
-- response back
type CRUDUpdateForm key = Capture "key" key :> "update" :> Get '[HTML] Html

type CRUDUpdate update key = CRUDUpdate' (ReqBody '[FormUrlEncoded] update) key

type CRUDUpdate' update key = Capture "key" key :> "update" :> update :> Put '[HTML] Html

-- Delete a specific resource
type CRUDDelete key = Capture "key" key :> "delete" :> Delete '[HTML] Html

-- A template for creating CRUD servers for specific types.
-- It will ensure that pages are provided so that users can perform the required actions
type CRUDForm create update key =
  CRUDCreate create
    :<|> CRUDCreateForm create
    :<|> CRUDRead key
    :<|> CRUDUpdate update key
    :<|> CRUDUpdateForm key
    :<|> CRUDDelete key

type CRUD create update key =
  CRUDCreate create
    :<|> CRUDRead key
    :<|> CRUDUpdate update key
    :<|> CRUDDelete key

type CRUDForm' create update key =
  CRUDCreateFile create
    :<|> CRUDCreateForm create
    :<|> CRUDRead' key
    -- :<|> CRUDUpdate' update key
    -- :<|> CRUDUpdateForm key
    :<|> CRUDDelete key
