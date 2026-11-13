module Website.Network.API.CRUD where

import qualified Data.ByteString.Lazy     as BSL
import           Data.Data                (Typeable)
import           Data.Int                 (Int64)
import           Data.Text                (Text)
import           Prelude                  (Maybe (Just))
import           Servant                  (Accept (contentType), Capture,
                                           Delete, FormUrlEncoded, Get, Header,
                                           Headers, JSON, MimeRender, NoContent,
                                           Put, ReqBody, StdMethod (POST), Verb,
                                           mimeRender, (:<|>), (:>))
import           Servant.API.ContentTypes (AllCTRender, handleAcceptH)
import           Servant.HTML.Blaze       (HTML)
import           Text.Blaze.Html          (Html)
import           Website.Data.Entry       (Entry, EntryUpdate)

-- A hack to return dynamic content types. This is used with `DOWNLOAD`.
-- `WithCT` specifies the content type, `DOWNLOAD` specifies that the
-- supplied bytes should be sent out as-is.
data WithCT = WithCT {header :: BSL.ByteString, content :: BSL.ByteString}

instance AllCTRender '[DOWNLOAD] WithCT where
  handleAcceptH _ _ (WithCT h c) = Just (h, c)

-- Send out raw bytes, and have `WithCT` specify the content types.
data DOWNLOAD deriving (Typeable)
instance MimeRender DOWNLOAD BSL.ByteString where
  mimeRender _ content = content
instance Accept DOWNLOAD where
  contentType _ = "*/*"

type LocHeaders a = Headers '[Header "Location" Text] a

-- This should always be paired with CRUDFormCreate so that we can get the initial form
-- for the CRUD creation, before posting to the CRUDCreate route, and getting the
-- response back
type CRUDFormCreateGet         = Get '[HTML] Html
type CRUDFileCreateGet         = Get '[HTML] Html
type CRUDFormCreatePost create = ReqBody '[FormUrlEncoded] create :> Verb 'POST 303 '[HTML] (LocHeaders Html)
type CRUDApiCreate      create = ReqBody '[FormUrlEncoded] create :> Verb 'POST 201 '[JSON] (LocHeaders Entry)
type CRUDFileCreatePost create = create                           :> Verb 'POST 201 '[HTML] (LocHeaders Html)

-- Read a specific resource
type CRUDFormRead key = Capture "key" key :> Get '[HTML] Html
type CRUDApiRead  key = Capture "key" key :> Get '[JSON] Entry
type CRUDFileRead key = Capture "key" key :> Get '[DOWNLOAD] (Headers '[Header "Content-Length" Int64] WithCT)

-- This should always be paired with CRUDUpdate so that we can get the initial form
-- for the CRUD update, before posting to the CRUDUpdate route, and getting the
-- response back
type CRUDFormUpdateGet         key = Capture "key" key :> "update"                                       :> Get '[HTML] Html
type CRUDFormUpdatePost update key = Capture "key" key :> "update" :> (ReqBody '[FormUrlEncoded] update) :> Put '[HTML] Html
type CRUDApiUpdate      update key = Capture "key" key :> "update" :> (ReqBody '[JSON] EntryUpdate)      :> Put '[JSON] Entry
type CRUDFileUpdateGet         key = Capture "key" key :> "update"                                       :> Get '[HTML] Html
type CRUDFileUpdatePost update key = Capture "key" key :> "update" :> (ReqBody '[FormUrlEncoded] update) :> Put '[HTML] Html

-- Delete a specific resource
type CRUDFormDelete key = Capture "key" key :> "delete" :> Delete '[HTML] Html
type CRUDApiDelete  key = Capture "key" key :> "delete" :> Delete '[JSON] NoContent
type CRUDFileDelete key = Capture "key" key :> "delete" :> Delete '[HTML] Html

-- A template for creating CRUD servers for specific types.
-- It will ensure that pages are provided so that users can perform the required actions
type CRUDForm create update key =
         CRUDFormCreatePost create
    :<|> CRUDFormCreateGet
    :<|> CRUDFormRead key
    :<|> CRUDFormUpdatePost update key
    :<|> CRUDFormUpdateGet key
    :<|> CRUDFormDelete key

type CRUDApi create update key =
         CRUDApiCreate create
    :<|> CRUDApiRead key
    :<|> CRUDApiUpdate update key
    :<|> CRUDApiDelete key

type CRUDFile create update key =
         CRUDFileCreatePost create
    :<|> CRUDFileCreateGet
    :<|> CRUDFileRead key
    -- :<|> CRUDFileUpdatePost update key
    -- :<|> CRUDFileUpdateGet key
    :<|> CRUDFileDelete key
