module Website.Auth.Authorisation where

import           Control.Monad                    ((>=>))
import qualified Data.Text                        as T
import           Database.SQLite.Simple.FromField (FromField, fromField)
import           Database.SQLite.Simple.ToField   (ToField, toField)
import           GHC.Generics                     (Generic)
import           Prelude                          (Bounded, Either (Left), Enum,
                                                   Eq, Ord, Read, Show, fail,
                                                   maybe, pure, show, ($), (.),
                                                   (<>))
import           Servant                          (FromHttpApiData)
import           Servant.API                      (parseQueryParam)
import           Text.Blaze                       (ToMarkup, toMarkup)
import           Text.Read                        (readMaybe)

--
-- Authorisation based on Unix style permissions.
--

-- Admins inherit all user permissions
data Group
  = Anon
  | User
  | Admin
  deriving (Eq, Show, Read, Ord, Generic, Bounded, Enum)
instance FromHttpApiData Group where
  parseQueryParam t = maybe (Left $ "Could not parse Group: " <> t) pure . readMaybe $ T.unpack t
-- instance ToJSON Group
-- instance FromJSON Group
instance FromField Group where
  fromField = fromField >=> maybe (fail "Could not parse Group") pure . readMaybe
instance ToField Group where
  toField = toField . show

instance ToMarkup Group where
    toMarkup = toMarkup . show

data Access
  = None
  | Read
  | Write
  deriving (Eq, Show, Read, Ord)
instance FromField Access where
  fromField = fromField >=> maybe (fail "Could not parse Access") pure . readMaybe
instance ToField Access where
  toField = toField . show
