module Website.Content.Index where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Text.Blaze.Html
import qualified Text.Blaze.Html5       as H
import           Website.Content.Common
import           Website.Content.Entry
import           Website.Data.Entry
import           Website.Data.Env
import           Website.Data.User
import           Website.Types

-- | Initial landing page.
index :: (OptionalUser c, CanAppM c e m) => m Html
index = do
  $(logDebug) "index"
  tz <- asks timeZone
  posts <- sortEntriesByDateDesc <$> getRecentEntries recentPostCount
  basicPage $
    mconcat
      [ H.p "Welcome to my website.",
        H.p "I use this as a test bed for various ways of deploying code and working with server-driven client interactions.",
        H.p "The current version of the site uses a REST architecture where the server sends pre-rendered HTML with expected state interactions.",
        H.p "HTMX is used to help with server interactions, mainly extending HTTP verb support in forms and allowing a small amount of client side updating for user interactions.",
        H.h2 "Recent Entries",
        H.ul $ mconcat $
          H.li . entryLink tz <$> posts
      ]
  where
    recentPostCount = 5
