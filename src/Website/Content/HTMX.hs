module Website.Content.HTMX where

import Data.String
import Text.Blaze
import Text.Blaze.Internal

-- |Helper function for adding HTMX attributes
htmxAttribute :: String -> AttributeValue -> Attribute
htmxAttribute hxName value = attribute "" (fromString $ " " <> hxName <> "=\"") value