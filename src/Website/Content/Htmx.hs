module Website.Content.Htmx where

import Text.Blaze.Internal qualified as HI
import Text.Blaze.Html5 qualified as H
import qualified Text.Blaze as HA

hxBoost :: H.Attribute
hxBoost = HI.customAttribute "hx-boost" "true"

hxOn :: String -> H.AttributeValue -> H.Attribute
hxOn event = HI.customAttribute (H.stringTag $ "hx-on" <> event)

hxTrigger :: HA.AttributeValue -> H.Attribute
hxTrigger = HI.customAttribute "hx-trigger"

hxSwap :: HA.AttributeValue -> H.Attribute
hxSwap = HI.customAttribute "hx-swap"

hxTarget :: HA.AttributeValue -> H.Attribute
hxTarget = HI.customAttribute "hx-target"

hxGet :: HA.AttributeValue -> H.Attribute
hxGet = HI.customAttribute "hx-get"

hxDelete :: HA.AttributeValue -> H.Attribute
hxDelete = HI.customAttribute "hx-delete"

hxPut :: HA.AttributeValue -> H.Attribute
hxPut = HI.customAttribute "hx-put"

hxConfirm :: HA.AttributeValue -> H.Attribute
hxConfirm = HI.customAttribute "hx-confirm"