module Website.Content.Common where

import Data.Text (Text, pack)
import Servant hiding (BasicAuth)
import Servant.HTML.Blaze
import Text.Blaze.Html
import Text.Blaze.Html qualified as H
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Website.Network.API.Types
import Website.Data.User
import Servant.Auth

type AuthEntry a = Auth Auths UserKey :> "entry" :> a
type AuthUser a = Auth Auths UserKey :> "user" :> a

siteTitle :: String
siteTitle = "Owen's Site"

-- | Helper function for generating typesafe internal API links for use in HTML attributes
htmlLink ::
  ( IsElem endpoint TopAPI,
    HasLink endpoint,
    ToHttpApiData (MkLink endpoint Link)
  ) =>
  Proxy endpoint ->
  Attribute
-- safeLink doesn't include a leading '/', so we need to stick one on here.
-- This also lets the API root routes work, otherwise the safeLink would be ""
htmlLink = HA.href . H.textValue . linkText

-- | Helper function when generating typesafe internal API links.
linkText ::
  ( IsElem endpoint TopAPI,
    HasLink endpoint,
    ToHttpApiData (MkLink endpoint Link)
  ) =>
  Proxy endpoint ->
  Text
linkText api = pack "/" <> toUrlPiece (safeLink topAPI api)

-- | A common location for common header elements
pageHeader :: Html
pageHeader =
  H.header $
    H.a ! htmlLink (Proxy @(Get '[HTML] H.Html)) $
      H.h1 $
        toHtml siteTitle

-- | A common location for common footer elements
pageFooter :: Html
pageFooter =
  H.footer mempty

-- | A common location for including resources needed by all pages. Invoked by 'basicPage'
commonHead :: Html
commonHead =
  H.head $
    mconcat
      [ H.link ! HA.rel (stringValue "stylesheet") ! HA.href (stringValue "/main.css"),
        H.script ! HA.src (stringValue "/htmx.min.js") $ mempty,
        H.title $ toHtml siteTitle
      ]

-- | Builds the list of links on the side. Invoked by 'basicPage'
sideNav :: Html
sideNav =
  H.nav $
    H.ul $
      mconcat
        [ H.li $ H.a ! htmlLink (Proxy @(Get '[HTML] H.Html)) $ "Home",
          H.li $ H.a ! htmlLink (Proxy @(Auth Auths UserKey :> "entries" :> Get '[HTML] H.Html)) $ "Entries",
          H.hr,
          H.li $ H.a ! HA.href "https://github.com/lepsa" $ "GitHub"
        ]

-- | 'basicPage' should be used as a wrapper on any route that could be loaded directly by a
--  user. This could be due to a page refresh, bookmark, history, etc.
--  'basicPage' includes all content and overall page structure that is required for styling and
--  HTMX interactivity.
basicPage :: Html -> Html
basicPage content =
  mconcat
    [ H.docType,
      commonHead,
      H.body $
        mconcat
          [ pageHeader,
            H.hr,
            H.div ! HA.id (stringValue "main-content") $
              mconcat
                [ sideNav,
                  H.main content
                ],
            pageFooter
          ]
    ]

-- | Initial landing page.
index :: Html
index =
  basicPage $
    mconcat
      [ H.p "Welcome to my website.",
        H.p "I use this as a test bed for various ways of deploying code and working with server-driven client interactions.",
        H.p "The current version of the site uses a REST architecture where the server sends pre-rendered HTML with expected state interactions.",
        H.p "HTMX is used to help with server interactions, mainly extending HTTP verb support in forms and allowing a small amount of client side updating for user interactions."
      ]

staticField :: String -> String -> Maybe String -> Html
staticField fieldName fieldLabel value =
  mconcat
    [ H.div ! HA.class_ "formlabel" ! HA.for (toValue fieldName) $ toHtml fieldLabel,
      H.div ! HA.class_ "formvalue" ! HA.name (toValue fieldName) $ maybe mempty toHtml value
    ]

-- | Helper function for building forms. Will apply styling, labels, and values.
formField :: String -> String -> String -> Maybe String -> Html
formField fieldName fieldLabel type_ value =
  mconcat
    [ H.label ! HA.class_ "formlabel" ! HA.for (toValue fieldName) $ toHtml fieldLabel,
      case type_ of
        "textarea" -> H.textarea ! HA.class_ "formvalue" ! HA.name (toValue fieldName) $ maybe mempty toHtml value
        _ -> H.input ! HA.class_ "formvalue" ! HA.name (toValue fieldName) ! HA.type_ (toValue type_) ! maybe mempty (HA.value . stringValue) value
    ]

-- | Helper function for building textarea inputs
formFieldTextArea :: String -> String -> Maybe String -> Html
formFieldTextArea fieldName fieldLabel value =
  mconcat
    [ H.label ! HA.for (toValue fieldName) $ toHtml fieldLabel,
      H.textarea ! HA.name (toValue fieldName) $ maybe mempty toHtml value
    ]
