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
import Website.Auth.Authentication
import Data.Maybe (catMaybes)
import Servant.Auth.Server (AuthResult (Authenticated))

type Authed = AuthResult UserLogin
type AuthLogin = Auth Auths UserLogin
type AuthEntry a = AuthLogin :> "entry" :> a
type AuthUser a = AuthLogin :> "user" :> a

siteTitle :: String
siteTitle = "Owen's Site"

whenLoggedIn :: Authed -> (UserLogin -> H.Html) -> Maybe H.Html
whenLoggedIn (Authenticated l) f = pure $ f l
whenLoggedIn _ _ = Nothing

greetUser :: UserLogin -> H.Html
greetUser (UserLogin _ e) = H.toHtml $ "Welcome back, " <> e

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
pageHeader :: Authed -> Html
pageHeader auth =
  H.header $
    mconcat $ catMaybes
      [ pure $ H.a ! htmlLink (Proxy @(AuthLogin :> Get '[HTML] H.Html)) $
          H.h1 $ toHtml siteTitle
      , whenLoggedIn auth greetUser
      ]

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
        [ H.li $ H.a ! htmlLink (Proxy @(AuthLogin :> Get '[HTML] H.Html)) $ "Home",
          H.li $ H.a ! htmlLink (Proxy @(AuthLogin :> "entries" :> Get '[HTML] H.Html)) $ "Entries",
          H.li $ H.a ! htmlLink (Proxy @(AuthLogin :> "login" :> Get '[HTML] H.Html)) $ "Login",
          H.hr,
          H.li $ H.a ! HA.href "https://github.com/lepsa" $ "GitHub"
        ]

-- | 'basicPage' should be used as a wrapper on any route that could be loaded directly by a
--  user. This could be due to a page refresh, bookmark, history, etc.
--  'basicPage' includes all content and overall page structure that is required for styling and
--  HTMX interactivity.
basicPage :: Authed -> Html -> Html
basicPage auth content =
  mconcat
    [ H.docType,
      commonHead,
      H.body $
        mconcat
          [ pageHeader auth,
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
index :: Authed -> Html
index auth =
  basicPage auth $
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

loginForm :: Authed -> Html
loginForm auth =
  basicPage auth $
  H.form
    ! HA.class_ "contentform"
    ! HA.action (textValue $ linkText (Proxy @(AuthLogin :> "login" :> ReqBody '[FormUrlEncoded] Login :> Verb 'POST 303 '[HTML] (SetLoginCookies NoContent))))
    ! HA.method "POST"
    $ mconcat
      [ H.label ! HA.class_ "formlabel" ! HA.for "login" $ "Username"
      , H.input ! HA.class_ "formvalue" ! HA.name "login" ! HA.type_ "text"
      , H.br
      , H.label ! HA.class_ "formlabel" ! HA.for "password" $ "Password"
      , H.input ! HA.class_ "formvalue" ! HA.name "password" ! HA.type_ "password"
      , H.br
      , H.input
          ! HA.type_ "submit"
          ! HA.class_ "formbutton"
          ! HA.value "Login"
      ]