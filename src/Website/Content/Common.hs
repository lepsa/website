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
import Servant.Auth.Server (AuthResult)
import Control.Monad.Except
import Control.Monad.Reader
import Website.Data.Env

type Authed = AuthResult UserKey
type AuthLogin = Auth Auths UserKey
type AuthEntry a = AuthLogin :> "entry" :> a
type AuthUser a = AuthLogin :> "user" :> a

siteTitle :: String
siteTitle = "Owen's Site"

-- tryError and withError are copied from mtl-2.3.1
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

withError :: MonadError e m => (e -> e) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) pure

whenLoggedIn :: (HasAuth c (Maybe UserLogin), MonadReader c m) => (UserLogin -> H.Html) -> m (Maybe H.Html)
whenLoggedIn f = do
  mUser <- asks auth
  case mUser of
    Nothing -> pure Nothing
    Just user -> pure $ pure $ f user

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
pageHeader :: MonadReader (EnvAuthed (Maybe UserLogin)) m => m Html
pageHeader = do
  loggedIn <- whenLoggedIn greetUser
  pure $ H.header $
    mconcat $ catMaybes
      [ pure $ H.a
          ! dataAttribute "hx-boost" "true"
          ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
          ! htmlLink (Proxy @(AuthLogin :> Get '[HTML] H.Html)) $
          H.h1 $ toHtml siteTitle
      , loggedIn
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
        H.script ! HA.src (stringValue "/main.js") $ mempty,
        H.script ! HA.src (stringValue "/htmx.min.js") $ mempty,
        H.title $ toHtml siteTitle
      ]

-- | Builds the list of links on the side. Invoked by 'basicPage'
sideNav :: MonadReader (EnvAuthed (Maybe UserLogin)) m => m Html
sideNav = do
  mHtml <- whenLoggedIn $ \_ -> mconcat
    [ H.li $ H.a
      ! dataAttribute "hx-boost" "true"
      ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
      ! htmlLink (Proxy @(AuthLogin :> "entries" :> Get '[HTML] H.Html)) $ "Entries",
      H.li $ H.a
      ! dataAttribute "hx-boost" "true"
      ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
      ! htmlLink (Proxy @(AuthLogin :> "users" :> Get '[HTML] H.Html)) $ "Users"
    ]
  pure $ H.nav $
    H.ul $
      mconcat $ catMaybes
        [ pure $ H.li $ H.a
          ! dataAttribute "hx-boost" "true"
          ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
          ! htmlLink (Proxy @(AuthLogin :> Get '[HTML] H.Html)) $ "Home",
          mHtml,
          pure H.hr,
          pure $ H.li $ H.a
          ! dataAttribute "hx-boost" "true"
          ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
          ! htmlLink (Proxy @(AuthLogin :> "login" :> Get '[HTML] H.Html)) $ "Login",
          pure H.hr,
          pure $ H.li $ H.a ! HA.href "https://github.com/lepsa" $ "GitHub"
        ]

-- | 'basicPage' should be used as a wrapper on any route that could be loaded directly by a
--  user. This could be due to a page refresh, bookmark, history, etc.
--  'basicPage' includes all content and overall page structure that is required for styling and
--  HTMX interactivity.
basicPage :: MonadReader (EnvAuthed (Maybe UserLogin)) m => Html -> m Html
basicPage content = do
  header <- pageHeader
  nav <- sideNav
  pure $ mconcat
    [ H.docType,
      commonHead,
      H.body
        $ mconcat
          [ header,
            H.hr,
            H.div ! HA.id (stringValue "main-content") $
              mconcat
                [ nav,
                  H.main content
                ],
            pageFooter
          ]
    ]

-- | Initial landing page.
index :: MonadReader (EnvAuthed (Maybe UserLogin)) m => m Html
index = basicPage $
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

loginForm :: MonadReader (EnvAuthed (Maybe UserLogin)) m => m Html
loginForm = basicPage $
  H.form
    ! HA.class_ "contentform"
    ! HA.action (textValue $ linkText (Proxy @(AuthLogin :> "login" :> ReqBody '[FormUrlEncoded] Login :> Verb 'POST 303 '[HTML] (SetLoginCookies NoContent))))
    ! HA.method "POST"
    -- ! dataAttribute "hx-post" (textValue $ linkText (Proxy @(AuthLogin :> "login" :> ReqBody '[FormUrlEncoded] Login :> Verb 'POST 303 '[HTML] (SetLoginCookies NoContent))))
    ! dataAttribute "hx-boost" "true"
    ! dataAttribute "hx-on::config-request" "setXsrfHeader(event)"
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
          ! dataAttribute "hx-boost" "true"
          ! HA.value "Login"
      ]