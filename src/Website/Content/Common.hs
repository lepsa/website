{-# LANGUAGE TemplateHaskell #-}
module Website.Content.Common where

import           Control.Monad.Reader
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Text                   (Text, pack)
import           Servant                     hiding (BasicAuth)
import           Servant.Auth
import           Servant.Auth.Server         (AuthResult)
import           Servant.HTML.Blaze
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Website.Auth.Authentication
import           Website.Auth.Authorisation
import           Website.Content.Htmx
import           Website.Data.Env
import           Website.Data.User
import           Website.Network.API.Types
import           Website.Types
import GitHash

type Authed = AuthResult UserKey
type AuthLogin = Auth Auths UserKey
type AuthEntry a = AuthLogin :> "entry" :> a
type AuthUser a = AuthLogin :> "user" :> a
type AuthFile a = AuthLogin :> "file" :> a

siteTitle :: String
siteTitle = "Owen's Site"

whenAdmin :: (RequiredUser c, CanAppM c e m) => (UserLogin -> H.Html) -> m (Maybe H.Html)
whenAdmin f = do
  user <- asks auth
  case user.unUserLogin.group of
    Admin -> pure $ pure $ f user
    _     -> pure Nothing

whenLoggedIn :: (OptionalUser c, MonadReader c m) => (UserLogin -> a) -> m (Maybe a)
whenLoggedIn f = do
  mUser <- asks mAuth
  case mUser of
    Nothing   -> pure Nothing
    Just user -> pure $ pure $ f user

greetUser :: UserLogin -> H.Html
greetUser (UserLogin u) = H.p
  ! HA.id "greeter"
  $ mconcat
  [ "Welcome back,"
  , H.br
  , H.toHtml u.email
  ]

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
pageHeader :: (OptionalUser c, MonadReader c m) => m Html
pageHeader = do
  loggedIn <- whenLoggedIn greetUser
  pure $ H.header $
    mconcat $ catMaybes
      [ pure $ H.a
          ! HA.id "site-header"
          ! hxBoost
          ! hxOn "::config-request" "setXsrfHeader(event)"
          ! htmlLink (Proxy @(AuthLogin :> Get '[HTML] H.Html)) $
          H.h2 $ toHtml siteTitle
      , loggedIn
      ]

-- | A common location for common footer elements
pageFooter :: Html
pageFooter = H.footer $ mconcat
  [ H.p $ H.a ! HA.href "https://github.com/lepsa/website" $ "Project Repository"
  , H.p $ H.toHtml $ "Code Version: " <> giBranch gi <> "@" <> giHash gi
  ]
  where
    gi = $$tGitInfoCwd

-- | A common location for including resources needed by all pages. Invoked by 'basicPage'
commonHead :: Html
commonHead = H.head $ mconcat
  [ H.link ! HA.rel (stringValue "stylesheet") ! HA.href (stringValue "/static/main.css"),
    H.script ! HA.src (stringValue "/static/main.js") $ mempty,
    H.script ! HA.src (stringValue "/static/htmx.min.js") $ mempty,
    H.title $ toHtml siteTitle
  ]

-- | Builds the list of links on the side. Invoked by 'basicPage'
sideNav :: (OptionalUser c, MonadReader c m) => m Html
sideNav = do
  mUsers <- whenLoggedIn $ \_ -> mconcat
    [
      H.li $ H.a
      ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! htmlLink (Proxy @(AuthLogin :> "users" :> Get '[HTML] H.Html)) $ "Users"
    ]
  pure $ H.nav $
    H.ul $
      mconcat $ catMaybes
        [ pure $ H.li $ H.a
          ! hxBoost
          ! hxOn "::config-request" "setXsrfHeader(event)"
          ! htmlLink (Proxy @(AuthLogin :> Get '[HTML] H.Html)) $ "Home",
          pure $ H.li $ H.a
          ! hxBoost
          ! hxOn "::config-request" "setXsrfHeader(event)"
          ! htmlLink (Proxy @(AuthLogin :> "entries" :> Get '[HTML] H.Html)) $ "Entries",
          pure $ H.li $ H.a
          ! hxBoost
          ! hxOn "::config-request" "setXsrfHeader(event)"
          ! htmlLink (Proxy @(AuthLogin :> "files" :> Get '[HTML] H.Html)) $ "Files",
          mUsers,
          pure $ H.li ! HA.class_ "nav-separator" $ H.a
          ! hxBoost
          ! hxOn "::config-request" "setXsrfHeader(event)"
          ! htmlLink (Proxy @(AuthLogin :> "login" :> Get '[HTML] H.Html)) $ "Login",
          pure $ H.li ! HA.class_ "nav-separator" $ H.a ! HA.href "https://github.com/lepsa" $ "GitHub"
        ]

-- | 'basicPage' should be used as a wrapper on any route that could be loaded directly by a
--  user. This could be due to a page refresh, bookmark, history, etc.
--  'basicPage' includes all content and overall page structure that is required for styling and
--  HTMX interactivity.
basicPage :: (OptionalUser c, MonadReader c m) => Html -> m Html
basicPage content = do
  header <- pageHeader
  nav <- sideNav
  pure $ mconcat
    [ H.docType,
      commonHead,
      H.body $ H.div
        ! HA.class_ "page-div"
        $ mconcat
          [ header,
            H.div ! HA.id (stringValue "main-content") $
              mconcat
                [ nav,
                  H.main content
                ],
            pageFooter
          ]
    ]

staticField :: String -> String -> Maybe String -> Html
staticField fieldName fieldLabel value = mconcat
  [ H.div ! HA.class_ "formlabel" ! HA.for (toValue fieldName) $ toHtml fieldLabel,
    H.div ! HA.class_ "formvalue" ! HA.name (toValue fieldName) $ maybe mempty toHtml value
  ]

-- | Helper function for building forms. Will apply styling, labels, and values.
formField :: String -> String -> String -> Maybe String -> Html
formField fieldName fieldLabel type_ value = mconcat
  [ H.label ! HA.class_ "formlabel" ! HA.for (toValue fieldName) $ toHtml fieldLabel,
    case type_ of
      "textarea" -> H.textarea ! HA.class_ "formvalue" ! HA.name (toValue fieldName) $ maybe mempty toHtml value
      _ -> H.input ! HA.class_ "formvalue" ! HA.name (toValue fieldName) ! HA.type_ (toValue type_) ! maybe mempty (HA.value . stringValue) value
  ]

selectField :: forall a. (Eq a, Enum a, Bounded a, Show a) => String -> String -> Maybe a -> Html
selectField fieldName fieldLabel value = mconcat
  [ H.label ! HA.class_ "formlabel" ! HA.for (toValue fieldName) $ toHtml fieldLabel,
    H.select ! HA.class_ "formvalue" ! HA.name (toValue fieldName) $ mconcat $
      mkOption <$> [minBound..maxBound]
  ]
  where
    mkOption :: a -> Html
    mkOption option = H.option
      ! HA.value (stringValue val)
      ! maybe mempty selected value
      $ toHtml val
      where
        val = show option
        selected a = if a == option
          then HA.selected "true"
          else mempty

-- | Helper function for building textarea inputs
formFieldTextArea :: String -> String -> Maybe String -> Html
formFieldTextArea fieldName fieldLabel value = mconcat
  [ H.label ! HA.for (toValue fieldName) $ toHtml fieldLabel,
    H.textarea ! HA.name (toValue fieldName) $ maybe mempty toHtml value
  ]

loginForm :: (OptionalUser c, MonadReader c m) => m Html
loginForm = basicPage $
  H.form
    ! HA.class_ "contentform"
    ! HA.action (textValue $ linkText (Proxy @(AuthLogin :> "login" :> ReqBody '[FormUrlEncoded] Login :> Verb 'POST 303 '[HTML] (SetLoginCookies NoContent))))
    ! HA.method "POST"
    ! hxBoost
    ! hxOn "::config-request" "setXsrfHeader(event)"
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
          ! hxBoost
          ! HA.value "Login"
      ]

editDeleteButtons :: AttributeValue -> Maybe Text -> Text -> Text -> Html
editDeleteButtons target confirm eLink dLink = H.div
  ! HA.id "edit-delete-buttons"
  $ mconcat
    [ edit,
      delete
    ]
  where
    edit :: Html
    edit = H.button
      ! hxTrigger "click"
      ! hxSwap "outerHTML"
      ! hxTarget target
      ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! hxGet (H.textValue eLink)
      $ "Edit"
    delete :: Html
    delete = H.button
      ! hxTrigger "click"
      ! hxSwap "outerHTML"
      ! hxTarget "#edit-delete-buttons"
      ! hxConfirm (H.textValue $ "Confirm deletion" <> fromMaybe mempty confirm)
      ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! hxDelete (H.textValue dLink)
      $ "Delete"
