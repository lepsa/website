module Website.Content.Common where

import Data.Text (Text, pack)
import Website.Network.API.Types
import Servant
import Data.List
import Servant.HTML.Blaze
import Text.Blaze.Html
import Text.Blaze.Html qualified as H
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Website.Data.Common
import Website.Content.HTMX (htmxAttribute)

siteTitle :: String
siteTitle = "Owen's Site"

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

linkText ::
  ( IsElem endpoint TopAPI,
    HasLink endpoint,
    ToHttpApiData (MkLink endpoint Link)
  ) =>
  Proxy endpoint ->
  Text
linkText api = pack "/" <> toUrlPiece (safeLink topAPI api)

pageHeader :: Html
pageHeader =
  H.header $
    H.a ! htmlLink (Proxy @(Get '[HTML] H.Html)) $
      H.h1 $
        toHtml siteTitle

pageFooter :: Html
pageFooter =
  H.footer mempty

commonHead :: Html
commonHead =
  H.head $
    mconcat
      [ H.link ! HA.rel (stringValue "stylesheet") ! HA.href (stringValue "/main.css"),
        H.script ! HA.src (stringValue "/htmx.min.js") $ mempty,
        H.title $ toHtml siteTitle
      ]

sideNav :: Html
sideNav =
  H.nav $
    H.ul $
      mconcat
        [ H.li $ H.a ! htmlLink (Proxy @(Get '[HTML] H.Html)) $ "Home",
          H.li $ H.a ! htmlLink (Proxy @("entries" :> Get '[HTML] H.Html)) $ "Entries",
          H.hr,
          H.li $ H.a ! HA.href "https://github.com/lepsa" $ "GitHub"
        ]

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

index :: Html
index =
  basicPage $
    H.p "Hello, Index!"

foo :: Html
foo =
  basicPage $
    mconcat
      [ H.p "Hello, World!"
      ]

dynamic :: String -> Html
dynamic s = H.p $ H.toHtml s

formField :: String -> String -> String -> Maybe String -> Html
formField fieldName fieldLabel type_ value = mconcat
  [ H.label ! HA.class_ "formlabel" ! HA.for (toValue fieldName) $ toHtml fieldLabel
  , H.input ! HA.class_ "formvalue" ! HA.name (toValue fieldName) ! HA.type_ (toValue type_) ! maybe mempty (HA.value . stringValue) value
  ]

formFieldTextArea :: String -> String -> Maybe String -> Html
formFieldTextArea fieldName fieldLabel value = mconcat
  [ H.label ! HA.for (toValue fieldName) $ toHtml fieldLabel
  , H.textarea ! HA.name (toValue fieldName) $ maybe mempty toHtml value
  ]

generateNewForm :: GenerateForm a => Proxy a -> Html
generateNewForm p = H.form
  ! HA.class_ "contentform"
  ! maybe mempty (\url -> HA.method "POST" <> HA.action (stringValue url)) fd.createUrl
  $ mconcat
  [ mconcat $ intersperse H.br $ generateField <$> fd.fields
  , H.br
  , H.input ! HA.class_ "formbutton" ! HA.type_ "submit" ! HA.value "Create"
  ]
  where
    fd = newForm p

generateUpdateForm :: GenerateForm a => a -> Html
generateUpdateForm a = H.form
  ! HA.class_ "contentform"
  ! maybe mempty (htmxAttribute "hx-put" . stringValue) fd.updateUrl
  $ mconcat
  [ mconcat $ intersperse H.br $ generateField <$> fd.fields
  , H.br
  , H.input
    ! HA.type_ "submit"
    ! HA.class_ "formbutton"
    ! HA.value "Update"
  ]
  where
    fd = updateForm a

generateField :: FieldData -> Html
generateField fd = formField fd.name fd.label fd.type_ fd.value