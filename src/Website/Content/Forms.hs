module Website.Content.Forms where
import Website.Data.User
import Website.Network.API.CRUD
import Text.Blaze.Html
import Control.Monad.Reader
import Website.Data.Env
import Data.Data
import Website.Data.Entry
import Website.Network.API.Types
import Website.Content.Common
import Servant
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Data.List
import Website.Data.Error
import Website.Types
-- | Values for a given form field
data FieldData
  = FieldData
      { label :: String,
        name :: String,
        type_ :: String,
        value :: Maybe String
      }
  | StaticData
      { label :: String,
        name :: String,
        value :: Maybe String
      }

-- | Overall data for creation and update forms.
data FormData = FormData
  { title :: String,
    createUrl :: Maybe String,
    updateUrl :: Maybe String,
    fields :: [FieldData]
  }

-- | Helper function for generating forms
generateField :: FieldData -> Html
generateField fd = case fd of
  FieldData {} -> formField fd.name fd.label fd.type_ fd.value
  StaticData {} -> staticField fd.name fd.label fd.value

class GenerateForm a where
  newForm :: (MonadReader Env m) => Proxy a -> m FormData
  updateForm :: (MonadReader Env m) => a -> m FormData

--
-- What an Entry should look like in HTML
--
instance GenerateForm Entry where
  newForm _ =
    pure $
      FormData
        { title = "Create Entry",
          createUrl = pure $ T.unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthEntry (CRUDCreate EntryCreate)))),
          updateUrl = Nothing,
          fields =
            [ FieldData
                { label = "Title",
                  name = "title",
                  type_ = "text",
                  value = Nothing
                },
              FieldData
                { label = "Value",
                  name = "value",
                  type_ = "textarea",
                  value = Nothing
                }
            ]
        }
  updateForm entry = do
    tz <- asks timeZone
    pure $
      FormData
        { title = "Update Entry",
          createUrl = Nothing,
          updateUrl = pure $ T.unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthEntry (CRUDUpdate EntryUpdate EntryKey))) entry.key),
          fields =
            [ FieldData
                { label = "Title",
                  name = "title",
                  type_ = "text",
                  value = pure entry.title
                },
              StaticData
                { label = "Created",
                  name = "created",
                  value = pure $ entryTimeFormat tz entry.created
                },
              FieldData
                { label = "Value",
                  name = "value",
                  type_ = "textarea",
                  value = pure entry.value
                }
            ]
        }

instance GenerateForm User where
  newForm _ = pure $ FormData
    { title = "Create User",
    createUrl = pure $ T.unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthUser (CRUDCreate UserCreate)))),
          updateUrl = Nothing,
          fields =
            [ FieldData
                { label = "Email",
                  name = "email",
                  type_ = "text",
                  value = Nothing
                },
              FieldData
                { label = "Group",
                  name = "group",
                  type_ = "group",
                  value = Nothing
                }
            ]
        }
  updateForm user = do
    pure $
      FormData
        { title = "Update Entry",
          createUrl = Nothing,
          updateUrl = pure $ T.unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthUser (CRUDUpdate UserUpdate UserKey))) user.uuid),
          fields =
            [ FieldData
                { label = "Email",
                  name = "email",
                  type_ = "text",
                  value = pure $ T.unpack user.email
                },
              StaticData
                { label = "Group",
                  name = "group",
                  value = pure $ show user.group
                }
            ]
        }


-- | Generate an empty form for a given type. This form can be used to create a new value for the type.
generateNewForm :: (MonadReader Env m, GenerateForm a) => Proxy a -> m Html
generateNewForm p = do
  fd <- newForm p
  pure
    $ H.form
      ! HA.class_ "contentform"
      ! maybe mempty (\url -> HA.method "POST" <> HA.action (stringValue url)) fd.createUrl
    $ mconcat
      [ mconcat $ intersperse H.br $ generateField <$> fd.fields,
        H.br,
        H.input ! HA.class_ "formbutton" ! HA.type_ "submit" ! HA.value "Create"
      ]

-- | Generate a form for editing a given value. Fields will be pre-populated with existing values.
generateUpdateForm :: (MonadReader Env m, GenerateForm a) => a -> m Html
generateUpdateForm a = do
  fd <- updateForm a
  pure
    $ H.form
      ! HA.class_ "contentform"
      ! dataAttribute "hx-swap" "outerHTML"
      ! maybe mempty (dataAttribute "hx-put" . stringValue) fd.updateUrl
    $ mconcat
      [ mconcat $ intersperse H.br $ generateField <$> fd.fields,
        H.br,
        H.input
          ! HA.type_ "submit"
          ! HA.class_ "formbutton"
          ! HA.value "Update"
      ]


entryUpdateForm :: (MonadReader Env m) => Entry -> m Html
entryUpdateForm = generateUpdateForm

entryCreationForm :: (MonadReader Env m) => m Html
entryCreationForm =
  basicPage <$> generateNewForm (Proxy @Entry)

userUpdateForm :: MonadReader Env m => User -> m Html
userUpdateForm = generateUpdateForm

userCreationForm :: MonadReader Env m => m Html
userCreationForm = basicPage <$> generateNewForm (Proxy @User)

getEntryInitial :: MonadReader Env m => m H.Html
getEntryInitial = entryCreationForm


getEntryForUpdate :: (CanAppM Env Err m) => EntryKey -> m H.Html
getEntryForUpdate key = entryUpdateForm =<< Website.Data.Entry.getEntry key