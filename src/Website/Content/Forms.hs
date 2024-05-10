{-# LANGUAGE FunctionalDependencies #-}
module Website.Content.Forms where

import           Control.Monad.Reader
import           Data.Data
import           Data.List
import qualified Data.Text                   as T
import           Servant
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Website.Auth.Authorisation  (Access (Read, Write), Group)
import           Website.Content.Common
import           Website.Content.Htmx
import           Website.Data.Entry
import           Website.Data.Env
import           Website.Data.Permission
import           Website.Data.User
import           Website.Data.Util
import           Website.Network.API.CRUD
import           Website.Network.API.Types
import           Website.Types

-- | Values for a given form field
data FieldData a
  = FieldData
      { fieldLabel :: String,
        fieldName  :: String,
        fieldType  :: String,
        fieldValue :: Maybe String
      }
  | StaticData
      { staticLabel :: String,
        staticName  :: String,
        staticValue :: Maybe String
      }
  | SelectData
      { selectLabel :: String
      , selectName  :: String
      , selectValue :: Maybe a
      }

-- | Overall data for creation and update forms.
data FormData a = FormData
  { title     :: String,
    createUrl :: Maybe String,
    updateUrl :: Maybe String,
    fields    :: [FieldData a]
  }

-- | Helper function for generating forms
generateField :: (Show a, Eq a, Enum a, Bounded a) => FieldData a -> Html
generateField fd = case fd of
  FieldData {} -> formField fd.fieldName fd.fieldLabel fd.fieldType fd.fieldValue
  StaticData {} -> staticField fd.staticName fd.staticLabel fd.staticValue
  SelectData {} -> selectField fd.selectName fd.selectLabel fd.selectValue

class (Show b, Bounded b, Enum b, Eq b) => GenerateForm a b | a -> b where
  newForm :: (HasEnv c, MonadReader c m) => Proxy a -> m (FormData b)
  updateForm :: (HasEnv c, MonadReader c m) => a -> m (FormData b)

--
-- What an Entry should look like in HTML
--
instance GenerateForm Entry () where
  newForm _ =
    pure $
      FormData
        { title = "Create Entry",
          createUrl = pure $ T.unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthEntry (CRUDCreate EntryCreate)))),
          updateUrl = Nothing,
          fields =
            [ FieldData
                { fieldLabel = "Title",
                  fieldName = "title",
                  fieldType = "text",
                  fieldValue = Nothing
                },
              FieldData
                { fieldLabel = "Value",
                  fieldName = "value",
                  fieldType = "textarea",
                  fieldValue = Nothing
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
                { fieldLabel = "Title",
                  fieldName = "title",
                  fieldType = "text",
                  fieldValue = pure $ T.unpack entry.title
                },
              StaticData
                { staticLabel = "Created",
                  staticName = "created",
                  staticValue = pure $ timeFormat tz entry.created
                },
              StaticData
                { staticLabel = "Last Updated",
                  staticName = "created",
                  staticValue = pure $ maybe "Never" (timeFormat tz) entry.updated
                },
              FieldData
                { fieldLabel = "Value",
                  fieldName = "value",
                  fieldType = "textarea",
                  fieldValue = pure $ T.unpack entry.value
                }
            ]
        }

instance GenerateForm User Group where
  newForm _ =
    pure $
      FormData
        { title = "Create User",
          createUrl = pure $ T.unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthUser (CRUDCreate UserCreate)))),
          updateUrl = Nothing,
          fields =
            [ FieldData
                { fieldLabel = "Email",
                  fieldName = "email",
                  fieldType = "text",
                  fieldValue = Nothing
                },
              FieldData
                { fieldLabel = "Password",
                  fieldName = "password",
                  fieldType = "password",
                  fieldValue = Nothing
                },
              SelectData
                { selectLabel = "Group",
                  selectName = "group",
                  selectValue = Nothing
                }
            ]
        }
  updateForm user = do
    pure $
      FormData
        { title = "Update User",
          createUrl = Nothing,
          updateUrl = pure $ T.unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthUser (CRUDUpdate UserUpdate UserKey))) user.uuid),
          fields =
            [ StaticData
                { staticLabel = "Email",
                  staticName = "email",
                  staticValue = pure $ T.unpack user.email
                },
              FieldData
                { fieldLabel = "Old Password",
                  fieldName = "oldPassword",
                  fieldType = "password",
                  fieldValue = Nothing
                },
              FieldData
                { fieldLabel = "New Password",
                  fieldName = "newPassword",
                  fieldType = "password",
                  fieldValue = Nothing
                },
              SelectData
                { selectLabel = "Group",
                  selectName = "group",
                  selectValue = pure user.group
                }
            ]
        }

-- | Generate an empty form for a given type. This form can be used to create a new value for the type.
generateNewForm :: (HasEnv c, MonadReader c m, GenerateForm a b) => Proxy a -> m Html
generateNewForm p = do
  fd <- newForm p
  pure $ mconcat
    [ H.h2 $ H.toHtml fd.title
    , H.form
      ! HA.class_ "contentform"
      ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! maybe mempty (\url -> HA.method "POST" <> HA.action (stringValue url)) fd.createUrl
    $ mconcat
      [ mconcat $ intersperse H.br $ generateField <$> fd.fields,
        H.br,
        H.input
          ! HA.class_ "formbutton"
          ! HA.type_ "submit"
          ! HA.value "Create"
      ]
    ]

-- | Generate a form for editing a given value. Fields will be pre-populated with existing values.
generateUpdateForm :: (HasEnv c, MonadReader c m, GenerateForm a b) => a -> m Html
generateUpdateForm a = do
  fd <- updateForm a
  pure $ mconcat
    [ H.h2 $ H.toHtml fd.title
    , H.form
      ! HA.class_ "contentform"
      ! hxSwap "outerHTML"
      ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! maybe mempty (hxPut . stringValue) fd.updateUrl
    $ mconcat
      [ mconcat $ intersperse H.br $ generateField <$> fd.fields,
        H.br,
        H.input
          ! HA.type_ "submit"
          ! HA.class_ "formbutton"
          ! HA.value "Update"
      ]
    ]

-- User forms
userUpdateForm :: (HasEnv c, MonadReader c m) => User -> m Html
userUpdateForm user = do
  generateUpdateForm user

userCreationForm :: (RequiredUser c, CanAppM c e m) => m Html
userCreationForm = do
  user <- asks auth
  checkPermission user "GET new user" Read
  basicPage =<< generateNewForm (Proxy @User)

getUserForUpdate :: (RequiredUser c, CanAppM c e m) => UserKey -> m H.Html
getUserForUpdate userKey = do
  user <- asks auth
  checkPermission user "GET update user" Read
  userUpdateForm =<< Website.Data.User.getUser userKey

-- Entry forms
entryUpdateForm :: (RequiredUser c, HasEnv c, MonadReader c m) => Entry -> m Html
entryUpdateForm = generateUpdateForm

entryCreationForm :: (RequiredUser c, CanAppM c e m) => m Html
entryCreationForm = do
  user <- asks auth
  checkPermission user "GET new entry" Write
  basicPage =<< generateNewForm (Proxy @Entry)

getEntryForUpdate :: (RequiredUser c, CanAppM c e m) => EntryKey -> m H.Html
getEntryForUpdate entry = do
  user <- asks auth
  checkPermission user "GET update entry" Read
  entryUpdateForm =<< Website.Data.Entry.getEntry entry
