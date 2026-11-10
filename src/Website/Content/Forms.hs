{-# LANGUAGE FunctionalDependencies #-}
module Website.Content.Forms where

import           Control.Monad.Logger        (MonadLogger, logDebug)
import           Control.Monad.Reader        (MonadReader, asks)
import           Data.List                   (intersperse)
import           Data.Text                   (show)
import qualified Data.Text                   as T
import           Prelude                     (Bounded, Enum, Eq,
                                              Maybe (Nothing), Show, String,
                                              maybe, mconcat, mempty, pure, ($),
                                              (.), (<$>), (<>), (=<<))
import           Servant                     (Proxy (Proxy),
                                              ToHttpApiData (toUrlPiece),
                                              safeLink)
import           Text.Blaze.Html             (Html, stringValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Website.Auth.Authorisation  (Access (Read, Write), Group)
import           Website.Content.Common      (AuthEntry, AuthFile, AuthUser,
                                              basicPage, formField, selectField,
                                              staticField)
import           Website.Content.Htmx        (hxBoost, hxIndicator, hxOn,
                                              hxPost, hxPut, hxSwap)
import           Website.Data.Entry          (Entry (created, key, title, updated, value),
                                              EntryCreate, EntryKey,
                                              EntryUpdate, getEntry)
import           Website.Data.Env            (HasEnv (timeZone))
import           Website.Data.Permission     (checkPermission)
import           Website.Data.User           (RequiredUser,
                                              User (email, group, uuid),
                                              UserCreate, UserKey, UserUpdate,
                                              getUser)
import           Website.Data.Util           (timeFormat)
import           Website.Network.API.CRUD    (CRUDCreate, CRUDCreateFile,
                                              CRUDUpdate)
import           Website.Network.API.Types   (FileUpload, topAPI)
import           Website.Types               (CanAppM)

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
  FieldData {}  -> formField fd.fieldName fd.fieldLabel fd.fieldType fd.fieldValue
  StaticData {} -> staticField fd.staticName fd.staticLabel fd.staticValue
  SelectData {} -> selectField fd.selectName fd.selectLabel fd.selectValue

class (Show b, Bounded b, Enum b, Eq b) => GenerateForm a b | a -> b where
  newForm :: (HasEnv c, MonadReader c m, MonadLogger m) => Proxy a -> m (FormData b)
  updateForm :: (HasEnv c, MonadReader c m, MonadLogger m) => a -> m (FormData b)

--
-- What an Entry should look like in HTML
--
instance GenerateForm Entry () where
  newForm _ = do
    $(logDebug) "newForm Entry"
    pure $ FormData
      { title = "Create Entry",
        createUrl = pure $ T.unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthEntry (Website.Network.API.CRUD.CRUDCreate EntryCreate)))),
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
    $(logDebug) $ "updateForm Entry " <> show entry
    tz <- asks timeZone
    pure $ FormData
      { title = "Update Entry",
        createUrl = Nothing,
        updateUrl = pure $ T.unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthEntry (Website.Network.API.CRUD.CRUDUpdate EntryUpdate EntryKey))) entry.key),
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
  newForm _ = do
    $(logDebug) "newForm User"
    pure $ FormData
      { title = "Create User",
        createUrl = pure $ T.unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthUser (Website.Network.API.CRUD.CRUDCreate UserCreate)))),
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
    $(logDebug) $ "updateForm User: " <> show user
    pure $ FormData
      { title = "Update User",
        createUrl = Nothing,
        updateUrl = pure $ T.unpack $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthUser (Website.Network.API.CRUD.CRUDUpdate UserUpdate UserKey))) user.uuid),
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
generateNewForm :: (HasEnv c, MonadReader c m, GenerateForm a b, MonadLogger m) => Proxy a -> m Html
generateNewForm p = do
  $(logDebug) "generateNewForm"
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
generateUpdateForm :: (Show a, HasEnv c, MonadReader c m, MonadLogger m, GenerateForm a b) => a -> m Html
generateUpdateForm a = do
  $(logDebug) $ "generateUpdateForm: " <> show a
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
userUpdateForm :: (HasEnv c, MonadReader c m, MonadLogger m) => User -> m Html
userUpdateForm u = do
  ($logDebug) $ "userUpdateForm " <> show u
  generateUpdateForm u

userCreationForm :: (RequiredUser c, CanAppM c e m) => m Html
userCreationForm = do
  $(logDebug) "userCreationForm"
  checkPermission "GET new user" Read
  basicPage =<< generateNewForm (Proxy @User)

getUserForUpdate :: (RequiredUser c, CanAppM c e m) => UserKey -> m H.Html
getUserForUpdate userKey = do
  $(logDebug) $ "getUserForUpdate " <> show userKey
  checkPermission "GET update user" Read
  userUpdateForm =<< Website.Data.User.getUser userKey

-- Entry forms
entryUpdateForm :: (RequiredUser c, HasEnv c, MonadReader c m, MonadLogger m) => Entry -> m Html
entryUpdateForm e = do
  $(logDebug) $ "entryUpdateForm " <> show e
  generateUpdateForm e

entryCreationForm :: (RequiredUser c, CanAppM c e m) => m Html
entryCreationForm = do
  $(logDebug) "entryCreationForm"
  checkPermission "GET new entry" Write
  basicPage =<< generateNewForm (Proxy @Entry)

getEntryForUpdate :: (RequiredUser c, CanAppM c e m) => EntryKey -> m H.Html
getEntryForUpdate entry = do
  $(logDebug) $ "getEntryForUpdate " <> show entry
  checkPermission "GET update entry" Read
  entryUpdateForm =<< Website.Data.Entry.getEntry entry

-- File form
fileCreationForm :: (RequiredUser c, CanAppM c e m) => m Html
fileCreationForm = do
  $(logDebug) "fileCreationForm"
  checkPermission "GET new file" Write
  basicPage $ mconcat
    [ H.form
      -- ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! HA.enctype "multipart/form-data"
      ! hxPost (H.textValue $ "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthFile (Website.Network.API.CRUD.CRUDCreateFile FileUpload)))))
      ! hxIndicator "#spinner"
      $ mconcat
      [ H.p $ H.label
        ! HA.for "file"
        $ "Choose a file for upload"
      , H.p $ H.input
        ! HA.type_ "file"
        ! HA.name "file"
      , H.p $ H.button
        ! HA.type_ "submit"
        $ "Upload"
      , H.img
        ! HA.id "spinner"
        ! HA.class_ "htmx-indicator"
        ! HA.src "/static/90-ring.svg"
      ]
    ]
