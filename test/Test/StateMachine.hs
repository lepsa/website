{-# LANGUAGE QuasiQuotes #-}
-- This helps us make sure that our commands are all added to the state machine
{-# OPTIONS_GHC -Wunused-top-binds #-}

module Test.StateMachine where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson                            (eitherDecode)
import           Data.Base64.Types
import           Data.Bool
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Char8                 as BS8
import qualified Data.ByteString.Lazy                  as BSL
import qualified Data.ByteString.Lazy.Char8            as BSL8
import           Data.CaseInsensitive                  (mk)
import           Data.Char
import           Data.List
import qualified Data.Map                              as M
import           Data.Maybe
import           Data.Text                             (Text, unpack)
import           Data.Text.Encoding                    (encodeUtf8)
import qualified Data.Text.Encoding                    as T
import           Data.Text.Encoding.Base64             (encodeBase64)
import           GHC.Records
import           Hedgehog                              hiding (Group)
import qualified Hedgehog.Gen                          as Gen
import qualified Hedgehog.Range                        as Range
import qualified Network.HTTP.Client                   as H
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Types
import           Test.Db.File                          ()
import           Test.Types
import           Text.RawString.QQ
import           Text.Regex.TDFA
import           Web.FormUrlEncoded
import           Website.Auth.Authorisation            (Group (..))
import           Website.Data.Entry                    (Entry (title, value))
import           Website.Data.File                     (File (fileData, fileName, fileType))
import qualified Website.Data.User                     (User (..))
import           Website.Data.User                     (User)

-- Reference the following QFPL blog posts to refresh yourself.
-- https://qfpl.io/posts/intro-to-state-machine-testing-1/
-- https://qfpl.io/posts/intro-to-state-machine-testing-2/
-- https://qfpl.io/posts/intro-to-state-machine-testing-3/

--
-- Generators
--

genChars :: (MonadGen m) => m Char
genChars = Gen.filterT predicate Gen.ascii
  where
    predicate c = isPrint c && c /= '\n' -- isAlpha c || isNumber c

minTextLength, maxTextLength :: (Num a) => a
minTextLength = 1
maxTextLength = 10

textLength :: (Integral a) => Range a
textLength = Range.linear minTextLength maxTextLength

genText :: (MonadGen m) => m Text
genText = Gen.text textLength genChars

genEmail :: (MonadGen m) => m Text
genEmail = Gen.text textLength $ Gen.filterT (/= ':') genChars

genPassword :: (MonadGen m) => m Text
genPassword = genText

genGroup :: (MonadGen m) => m Group
genGroup = Gen.element [Admin, User]

genRegisterUser :: (MonadGen m) => m (RegisterUser v)
genRegisterUser = RegisterUser <$> genEmail <*> genPassword <*> genGroup

genPasswordUpdate :: (MonadGen m) => TestUser v -> m (Maybe (PasswordUpdate v))
genPasswordUpdate user = Gen.choice
  [ pure Nothing,
    Gen.filterT (/= user ^. tuPassword) genPassword <&> \newPass ->
      pure PasswordUpdate { _oldPassword = user ^. tuPassword, _newPassword = newPass }
  ]

--
-- Helper functions
--

mkReq' :: Method -> Header -> [Header] -> H.Request -> H.Request
mkReq' method accept headers req = req
  { H.method = method,
    H.redirectCount = 0,
    H.requestHeaders = accept : headers <> H.requestHeaders req
  }

mkReq :: Method -> [Header] -> H.Request -> H.Request
mkReq method = mkReq' method acceptHtml

emailExists :: (HasEmail r) => ApiState v -> r -> Bool
emailExists state command = any (\u -> u ^. email == command ^. email) state._users

userExists :: (HasAuth r, Ord1 v) => ApiState v -> r v -> Bool
userExists state command = case command ^. auth of
  Normal user -> case M.lookup (user ^. key) state._users of
    Just u -> u ^. tuEmail == user ^. akUser . tuEmail &&
              u ^. tuPassword == user ^. akUser . tuPassword &&
              u ^. tuGroup == user ^. akUser . tuGroup
    Nothing -> False
  _ -> True

userGroupAdmin :: (HasAuth r) => a -> r v -> Bool
userGroupAdmin _ = userGroup Admin

userGroupUser :: (HasAuth r) => a -> r v -> Bool
userGroupUser _ = userGroup User

userGroup :: (HasAuth r) => Group -> r v -> Bool
userGroup g command = case command ^. auth of
  Normal user -> user ^. akUser . tuGroup == g
  _           -> True

entryExists :: (HasKey r, Ord1 v) => ApiState v -> r v -> Bool
entryExists state command = M.member (command ^. key) state._entries

entryKeyExists :: (Ord1 v, HasField "key" command (Key v)) => ApiState v -> command -> Bool
entryKeyExists state command = M.member command.key state._entries

userKeyDoesntExist :: (Ord1 v, HasKey r) => ApiState v -> r v -> Bool
userKeyDoesntExist state input = M.notMember (input ^. key) state._users

entryKeyDoesntExist :: (HasKey r, Ord1 v) => ApiState v -> r v -> Bool
entryKeyDoesntExist state input = M.notMember (input ^. key) state._entries

response204 :: (HasField "responseStatus" r Status, MonadTest m) => ApiState v -> ApiState v -> a -> r -> m ()
response204 _old _new _input output = output.responseStatus === status204

mkAuthHeader :: (HasAuth command) => command Concrete -> [Header]
mkAuthHeader command = case command ^. auth of
  Normal (AuthKey _ u) -> pure $ maybe (mkBasicAuthHeader u) (mkBearerAuthHeader . concrete) $ u ^. tuJwt
  Bad user             -> badHeader $ view akUser <$> user
  None                 -> []
  where
    badHeader = maybe [] (pure . mkBasicAuthHeader)
    mkBearerAuthHeader token = ("Authorization", "Bearer " <> BS8.pack token)
    mkBasicAuthHeader user =
      ( "Authorization",
        "Basic " <> encodeUtf8 (extractBase64 @_ @Text $ encodeBase64 (user ^. email <> ":" <> user ^. password))
      )

mkGoodAuth :: (MonadGen m) => ApiState v -> m (Auth v)
mkGoodAuth state = Normal . uncurry AuthKey <$> Gen.element (M.toList state._users)

mkAuth :: (MonadGen m) => ApiState v -> m (Auth v)
mkAuth state = Gen.choice
  [ mkGoodAuth state
  , mkBadAuth state
  ]

mkKey :: MonadGen m => String -> [Key v] -> m (Key v)
mkKey base keys = Gen.choice
  [ Gen.element keys
  , mkBadKey base
  ]

genFileName :: MonadGen m => m Text
genFileName = Gen.text (Range.linear 1 10) $ Gen.filterT (\c -> c `notElem` ['/', '\\', ';']) genChars

genFileType :: MonadGen m => m Text
genFileType = do
  a <- Gen.text (Range.linear 1 10) Gen.alpha
  b <- Gen.text (Range.linear 1 10) Gen.alpha
  pure $ a <> "/" <> b

genUuid :: MonadGen m => m String
genUuid = do
  a <- Gen.string (Range.singleton 8) Gen.hexit
  b <- Gen.string (Range.singleton 4) Gen.hexit
  c <- Gen.string (Range.singleton 4) Gen.hexit
  d <- Gen.string (Range.singleton 4) Gen.hexit
  e <- Gen.string (Range.singleton 12) Gen.hexit
  pure $ a <> "-" <> b <> "-" <> c <> "-" <> d <> "-" <> e

mkBadKey :: MonadGen m => String -> m (Key v)
mkBadKey base = BadKey . (base <>) <$> genUuid

mkBadAuth :: (MonadGen m) => ApiState v -> m (Auth v)
mkBadAuth state = Gen.choice
  [ pure None
  , pure $ Bad Nothing
  , if M.null state._users
    then pure $ Bad Nothing
    else do
      (k, u') <- Gen.element $ M.toList state._users
      let u = u' & tuJwt .~ Nothing
      Bad . pure
        <$> Gen.choice
          [ do
              newPass <- Gen.filterT (/= u ^. password) genPassword
              pure $ AuthKey k $ u & tuPassword .~ newPass,
            do
              newEmail <- Gen.filterT (/= u ^. email) genEmail
              pure $ AuthKey k $ u & tuEmail .~ newEmail
          ]
  ]

formHeader :: Header
formHeader = ("Content-Type", "application/x-www-form-urlencoded")

acceptHtml :: Header
acceptHtml = ("Accept", "text/html")

extractJwt :: (MonadFail m, MonadTest m) => H.Response body -> m String
extractJwt = maybe
  (fail "Could not extract JWT value")
  (pure . takeWhile (/= ';') . drop 1 . dropWhile (/= '=') . BS8.unpack . snd)
  -- Find the JWT cookie
  . find (BS8.isPrefixOf "JWT" . snd)
  . findSetCookies
  . H.responseHeaders

findSetCookies :: [Header] -> [Header]
findSetCookies = filter (\(k, v) -> k == mk "Set-Cookie" && not (BS.null v))

extractKey :: (MonadFail m) => H.Response body -> m String
extractKey = maybe
  (fail "No Location header returned")
  (pure . BS8.unpack . snd)
  . find (\(h, _) -> h == "Location")
  . H.responseHeaders

keyString :: Key Concrete -> String
keyString (GoodKey s) = concrete s
keyString (BadKey s)  = s

--
-- Main api commands
--

type CanStateM gen m = (MonadGen gen, MonadFail m, MonadThrow m, MonadIO m, MonadTest m)

cRegister :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cRegister env = Command gen execute
  [ Require $ \state input -> not $ any (\u -> u ^. email == input ^. email) state._users,
    Require $ \state _input -> not $ any (\u -> u ^. tuGroup == Admin) state._users,
    Update $ \state input output -> state & users %~ M.insert (GoodKey output) (fromRegisterUser input),
    Ensure $ \oldState newState input _output -> do
      assert $ not $ any (\u -> u ^. email == input ^. email) oldState._users
      assert $ any (\u -> u ^. email == input ^. email) newState._users
  ]
  where
    gen _apiState = pure genRegisterUser
    execute register = do
      req <- H.parseRequest $ env.baseUrl <> "/register"
      let req' = mkReq methodPost [formHeader] req {H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm register}
      res <- liftIO $ H.httpNoBody req' env.manager
      res.responseStatus === status204
      extractKey res

cLogin :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cLogin env = Command gen execute
  [ Require $ \state input ->
      let f u = u ^. tuEmail == input ^. tlUser && u ^. tuPassword == input ^. tlPass
       in any f state._users,
    Update $ \state input output ->
      let f u = u & tuJwt .~ pure output
       in state & users %~ M.update (pure . f) (input ^. key)
  ]
  where
    -- From our state, generate a value to be used for the test.
    -- In this case, pick a user to log in from the list of users.
    gen apiState = if M.null apiState._users
      then Nothing
      else Just $ do
        (k, u) <- Gen.element $ M.toList apiState._users
        pure $ TestLogin k Good (u ^. tuEmail) (u ^. tuPassword)
    -- What we want to do with this value
    execute login = do
      req <- H.parseRequest $ env.baseUrl <> "/login"
      let req' = mkReq methodPost [formHeader] req {H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm login}
      res <- liftIO $ H.httpNoBody req' env.manager
      res.responseStatus === status303
      extractJwt res

cLoginFail :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cLoginFail env = Command gen execute []
  where
    gen apiState = if M.null apiState._users
      then Nothing
      else Just $ Gen.choice
        [ do
            (k, u) <- Gen.element $ M.toList apiState._users
            TestLogin k BadPassword (u ^. email)
              <$> Gen.filterT (\p -> p /= u ^. password) genPassword,
          do
            (k, u) <- Gen.element $ M.toList apiState._users
            TestLogin k BadUser
              <$> Gen.filterT (\e -> e /= u ^. email) genEmail
              <*> pure (u ^. password)
        ]
    execute login = do
      req <- H.parseRequest $ env.baseUrl <> "/login"
      let req' = mkReq methodPost [formHeader] req {H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm login}
      res <- liftIO $ H.httpNoBody req' env.manager
      res.responseStatus === status401

cGetEntries :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cGetEntries env = Command gen execute
  [ Require userExists,
    Ensure $ \_oldState newState _input output -> do
      output.responseStatus === status200
      -- Commit the great sin of "parsing" html with regex
      let matches :: [BSL8.ByteString] = getAllTextMatches (output.responseBody =~ ([r|"/entry/[[:alnum:]]{8}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{12}"|] :: String))
      length newState._entries === length matches
  ]
  where
    gen state = if M.null state._users
      then Just $ pure $ GetEntries None
      else Just $ GetEntries <$> mkAuth state
    execute getEntries = do
      req <- H.parseRequest $ env.baseUrl <> "/entries"
      let req' = mkReq methodGet (mkAuthHeader getEntries) req
      liftIO $ H.httpLbs req' env.manager

cGetEntry :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cGetEntry env = Command gen execute
  [ Require userExists,
    Require $ \state input -> case input ^. key of
      k@(GoodKey _) -> M.member k $ state ^. files
      BadKey _      -> True,
    Ensure $ \old _new input output -> do
      let keyStatus = case input ^. key of
            GoodKey _ -> output.responseStatus === status200
            BadKey _  -> output.responseStatus === status404
      case input ^. auth of
        Normal (AuthKey k u) -> case old ^? users . ix k of
          Nothing -> fail "User key doesn't exist in old state"
          Just user -> do
            user === u
            keyStatus
        _ -> keyStatus
  ]
  where
    gen state = pure $ GetEntry
      <$> bool (mkKey "/entry/" $ M.keys state._entries) (mkBadKey "/entry/") (M.null state._entries)
      <*> bool (mkAuth state) (mkBadAuth state) (M.null state._users)
    execute getEntry = do
      req <- H.parseRequest $ env.baseUrl <> keyString (getEntry ^. key)
      let req' = mkReq methodGet (mkAuthHeader getEntry) req
      liftIO $ H.httpLbs req' env.manager

cCreateEntry :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cCreateEntry env = Command gen execute
  [ Require userExists
  , Update $ \state input output -> state
      & entries %~ M.insert (GoodKey output) (TestEntry (input ^. ceTitle) (input ^. ceValue)),
    Ensure $ \_old _new _input output -> do
      assert $ isPrefixOf "/entry/" output
      assert $ length output > length ("/entry/" :: String)
  ]
  where
    gen state = if M.null state._users
      then Nothing
      else pure $ CreateEntry <$> mkGoodAuth state <*> genText <*> genText
    execute createEntry = do
      req <- H.parseRequest (env.baseUrl <> "/entry")
      let req' = mkReq
            methodPost
            (formHeader : mkAuthHeader createEntry)
            req
              { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm createEntry
              }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status303
      extractKey res

cCreateEntryBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cCreateEntryBadAuth env = Command gen execute []
  where
    gen state = if M.null state._users
      then Nothing
      else pure $ CreateEntry <$> mkBadAuth state <*> genText <*> genText
    execute createEntry = do
      req <- H.parseRequest $ env.baseUrl <> "/entry"
      let req' = mkReq
            methodPost
            (formHeader : mkAuthHeader createEntry)
            req
              { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm createEntry
              }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cDeleteEntry :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cDeleteEntry env = Command gen execute
  [ Require userExists,
    Require userGroupAdmin,
    Update $ \state input _output -> case input ^. auth of
      Normal _ -> case input ^. key of
        GoodKey _ -> state & entries %~ M.delete (input ^. key)
        _         -> state
      _ -> state,
    Ensure $ \old new input output -> case input ^. auth of
      Normal _ -> do
        output.responseStatus === status200
        case input ^. key of
          GoodKey _ -> length (old ^. entries) === length (new ^. entries) + 1
          _         -> length (old ^. entries) === length (new ^. entries)
      _ -> do
        output.responseStatus === status401
        length (old ^. entries) === length (new ^. entries)
  ]
  where
    gen state = pure $ DeleteEntry
      <$> bool (mkKey "/entry/" $ M.keys state._entries) (mkBadKey "/entry/") (M.null state._entries)
      <*> bool (mkAuth state) (mkBadAuth state) (M.null state._users)
    execute deleteEntry = do
      req <- H.parseRequest $ env.baseUrl <> keyString (deleteEntry ^. key) <> "/delete"
      let req' = mkReq methodDelete (mkAuthHeader deleteEntry) req
      liftIO $ H.httpLbs req' env.manager

cUpdateEntry :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cUpdateEntry env = Command gen execute
  [ Update $ \state input _output ->
      state
        & entries %~ M.update (pure . updateEntry input) (input ^. key)
  ]
  where
    updateEntry input u = u
      & teTitle .~ input ^. ueTitle
      & teValue .~ input ^. ueValue
    gen state = if not (M.null state._users) && not (M.null state._entries)
      then Just $ UpdateEntry
        <$> mkGoodAuth state
        <*> Gen.element (M.keys state._entries)
        <*> genText
        <*> genText
      else Nothing
    execute input = do
      req <- H.parseRequest $ env.baseUrl <> keyString (input ^. key) <> "/update"
      let req' = mkReq methodPut (formHeader : mkAuthHeader input)
              req { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm input }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200

cUpdateEntryBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cUpdateEntryBadAuth env = Command gen execute []
  where
    gen state = if not (M.null state._users) && not (M.null state._entries)
      then Just $ UpdateEntry
        <$> mkBadAuth state
        <*> Gen.element (M.keys state._entries)
        <*> genText
        <*> genText
      else Nothing
    execute updateEntry = do
      req <- H.parseRequest $ env.baseUrl <> keyString (updateEntry ^. key) <> "/update"
      let req' = mkReq methodPut (formHeader : mkAuthHeader updateEntry)
              req { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm updateEntry }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cCreateUser :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cCreateUser env = Command gen execute
  [ Require userExists,
    Require userGroupAdmin,
    Require $ \state input -> all (\u -> u ^. tuEmail /= input ^. cuEmail) (state ^. users),
    Update $ \state input output ->
      state
        & users %~ M.insert (GoodKey output) (TestUser (input ^. email) (input ^. password) (input ^. cuGroup) Nothing),
    Ensure $ \old new input _output -> do
      length old._users + 1 === length new._users
      assert $ any (\u -> u ^. email == input ^. email) new._users
  ]
  where
    gen state = if M.null state._users
      then Nothing
      else Just $ CreateUser
        <$> mkGoodAuth state
        <*> genGroup
        <*> Gen.filterT (\e -> not $ any (\u -> u ^. email == e) state._users) genEmail
        <*> genPassword
    execute cUser = do
      req <- H.parseRequest $ env.baseUrl <> "/user"
      let req' = mkReq methodPost (formHeader : mkAuthHeader cUser)
              req { H.requestBody = H.RequestBodyLBS $ urlEncodeAsForm $ toForm cUser }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status303
      extractKey res

cCreateUserBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cCreateUserBadAuth env = Command gen execute []
  where
    gen state = if M.null state._users
      then Nothing
      else Just $ CreateUser
        <$> mkBadAuth state
        <*> genGroup
        <*> Gen.filterT (\e -> not $ any (\u -> u ^. email == e) state._users) genEmail
        <*> genPassword
    execute cUser = do
      req <- H.parseRequest $ env.baseUrl <> "/user"
      let req' = mkReq methodPost (formHeader : mkAuthHeader cUser)
            req { H.requestBody = H.RequestBodyLBS $ urlEncodeAsForm $ toForm cUser }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cUpdateUser :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cUpdateUser env = Command gen execute
  [ Require userExists,
    Require userGroupAdmin,
    Update $ \state input _output ->
      let update u = u
            & tuPassword .~ maybe (u ^. tuPassword) (view newPassword) (input ^. uuPassword)
            & tuGroup .~ fromMaybe (u ^. tuGroup) (input ^. uuGroup)
      in state & users %~ M.update (pure . update) (input ^. key)
  ]
  where
    gen state = if M.null state._users
      then Nothing
      else Just $ do
        (k, u) <- Gen.element $ M.toList state._users
        UpdateUser k
          <$> genPasswordUpdate u
          <*> fmap pure genGroup
          <*> mkGoodAuth state
    execute update = do
      req <- H.parseRequest $ env.baseUrl <> keyString (update ^. key) <> "/update"
      let req' = mkReq methodPut (formHeader : mkAuthHeader update)
            req { H.requestBody = H.RequestBodyLBS $ urlEncodeAsForm $ toForm update }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200

cUpdateUserBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cUpdateUserBadAuth env = Command gen execute []
  where
    gen state = if M.null state._users
      then Nothing
      else Just $ do
        (k, u) <- Gen.element $ M.toList state._users
        UpdateUser k
          <$> genPasswordUpdate u
          <*> fmap pure genGroup
          <*> mkBadAuth state
    execute update = do
      req <- H.parseRequest $ env.baseUrl <> keyString (update ^. key) <> "/update"
      let req' = mkReq methodPut (formHeader : mkAuthHeader update)
            req { H.requestBody = H.RequestBodyLBS $ urlEncodeAsForm $ toForm update }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cDeleteUser :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cDeleteUser env = Command gen execute
  [ Require userExists,
    Require userGroupAdmin,
    Update $ \state input _output -> case input ^. auth of
      Normal _ -> state & users %~ M.delete (input ^. key)
      _        -> state,
    Ensure $ \old new input output -> case input ^. auth of
      Normal _ -> do
        length old._users === length new._users + 1
        assert $ not $ M.member (input ^. key) new._users
        assert $ M.member (input ^. key) old._users
      _ -> output.responseStatus === status401
  ]
  where
    gen state = if M.null state._users
      then Nothing
      else Just $ DeleteUser <$> mkAuth state <*> Gen.element (M.keys state._users)
    execute update = do
      req <- H.parseRequest (env.baseUrl <> keyString (update ^. key) <> "/delete")
        <&> mkReq methodDelete (formHeader : mkAuthHeader update)
      liftIO $ H.httpLbs req env.manager

cGetUser :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cGetUser env = Command gen execute
  [ Require $ \state input -> case input ^. auth of
      Normal _ ->
        userExists state input
          && userGroupAdmin state input
      Bad (Just (AuthKey _ u)) ->
        all (\u' -> u ^. tuEmail == u' ^. tuEmail && u ^. tuPassword == u' ^. tuPassword) $
          state ^. users
      _ -> True,
    Ensure $ \_old _new input output -> case input ^. auth of
      Normal _ -> do
        output.responseStatus === status200
        output.responseBody /== mempty
      _ -> output.responseStatus === status401
  ]
  where
    gen state =
      let adminUsers = state ^.. users . to M.toList . each . filtered (\(_, u) -> u ^. tuGroup == Admin)
      in if null adminUsers
        then Nothing
        else Just $ do
          (k, u) <- Gen.element adminUsers
          k' <- Gen.element $ M.keys state._users
          let f = flip GetUser k'
          Gen.choice
            [ pure $ f $ Normal $ AuthKey k u,
              f <$> mkBadAuth state
            ]
    execute getUser = do
      req <- H.parseRequest $ env.baseUrl <> keyString (getUser ^. key)
      let req' = mkReq methodGet (mkAuthHeader getUser) req
      liftIO $ H.httpLbs req' env.manager

cCreateFile :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cCreateFile env = Command gen execute
  [ Require userExists,
    Require userGroupAdmin,
    Update $ \state input output -> state
      & files %~ M.insert
        (GoodKey output)
        (TestFile (input ^. cfName) (input ^. cfType) (input ^. cfData)),
    Ensure $ \old new _input output -> do
      assert $ isPrefixOf "/file/" output
      assert $ length output > length ("/file/" :: String)
      length old._files + 1 === length new._files
  ]
  where
    gen state = if M.null state._users
      then Nothing
      else Just $ CreateFile
        <$> mkGoodAuth state
        <*> genFileName
        <*> genFileType
        <*> fmap BSL.fromStrict (Gen.bytes textLength)
    execute tfUpload = do
      req <- H.parseRequest $ env.baseUrl <> "/file"
      let body = partFileRequestBody "file" (unpack $ tfUpload ^. cfName) $ H.RequestBodyLBS $ tfUpload ^. cfData
      req' <- formDataBody [ body { partContentType = pure $ tfUpload ^. cfType . to T.encodeUtf8 } ] $
        mkReq methodPost (formHeader : mkAuthHeader tfUpload) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status201
      extractKey res

cCreateFileBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cCreateFileBadAuth env = Command gen execute []
  where
    gen state = if M.null state._users
      then Just $ CreateFile None
        <$> genFileName
        <*> genFileType
        <*> fmap BSL.fromStrict (Gen.bytes textLength)
      else Just $ CreateFile
        <$> mkBadAuth state
        <*> genFileName
        <*> genFileType
        <*> fmap BSL.fromStrict (Gen.bytes textLength)
    execute tfUpload = do
      req <- H.parseRequest (env.baseUrl <> "/file") >>= formDataBody
          [ partFileRequestBody "file" (unpack $ tfUpload ^. cfName) $ H.RequestBodyLBS $ tfUpload ^. cfData
          ] . mkReq
          methodPost
          (formHeader : mkAuthHeader tfUpload)
      res <- liftIO $ H.httpLbs req env.manager
      res.responseStatus === status401

cGetFile :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cGetFile env = Command gen execute
  [ Require userExists,
    Require $ \state (GetFile k _) -> isJust $ M.lookup k $ state ^. files,
    Ensure $ \old _new input output -> case input ^. auth of
      Normal (AuthKey _ _) -> do
        output.responseStatus === status200
        case M.lookup (input ^. key) old._files of
          Nothing -> fail "File doesn't exist in local state"
          Just f  -> f ^. tfData === output.responseBody
      _ -> output.responseStatus === status200
  ]
  where
    gen state = if M.null state._users || M.null state._files
      then Nothing
      else pure $ GetFile
        <$> Gen.element (M.keys state._files)
        <*> Gen.choice [mkGoodAuth state, mkBadAuth state]
    execute getFile = do
      req <- H.parseRequest $ env.baseUrl <> keyString (getFile ^. key)
      liftIO $ H.httpLbs (mkReq' methodGet ("Accept", "*/*") (mkAuthHeader getFile) req) env.manager

cDeleteFile :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cDeleteFile env = Command gen execute
  [ Require userExists,
    Require userGroupAdmin,
    Require $ \state (DeleteFile k _) -> isJust $ M.lookup k $ state ^. files,
    Update $ \state input _output -> case input ^. auth of
      Normal _ -> state & files %~ M.delete (input ^. dfKey)
      _        -> state,
    Ensure $ \old new input output -> case input ^. dfAuth of
      Normal (AuthKey _ _) -> do
        output.responseStatus === status200
        length (old ^. files) === length (new ^. files) + 1
      _ -> do
        output.responseStatus === status401
        length (old ^. files) === length (new ^. files)
  ]
  where
    gen state = if M.null state._users || M.null state._files
      then Nothing
      else pure $ DeleteFile
        <$> Gen.element (M.keys state._files)
        <*> mkAuth state
    execute getFile = do
      req <- H.parseRequest $ env.baseUrl <> keyString (getFile ^. key) <> "/delete"
      liftIO $ H.httpLbs (mkReq methodDelete (mkAuthHeader getFile) req) env.manager

--
-- Side channel commands.
-- These are used to check that the server is doing what we expect in
-- multiple ways. Things like checking what the DB thinks the world
-- looks like vs what the API thinks the world looks like.
--

cTestGetUsers :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cTestGetUsers env = Command gen execute
  [ Ensure $ \_oldState newState _input output ->
    let stateEmails = sort (M.elems $ view email <$> newState._users)
        apiEmails = sort ((.email) <$> output)
     in stateEmails === apiEmails
  ]
  where
    gen _ = Just $ pure GetTestUsers
    execute _ = do
      req <- H.parseRequest (env.baseUrl <> "/getUsers")
      let req' = req {H.method = methodGet}
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200
      either fail pure $ eitherDecode @[User] res.responseBody

cTestGetEntries :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cTestGetEntries env = Command gen execute
  [ Ensure $ \_oldState newState _input output ->
    let stateEntries = sort $ (\e -> (e ^. teTitle, e ^. teValue)) <$> M.elems newState._entries
        apiEntries = sort $ (\e -> (title e, value e)) <$> output
     in stateEntries === apiEntries
  ]
  where
    gen _ = Just $ pure GetTestEntries
    execute _ = do
      req <- H.parseRequest (env.baseUrl <> "/getEntries")
      let req' = req {H.method = methodGet}
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200
      either fail pure $ eitherDecode @[Entry] res.responseBody

cTestGetFiles :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cTestGetFiles env = Command gen execute
  [ Ensure $ \_oldState newState _input output ->
    let stateFiles = sort $ (\f -> (f ^. tfName, f ^. tfType, f ^. tfData)) <$> M.elems newState._files
        apiFiles = sort $ (\f -> (fileName f, fileType f, fileData f)) <$> output
     in stateFiles === apiFiles
  ]
  where
    gen _ = Just $ pure GetTestFiles
    execute _ = do
      req <- H.parseRequest (env.baseUrl <> "/getFiles")
      let req' = req {H.method = methodGet}
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200
      either fail pure $ eitherDecode @[File] res.responseBody

--
-- The prop test machine
--

propApiTests :: TestEnv -> IO Bool -> Property
propApiTests env reset = withTests 100 . property $ do
  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState commands
  -- Once we have our set of actions, reset the API and begin to test it.
  -- The reset has to be performed here, not above `actions <- forAll...`
  -- otherwise it seems to only run for the first test, but any shrinks
  -- or changes don't get a fresh DB.
  worked' <- evalIO reset
  if worked' then pure () else fail "Could not reset the API, again"
  executeSequential initialState actions
  where
    commands =
      [ cRegister
      , cLogin
      , cLoginFail
      -- Side channels
      , cTestGetUsers
      , cTestGetEntries
      , cTestGetFiles
      -- Entry commands
      , cGetEntry
      , cGetEntries
      , cCreateEntry
      , cCreateEntryBadAuth
      , cDeleteEntry
      , cUpdateEntry
      , cUpdateEntryBadAuth
      -- User commands
      , cCreateUser
      , cCreateUserBadAuth
      , cUpdateUser
      , cUpdateUserBadAuth
      , cDeleteUser
      , cGetUser
      -- File commands
      , cCreateFile
      , cCreateFileBadAuth
      , cGetFile
      , cDeleteFile
      ] ?? env
