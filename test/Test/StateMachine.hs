{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
-- This helps us make sure that our commands are all added to the state machine
{-# OPTIONS_GHC -Wunused-top-binds #-}

module Test.StateMachine where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson (eitherDecode)
import Data.Base64.Types
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.CaseInsensitive (mk)
import Data.Char
import Data.List (find, isPrefixOf, sort)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Base64 qualified as B64
import GHC.Records
import Hedgehog hiding (Group)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client qualified as H
import Network.HTTP.Types (Header, Method, Status, methodDelete, methodGet, methodPost, methodPut, status200, status204, status303, status401, status404)
import Test.Types
import Text.RawString.QQ
import Text.Regex.TDFA
import Web.FormUrlEncoded
import Website.Auth.Authorisation ()
import Website.Auth.Authorisation qualified as Auth
import Website.Data.User (User)
import Website.Data.User qualified (User (..))

-- Reference the following QFPL blog posts to refresh yourself.
-- https://qfpl.io/posts/intro-to-state-machine-testing-1/
-- https://qfpl.io/posts/intro-to-state-machine-testing-2/
-- https://qfpl.io/posts/intro-to-state-machine-testing-3/

--
-- Generators
--

chars :: (MonadGen m) => m Char
chars = Gen.filterT predicate Gen.ascii
  where
    predicate c = isPrint c && c /= '\n' -- isAlpha c || isNumber c

minTextLength, maxTextLength :: (Num a) => a
minTextLength = 1
maxTextLength = 10

textLength :: (Integral a) => Range a
textLength = Range.linear minTextLength maxTextLength

genText :: (MonadGen m) => m Text
genText = Gen.text textLength chars

genEmail :: (MonadGen m) => m Text
genEmail = Gen.text textLength $ Gen.filterT (/= ':') $ Gen.filterT isAlpha chars

genPassword :: (MonadGen m) => m Text
genPassword = Gen.text textLength $ Gen.filterT (/= ':') chars

genGroup :: (MonadGen m) => m Auth.Group
genGroup = Gen.element [Auth.Admin, Auth.User]

genRegisterUser :: (MonadGen m) => m (RegisterUser v)
genRegisterUser = RegisterUser <$> genEmail <*> genPassword <*> genGroup

genPasswordUpdate :: (MonadGen m) => TestUser v -> m (Maybe (PasswordUpdate v))
genPasswordUpdate user = do
  Gen.choice
    [ pure Nothing,
      do
        newPass <- Gen.filterT (/= user ^. tuPassword) genPassword
        pure $
          pure
            PasswordUpdate
              { _oldPassword = user ^. tuPassword,
                _newPassword = newPass
              }
    ]

--
-- Helper functions
--

mkReq :: Method -> [Header] -> H.Request -> H.Request
mkReq method headers req =
  req
    { H.method = method,
      H.redirectCount = 0,
      H.requestHeaders = acceptHtml : headers <> H.requestHeaders req
    }

emailExists :: (HasEmail r) => ApiState v -> r -> Bool
emailExists state command = any (\u -> u ^. email == command ^. email) state._users

userExists :: (HasAuth r, Ord1 v) => ApiState v -> r v -> Bool
userExists state command = case command ^. auth of
  Normal user -> M.member (user ^. key) state._users
  Bad _ -> True

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
  Normal user ->
    let u = user ^. akUser
    in pure $ maybe (mkBasicAuthHeader u) (mkBearerAuthHeader . concrete) $ u ^. tuJwt
  Bad user -> badHeader $ view akUser <$> user
  where
    badHeader :: Maybe (TestUser Concrete) -> [Header]
    badHeader = maybe [] (pure . mkBasicAuthHeader)
    mkBearerAuthHeader :: String -> Header
    mkBearerAuthHeader token =
      ( "Authorization",
        "Bearer " <> BS8.pack token
      )
    mkBasicAuthHeader :: TestUser v -> Header
    mkBasicAuthHeader user =
      ( "Authorization",
        "Basic " <> encodeUtf8 (extractBase64 @_ @T.Text $ B64.encodeBase64 (user ^. email <> ":" <> user ^. password))
      )

mkAuth :: (MonadGen m) => ApiState v -> m (Auth v)
mkAuth state = Normal . uncurry AuthKey <$> Gen.element (M.toList state._users)

mkBadAuth :: (MonadGen m) => ApiState v -> m (Auth v)
mkBadAuth state =
  Gen.choice
    [ pure $ Bad Nothing,
      if M.null state._users
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
extractJwt =
  maybe
    (fail "Could not extract JWT value")
    (pure . takeWhile (/= ';') . drop 1 . dropWhile (/= '=') . BS8.unpack . snd)
    -- Find the JWT cookie
    . find (BS8.isPrefixOf "JWT" . snd)
    . findSetCookies
    . H.responseHeaders

findSetCookies :: [Header] -> [Header]
findSetCookies = filter (\(k, v) -> k == mk "Set-Cookie" && not (BS.null v))

extractKey :: (MonadFail m) => H.Response body -> m String
extractKey =
  maybe
    (fail "No Location header returned")
    (pure . BS8.unpack . snd)
    . find (\(h, _) -> h == "Location")
    . H.responseHeaders

--
-- Main api commands
--

type CanStateM gen m = (MonadGen gen, MonadFail m, MonadThrow m, MonadIO m, MonadTest m)

cRegister :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cRegister env =
  Command
    gen
    execute
    [ Require $ \state input -> not $ any (\u -> u ^. email == input ^. email) state._users,
      Update $ \state (RegisterUser e p g) output ->
        state & users .~ M.insert output (TestUser e p g Nothing) state._users,
      Ensure $ \oldState newState input _output -> do
        assert $ not $ any (\u -> u ^. email == input ^. email) oldState._users
        assert $ any (\u -> u ^. email == input ^. email) newState._users
    ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (RegisterUser Symbolic))
    gen _apiState = pure genRegisterUser
    execute :: RegisterUser Concrete -> m String
    execute register = do
      req <- H.parseRequest $ env.baseUrl <> "/register"
      let req' = mkReq methodPost [formHeader] req {H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm register}
      res <- liftIO $ H.httpNoBody req' env.manager
      res.responseStatus === status204
      extractKey res

cLogin :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cLogin env =
  Command
    gen
    execute
    [ Require $ \state input ->
        let f u =
              u ^. tuEmail == input ^. tlUser
                && u ^. tuPassword == input ^. tlPass
         in any f state._users,
      Update $ \state input output ->
        let f u = u & tuJwt .~ pure output
         in state & users %~ M.update (pure . f) (input ^. key)
    ]
  where
    -- From our state, generate a value to be used for the test.
    -- In this case, pick a user to log in from the list of users.
    gen :: ApiState Symbolic -> Maybe (gen (TestLogin Symbolic))
    gen apiState =
      if M.null apiState._users
        then Nothing
        else Just $ do
          (k, u) <- Gen.element $ M.toList apiState._users
          pure $ TestLogin k Good (u ^. tuEmail) (u ^. tuPassword)
    -- What we want to do with this value
    execute login = do
      req <- H.parseRequest $ env.baseUrl <> "/login"
      let req' = mkReq methodPost [formHeader] req {H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm login}
      res <- liftIO $ H.httpNoBody req' env.manager
      res.responseStatus === status204
      extractJwt res

cLoginFail :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cLoginFail env = Command gen execute []
  where
    gen :: ApiState Symbolic -> Maybe (gen (TestLogin Symbolic))
    gen apiState = if M.null apiState._users
          then Nothing
          else Just $
            Gen.choice
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
cGetEntries env =
  Command
    gen
    execute
    [ Ensure $ \_oldState newState _input output -> do
        length newState._entries === length output
    ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntries Symbolic))
    gen state =
      if M.null state._users
        then Nothing
        else pure $ GetEntries . Normal . uncurry AuthKey <$> Gen.element (M.toList state._users)
    execute :: GetEntries Concrete -> m [BSL.ByteString]
    execute getEntries = do
      req <- H.parseRequest $ env.baseUrl <> "/entries"
      let req' = mkReq methodGet (mkAuthHeader getEntries) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200
      -- Commit the great sin of "parsing" html with regex
      let matches = getAllTextMatches (res.responseBody =~ ([r|"/entry/[[:digit:]]+"|] :: String))
      pure matches

cGetEntriesBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cGetEntriesBadAuth env = Command gen execute []
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntries Symbolic))
    gen state = pure $ GetEntries <$> mkBadAuth state
    execute :: GetEntries Concrete -> m ()
    execute getEntries = do
      req <- H.parseRequest $ env.baseUrl <> "/entries"
      let req' = mkReq methodGet (mkAuthHeader getEntries) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cGetEntry :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cGetEntry env = Command gen execute []
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntry Symbolic))
    gen state =
      if M.null state._users || M.null state._entries
        then Nothing
        else pure $ do
          k <- Gen.element (M.keys state._entries)
          GetEntry k <$> mkAuth state
    execute :: GetEntry Concrete -> m String
    execute getEntry = do
      req <- H.parseRequest $ env.baseUrl <> concrete (getEntry ^. key)
      let req' = mkReq methodGet (mkAuthHeader getEntry) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200
      pure $ BSL8.unpack res.responseBody

cGetEntryBadKey :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cGetEntryBadKey env = Command gen execute []
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntryMissing Symbolic))
    gen state =
      if M.null state._users
        then Nothing
        else pure $ do
          k <- ("/entry/" <>) <$> Gen.string (Range.linear 1 10) Gen.alpha
          GetEntryMissing k <$> mkAuth state
    execute :: GetEntryMissing Concrete -> m ()
    execute getEntry = do
      req <- H.parseRequest $ env.baseUrl <> getEntry ^. gemKey
      let req' = mkReq methodGet (mkAuthHeader getEntry) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status404

cGetEntryBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cGetEntryBadAuth env = Command gen execute []
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntry Symbolic))
    gen state =
      if M.null state._entries
        then Nothing
        else pure $ do
          GetEntry
            <$> Gen.element (M.keys state._entries)
            <*> mkBadAuth state
    execute :: GetEntry Concrete -> m ()
    execute getEntry = do
      req <- H.parseRequest $ env.baseUrl <> concrete (getEntry ^. key)
      let req' = mkReq methodGet (mkAuthHeader getEntry) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cCreateEntry :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cCreateEntry env =
  Command
    gen
    execute
    [ Update $ \state input output ->
        state
          & entries %~ M.insert output (TestEntry (input ^. ceTitle) (input ^. ceValue)),
      Ensure $ \_old _new _input output -> do
        assert $ isPrefixOf "/entry/" output
        assert $ length output > length ("/entry/" :: String)
    ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (CreateEntry Symbolic))
    gen state =
      if M.null state._users
        then Nothing
        else pure $ CreateEntry <$> mkAuth state <*> genText <*> genText
    execute :: CreateEntry Concrete -> m String
    execute createEntry = do
      req <- H.parseRequest (env.baseUrl <> "/entry")
      let req' =
            mkReq
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
    gen :: ApiState Symbolic -> Maybe (gen (CreateEntry Symbolic))
    gen state =
      if M.null state._users
        then Nothing
        else pure $ CreateEntry <$> mkBadAuth state <*> genText <*> genText
    execute :: CreateEntry Concrete -> m ()
    execute createEntry = do
      req <- H.parseRequest (env.baseUrl <> "/entry")
      let req' =
            mkReq
              methodPost
              (formHeader : mkAuthHeader createEntry)
              req
                { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm createEntry
                }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cDeleteEntry :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cDeleteEntry env =
  Command
    gen
    execute
    [ Update $ \state input _output ->
        state
          & entries %~ M.delete (input ^. key)
    ]
  where
    gen :: (Ord1 v) => ApiState v -> Maybe (gen (GetEntry v))
    gen state =
      if not (M.null state._users) && not (M.null state._entries)
        then Just $ do
          GetEntry
            <$> Gen.element (M.keys state._entries)
            <*> mkAuth state
        else Nothing
    execute :: GetEntry Concrete -> m ()
    execute deleteEntry = do
      req <- H.parseRequest $ env.baseUrl <> concrete (deleteEntry ^. key) <> "/delete"
      let req' = mkReq methodDelete (mkAuthHeader deleteEntry) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200

cDeleteEntryBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cDeleteEntryBadAuth env = Command gen execute []
  where
    gen :: (Ord1 v) => ApiState v -> Maybe (gen (GetEntry v))
    gen state =
      if not (M.null state._users) && not (M.null state._entries)
        then Just $ do
          GetEntry
            <$> Gen.element (M.keys state._entries)
            <*> mkBadAuth state
        else Nothing
    execute :: GetEntry Concrete -> m ()
    execute deleteEntry = do
      req <- H.parseRequest $ env.baseUrl <> concrete (deleteEntry ^. key) <> "/delete"
      let req' = mkReq methodDelete (mkAuthHeader deleteEntry) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cUpdateEntry :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cUpdateEntry env =
  Command
    gen
    execute
    [ Update $ \state input _output ->
        state
          & entries %~ M.update (pure . updateEntry input) (input ^. key)
    ]
  where
    updateEntry :: UpdateEntry v -> TestEntry v -> TestEntry v
    updateEntry input u =
      u
        & teTitle .~ input ^. ueTitle
        & teValue .~ input ^. ueValue
    gen :: (Ord1 v) => ApiState v -> Maybe (gen (UpdateEntry v))
    gen state =
      if not (M.null state._users) && not (M.null state._entries)
        then
          Just $
            UpdateEntry
              <$> mkAuth state
              <*> Gen.element (M.keys state._entries)
              <*> genText
              <*> genText
        else Nothing
    execute :: UpdateEntry Concrete -> m ()
    execute input = do
      req <- H.parseRequest $ env.baseUrl <> concrete (input ^. key) <> "/update"
      let req' =
            mkReq
              methodPut
              (formHeader : mkAuthHeader input)
              req
                { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm input
                }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200

cUpdateEntryBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cUpdateEntryBadAuth env = Command gen execute []
  where
    gen :: (Ord1 v) => ApiState v -> Maybe (gen (UpdateEntry v))
    gen state =
      if not (M.null state._users) && not (M.null state._entries)
        then
          Just $
            UpdateEntry
              <$> mkBadAuth state
              <*> Gen.element (M.keys state._entries)
              <*> genText
              <*> genText
        else Nothing
    execute :: UpdateEntry Concrete -> m ()
    execute updateEntry = do
      req <- H.parseRequest $ env.baseUrl <> concrete (updateEntry ^. key) <> "/update"
      let req' =
            mkReq
              methodPut
              (formHeader : mkAuthHeader updateEntry)
              req
                { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm updateEntry
                }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cCreateUser :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cCreateUser env =
  Command
    gen
    execute
    [ Update $ \state input output ->
        state
          & users %~ M.insert output (TestUser (input ^. email) (input ^. password) (input ^. cuGroup) Nothing),
      Ensure $ \old new input _output -> do
        length old._users + 1 === length new._users
        assert $ any (\u -> u ^. email == input ^. email) new._users
    ]
  where
    gen :: ApiState v -> Maybe (gen (CreateUser v))
    gen state =
      if M.null state._users
        then Nothing
        else Just $ do
          CreateUser
            <$> mkAuth state
            <*> genGroup
            <*> Gen.filterT (\e -> not $ any (\u -> u ^. email == e) state._users) genEmail
            <*> genPassword
    execute :: CreateUser Concrete -> m String
    execute cUser = do
      req <- H.parseRequest $ env.baseUrl <> "/user"
      let req' =
            mkReq
              methodPost
              (formHeader : mkAuthHeader cUser)
              req
                { H.requestBody = H.RequestBodyLBS $ urlEncodeAsForm $ toForm cUser
                }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status303
      extractKey res

cCreateUserBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cCreateUserBadAuth env = Command gen execute []
  where
    gen :: ApiState v -> Maybe (gen (CreateUser v))
    gen state =
      if M.null state._users
        then Nothing
        else Just $ do
          CreateUser
            <$> mkBadAuth state
            <*> genGroup
            <*> Gen.filterT (\e -> not $ any (\u -> u ^. email == e) state._users) genEmail
            <*> genPassword
    execute :: CreateUser Concrete -> m ()
    execute cUser = do
      req <- H.parseRequest $ env.baseUrl <> "/user"
      let req' =
            mkReq
              methodPost
              (formHeader : mkAuthHeader cUser)
              req
                { H.requestBody = H.RequestBodyLBS $ urlEncodeAsForm $ toForm cUser
                }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cUpdateUser :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cUpdateUser env =
  Command
    gen
    execute
    [ Update $ \state input _output ->
        let update u =
              u
                & tuPassword .~ maybe (u ^. tuPassword) (view newPassword) (input ^. uuPassword)
                & tuGroup .~ fromMaybe (u ^. tuGroup) (input ^. uuGroup)
         in state & users %~ M.update (pure . update) (input ^. key)
    ]
  where
    gen :: ApiState v -> Maybe (gen (UpdateUser v))
    gen state =
      if M.null state._users
        then Nothing
        else Just $ do
          (k, u) <- Gen.element $ M.toList state._users
          UpdateUser k
            <$> genPasswordUpdate u
            <*> fmap pure genGroup
            <*> mkAuth state
    execute :: UpdateUser Concrete -> m ()
    execute update = do
      req <- H.parseRequest $ env.baseUrl <> concrete (update ^. key) <> "/update"
      let req' =
            mkReq
              methodPut
              (formHeader : mkAuthHeader update)
              req
                { H.requestBody = H.RequestBodyLBS $ urlEncodeAsForm $ toForm update
                }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status200

cUpdateUserBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cUpdateUserBadAuth env = Command gen execute []
  where
    gen :: ApiState v -> Maybe (gen (UpdateUser v))
    gen state =
      if M.null state._users
        then Nothing
        else Just $ do
          (k, u) <- Gen.element $ M.toList state._users
          UpdateUser k
            <$> genPasswordUpdate u
            <*> fmap pure genGroup
            <*> mkBadAuth state
    execute :: UpdateUser Concrete -> m ()
    execute update = do
      req <- H.parseRequest $ env.baseUrl <> concrete (update ^. key) <> "/update"
      let req' =
            mkReq
              methodPut
              (formHeader : mkAuthHeader update)
              req
                { H.requestBody = H.RequestBodyLBS $ urlEncodeAsForm $ toForm update
                }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status401

cDeleteUser :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cDeleteUser env =
  Command
    gen
    execute
    [ Update $ \state input _output ->
        state & users %~ M.delete (input ^. key),
      Ensure $ \old new input _output -> do
        length old._users === length new._users + 1
        assert $ not $ M.member (input ^. key) new._users
        assert $ M.member (input ^. key) old._users
    ]
  where
    gen :: ApiState v -> Maybe (gen (DeleteUser v))
    gen state =
      if M.null state._users
        then Nothing
        else Just $ do
          (k, u) <- Gen.element $ M.toList state._users
          k' <- Gen.element (M.keys state._users)
          pure $ DeleteUser (Normal $ AuthKey k u) k'
    execute :: DeleteUser Concrete -> m ()
    execute update = do
      req <- H.parseRequest $ env.baseUrl <> concrete (update ^. key) <> "/delete"
      let req' = mkReq methodDelete (formHeader : mkAuthHeader update) req
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status200

cDeleteUserBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cDeleteUserBadAuth env = Command gen execute []
  where
    gen :: ApiState v -> Maybe (gen (DeleteUser v))
    gen state =
      if M.null state._users
        then Nothing
        else Just $ do
          a <- mkBadAuth state
          k <- Gen.element $ M.keys state._users
          pure $ DeleteUser a k
    execute :: DeleteUser Concrete -> m ()
    execute update = do
      req <- H.parseRequest $ env.baseUrl <> concrete (update ^. key) <> "/delete"
      let req' = mkReq methodDelete (formHeader : mkAuthHeader update) req
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status401

cGetUser :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cGetUser env = Command gen execute []
  where
    gen :: ApiState v -> Maybe (gen (GetUser v))
    gen state =
      if M.null state._users
        then Nothing
        else Just $ do
          (k, u) <- Gen.element $ M.toList state._users
          k' <- Gen.element $ M.keys state._users
          pure $ GetUser (Normal $ AuthKey k u) k'
    execute :: GetUser Concrete -> m String
    execute getUser = do
      req <- H.parseRequest $ env.baseUrl <> concrete (getUser ^. key)
      let req' = mkReq methodGet (mkAuthHeader getUser) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200
      pure $ BSL8.unpack res.responseBody

cGetUserBadAuth :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cGetUserBadAuth env = Command gen execute []
  where
    gen :: ApiState v -> Maybe (gen (GetUser v))
    gen state =
      if M.null state._users
        then Nothing
        else
          pure $
            GetUser
              <$> mkBadAuth state
              <*> Gen.element (M.keys state._users)
    execute :: GetUser Concrete -> m ()
    execute getUser = do
      req <- H.parseRequest $ env.baseUrl <> concrete (getUser ^. key)
      let req' = mkReq methodGet (mkAuthHeader getUser) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

--
-- Side channel commands.
-- These are used to check that the server is doing what we expect in
-- multiple ways. Things like checking what the DB thinks the world
-- looks like vs what the API thinks the world looks like.
--

-- cTestReset :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
-- cTestReset env = Command gen execute
--   [ Update $ \_state _ _ -> initialState
--   ]
--   where
--     gen :: ApiState Symbolic -> Maybe (gen (Reset Symbolic))
--     gen _apiState = Just $ pure Reset
--     execute :: Reset Concrete -> m ()
--     execute _reset = do
--       req <- H.parseRequest (env.baseUrl <> "/reset")
--       let req' = req {H.method = methodPost}
--       res <- liftIO $ H.httpNoBody req' env.manager
--       res.responseStatus === status204

cTestGetUsers :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cTestGetUsers env =
  Command
    gen
    execute
    [ Ensure $ \_oldState newState _input output ->
        let stateEmails = sort (M.elems $ view email <$> newState._users)
            apiEmails = sort ((.email) <$> output)
         in stateEmails === apiEmails
    ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetTestUsers Symbolic))
    gen _ = Just $ pure GetTestUsers
    execute :: GetTestUsers Concrete -> m [User]
    execute _ = do
      req <- H.parseRequest (env.baseUrl <> "/getUsers")
      let req' = req {H.method = methodGet}
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200
      either fail pure $ eitherDecode @[User] res.responseBody

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
      ($ env)
        <$> [ cRegister,
              cLogin,
              cLoginFail,
              cTestGetUsers,
              -- Entry commands
              cGetEntry,
              cGetEntryBadKey,
              cGetEntryBadAuth,
              cGetEntries,
              cGetEntriesBadAuth,
              cCreateEntry,
              cCreateEntryBadAuth,
              cDeleteEntry,
              cDeleteEntryBadAuth,
              cUpdateEntry,
              cUpdateEntryBadAuth,
              -- User commands
              cCreateUser,
              cCreateUserBadAuth,
              cUpdateUser,
              cUpdateUserBadAuth,
              cDeleteUser,
              cDeleteUserBadAuth,
              cGetUser,
              cGetUserBadAuth
            ]