{-# LANGUAGE QuasiQuotes #-}

-- This helps us make sure that our commands are all added to the state machine
{-# OPTIONS_GHC -Wunused-top-binds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.StateMachine where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString qualified as BS
import Data.CaseInsensitive (mk)
import Data.List (sort, find)
import Data.Text (Text)
import Hedgehog hiding (Group)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client qualified as H
import Network.HTTP.Types ( Header, status204, methodPost, methodGet, status200, status303, methodDelete, methodPut, status401, Status, Method )
import Website.Auth.Authorisation qualified as Auth
import Website.Data.User (User)
import qualified Website.Data.User (User (..))
import Web.FormUrlEncoded
import Test.StateMachine.Types
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA
import Text.RawString.QQ
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding.Base64 as B64
import Data.Base64.Types
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Char
import GHC.Records
import Data.Map qualified as M
import Control.Lens
import Data.Monoid

-- Reference the following QFPL blog posts to refresh yourself.
-- https://qfpl.io/posts/intro-to-state-machine-testing-1/
-- https://qfpl.io/posts/intro-to-state-machine-testing-2/
-- https://qfpl.io/posts/intro-to-state-machine-testing-3/
import Website.Auth.Authorisation ()

--
-- Generators
--

chars :: MonadGen m => m Char
chars = Gen.filterT predicate Gen.ascii
  where
    predicate c = isPrint c && c /= '\n' -- isAlpha c || isNumber c

minTextLength, maxTextLength :: Num a => a
minTextLength = 1
maxTextLength = 10

textLength :: Integral a => Range a
textLength = Range.linear minTextLength maxTextLength

genText :: MonadGen m => m Text
genText = Gen.text textLength chars

genEmail :: MonadGen m => m Text
genEmail = Gen.text textLength $ Gen.filterT (/= ':') $ Gen.filterT isAlpha chars

genPassword :: MonadGen m => m Text
genPassword =  Gen.text textLength $ Gen.filterT (/= ':') chars

genGroup :: MonadGen m => m Auth.Group
genGroup = Gen.element [Auth.Admin, Auth.User]

genRegisterUser :: MonadGen m => m (RegisterUser v)
genRegisterUser = RegisterUser <$> genEmail <*> genPassword <*> genGroup

genPasswordUpdate :: MonadGen m => TestUser v -> m (Maybe (PasswordUpdate v))
genPasswordUpdate user = do
  Gen.choice
    [ pure Nothing
    , do
      newPass <- Gen.filterT (/= user ^. tuPassword) genPassword 
      pure $ pure PasswordUpdate
        { _oldPassword = user ^. tuPassword
        , _newPassword = newPass
        }
    ]

--
-- Helper functions
--

mkReq :: Method -> [Header] -> H.Request -> H.Request
mkReq method headers req = req
  { H.method = method
  , H.redirectCount = 0
  , H.requestHeaders = acceptHtml : headers <> H.requestHeaders req
  }

emailExists :: HasEmail r => ApiState v -> r -> Bool
emailExists state command = any (\u -> u ^. email == command ^. email) state._stateUsers

userExists :: (HasAuth r, Ord1 v) => ApiState v -> r v -> Bool
userExists state command = case command ^. auth of
  Normal user -> M.member (user ^. key) state._stateUsers
  Bad _ -> True

entryExists :: (HasKey r, Ord1 v) => ApiState v -> r v -> Bool
entryExists state command = M.member (command ^. key) state._stateEntries

entryKeyExists :: (Ord1 v, HasField "key" command (Var BS8.ByteString v)) => ApiState v -> command -> Bool
entryKeyExists state command = M.member command.key state._stateEntries

userKeyDoesntExist :: (Ord1 v, HasKey r) => ApiState v -> r v -> Bool
userKeyDoesntExist state input = M.notMember (input ^. key) state._stateUsers

entryKeyDoesntExist :: (HasKey r, Ord1 v) => ApiState v -> r v -> Bool
entryKeyDoesntExist state input = M.notMember (input ^. key) state._stateEntries

response204 :: (HasField "responseStatus" r Status, MonadTest m) => ApiState v -> ApiState v -> a -> r -> m ()
response204 _old _new _input output = output.responseStatus === status204

mkAuthHeader :: HasAuth command => command v -> [Header]
mkAuthHeader command = case command ^. auth of
  Normal user -> pure $ mkBasicAuthHeader $  user ^. akUser
  Bad user -> badHeader $ view akUser <$> user
  where
    badHeader :: Maybe (TestUser v) -> [Header]
    badHeader = maybe [] (pure . mkBasicAuthHeader)
    mkBasicAuthHeader :: TestUser v -> Header
    mkBasicAuthHeader user =
      ( "Authorization"
      , "Basic " <> encodeUtf8 (extractBase64 @_ @T.Text $ B64.encodeBase64 (user ^. email <> ":" <> user ^. password))
      )

mkAuth :: MonadGen m => ApiState v -> m (Auth v)
mkAuth state = Normal . uncurry AuthKey <$> Gen.element (M.toList state._stateUsers)

mkBadAuth :: MonadGen m => ApiState v -> m (Auth v)
mkBadAuth state = Gen.choice
  [ pure $ Bad Nothing
  , if M.null state._stateUsers
    then pure $ Bad Nothing
    else do
      (k, u) <- Gen.element $ M.toList state._stateUsers
      Bad . pure <$> Gen.choice
        [ do
          newPass <- Gen.filterT (/= u ^. password) genPassword
          pure $ AuthKey k $ u & tuPassword .~ newPass
        , do
          newEmail <- Gen.filterT (/= u ^. email) genEmail
          pure $ AuthKey k $ u & tuEmail .~ newEmail
        ]
  ]

formHeader :: Header
formHeader = ("Content-Type", "application/x-www-form-urlencoded")

acceptHtml :: Header
acceptHtml = ("Accept", "text/html")

extractJwt :: (MonadFail m, MonadTest m) => [Header] -> m BS8.ByteString
extractJwt = maybe
  (fail "Could not extract JWT value")
  (pure . snd)
  -- Find the JWT cookie
  . find (BS8.isPrefixOf "JWT" . snd)

findSetCookies :: [Header] -> [Header]
findSetCookies = filter (\(k, v) -> k == mk "Set-Cookie" && not (BS.null v))

--
-- Main api commands
--

type CanStateM gen m = (MonadGen gen, MonadFail m, MonadThrow m, MonadIO m, MonadTest m)

cRegister :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cRegister env = Command gen execute
  [ Update $ \state (RegisterUser e p g) output -> 
    state & stateUsers .~ M.insert output (TestUser e p g) state._stateUsers
  , Ensure $ \oldState newState input _output -> do
    assert $ not $ any (\u -> u ^. email == input ^. email) oldState._stateUsers
    assert $       any (\u -> u ^. email == input ^. email) newState._stateUsers
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (RegisterUser Symbolic))
    gen _apiState = pure genRegisterUser
    execute :: RegisterUser Concrete -> m BS8.ByteString
    execute register = do
      req <- H.parseRequest $ env.baseUrl <> "/register"
      let req' = mkReq methodPost [formHeader] req { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm register }
      res <- liftIO $ H.httpNoBody req' env.manager
      res.responseStatus === status204
      maybe
        (fail "No Location header returned")
        (pure . snd)
        $ find (\(h, _) -> h == "Location")
        $ H.responseHeaders res
      
cLogin :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cLogin env = Command gen execute
  [ Require $ \state input ->
      let f u = Any $ 
            u ^. tuEmail == input ^. tlUser &&
            u ^. tuPassword == input ^. tlPass
      in getAny $ state ^. stateUsers . each . to f
  ]
  where
    -- From our state, generate a value to be used for the test.
    -- In this case, pick a user to log in from the list of users.
    gen :: ApiState Symbolic -> Maybe (gen (TestLogin Symbolic))
    gen apiState = if M.null apiState._stateUsers
      then Nothing
      else Just $ do
        user <- Gen.element apiState._stateUsers
        pure $ TestLogin Good (user ^. tuEmail) (user ^. tuPassword)
    -- What we want to do with this value
    execute login = do
      req <- H.parseRequest $ env.baseUrl <> "/login"
      let req' = mkReq methodPost [formHeader] req { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm login }
      res <- liftIO $ H.httpNoBody req' env.manager
      res.responseStatus === status204
      jwt <- extractJwt $ findSetCookies res.responseHeaders
      -- Drop the cookie name and settings. We don't care about them and
      -- only want to get the bearer token
      pure $ BS8.takeWhile (/= ';') $ BS8.drop 1 $ BS8.dropWhile (/= '=') jwt

cLoginFail :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cLoginFail env = Command gen execute []
  where
    gen :: ApiState Symbolic -> Maybe (gen (TestLogin Symbolic))
    gen apiState = Just $
      -- We have 3 things we want to test.
      -- 1) unknown user-password pairs. This can be done if there are no users.
      -- 2) known user with a bad password
      -- 3) unknown user with a known password
      let genRandom = TestLogin Random <$> genEmail <*> genPassword
      in if M.null apiState._stateUsers
        then genRandom
        else do
          Gen.choice
            [ Gen.filterT
              ( \u -> not (any (\u' -> u' ^. email == u ^. email) apiState._stateUsers)
                   && not (any (\u' -> u' ^. password == u ^. password) apiState._stateUsers)
              )
              genRandom
            , do
              user <- Gen.element apiState._stateUsers
              TestLogin BadPassword (user ^. email)
                <$> Gen.filterT (\p -> p /= user ^. password) genPassword
            , do
              user <- Gen.element apiState._stateUsers
              TestLogin BadUser
                <$> Gen.filterT (\e -> e /= user ^. email) genEmail
                <*> pure (user ^. password)
            ]
    execute login = do
      req <- H.parseRequest $ env.baseUrl <> "/login"
      let req' = mkReq methodPost [formHeader] req { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm login }
      res <- liftIO $ H.httpNoBody req' env.manager
      res.responseStatus === status401

cGetEntries :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cGetEntries env = Command gen execute
  [ Ensure $ \_oldState newState _input output -> do
    length newState._stateEntries === length output
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntries Symbolic))
    gen state = if M.null state._stateUsers
      then Nothing
      else pure $ GetEntries . Normal . uncurry AuthKey <$> Gen.element (M.toList state._stateUsers)
    execute :: GetEntries Concrete -> m [BSL.ByteString]
    execute getEntries = do
      req <- H.parseRequest $ env.baseUrl <> "/entries"
      let req' = mkReq methodGet (mkAuthHeader getEntries) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200
      -- Commit the great sin of "parsing" html with regex
      let matches = getAllTextMatches (res.responseBody =~ ([r|"/entry/[[:digit:]]+"|] :: String))
      pure matches

cGetEntriesBadAuth :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
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

cGetEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cGetEntry env = Command gen execute []
  where
    gen :: ApiState Symbolic -> Maybe (gen (EntryAccess Symbolic))
    gen state = if M.null state._stateUsers || M.null state._stateEntries
      then Nothing
      else pure $ do
        k <- Gen.element (M.keys state._stateEntries)
        EntryAccess k <$> mkAuth state
    execute :: EntryAccess Concrete -> m BSL8.ByteString
    execute getEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete $ getEntry ^. key)
      let req' = mkReq methodGet (mkAuthHeader getEntry) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200
      pure res.responseBody

cGetEntryBadAuth :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cGetEntryBadAuth env = Command gen execute []
  where
    gen :: ApiState Symbolic -> Maybe (gen (EntryAccess Symbolic))
    gen state = if M.null state._stateEntries
      then Nothing
      else pure $ do
        EntryAccess
          <$> Gen.element (M.keys state._stateEntries)
          <*> mkBadAuth state
    execute :: EntryAccess Concrete -> m ()
    execute getEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete $ getEntry ^. key)
      let req' = mkReq methodGet (mkAuthHeader getEntry) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cCreateEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cCreateEntry env = Command gen execute
  [ Update $ \state input output -> state &
    stateEntries %~ M.insert output (TestEntry (input ^. ceTitle) (input ^. ceValue))
  , Ensure $ \_old _new _input output -> do
    assert $ BS8.isPrefixOf "/entry/" output
    assert $ BS8.length output > BS8.length "/entry/"
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (CreateEntry Symbolic))
    gen state = if M.null state._stateUsers
      then Nothing
      else pure $ CreateEntry <$> mkAuth state <*> genText <*> genText
    execute :: CreateEntry Concrete -> m BS8.ByteString
    execute createEntry = do
      req <- H.parseRequest (env.baseUrl <> "/entry")
      let req' = mkReq methodPost (formHeader : mkAuthHeader createEntry) req
            { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm createEntry }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status303
      maybe
        (fail "No Location header returned")
        (pure . snd)
        $ find (\(h, _) -> h == "Location")
        $ H.responseHeaders res

cCreateEntryBadAuth :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cCreateEntryBadAuth env = Command gen execute []
  where
    gen :: ApiState Symbolic -> Maybe (gen (CreateEntry Symbolic))
    gen state = if M.null state._stateUsers
      then Nothing
      else pure $ CreateEntry <$> mkBadAuth state <*> genText <*> genText
    execute :: CreateEntry Concrete -> m ()
    execute createEntry = do
      req <- H.parseRequest (env.baseUrl <> "/entry")
      let req' = mkReq methodPost (formHeader:mkAuthHeader createEntry) req
            { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm createEntry }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cDeleteEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cDeleteEntry env = Command gen execute
  [ Update $ \state input _output -> state &
    stateEntries %~ M.delete (input ^. key)
  ]
  where
    gen :: Ord1 v => ApiState v -> Maybe (gen (EntryAccess v))
    gen state = if not (M.null state._stateUsers) && not (M.null state._stateEntries)
      then Just $ do
        EntryAccess
          <$> Gen.element (M.keys state._stateEntries)
          <*> mkAuth state
      else Nothing
    execute :: EntryAccess Concrete -> m ()
    execute deleteEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete $ deleteEntry ^. key) <> "/delete"
      let req' = mkReq methodDelete (mkAuthHeader deleteEntry) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200

cDeleteEntryBadAuth :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cDeleteEntryBadAuth env = Command gen execute []
  where
    gen :: Ord1 v => ApiState v -> Maybe (gen (EntryAccess v))
    gen state = if not (M.null state._stateUsers) && not (M.null state._stateEntries)
      then Just $ do
        EntryAccess
          <$> Gen.element (M.keys state._stateEntries)
          <*> mkBadAuth state
      else Nothing
    execute :: EntryAccess Concrete -> m ()
    execute deleteEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete $ deleteEntry ^. key) <> "/delete"
      let req' = mkReq methodDelete (mkAuthHeader deleteEntry) req
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cUpdateEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cUpdateEntry env = Command gen execute
  [ Update $ \state input _output -> state
    & stateEntries %~ M.update (pure . updateEntry input) (input ^. key)
  ]
  where
    updateEntry :: UpdateEntry v -> TestEntry v -> TestEntry v
    updateEntry input u = u
      & teTitle .~ input ^. ueTitle
      & teValue .~ input ^. ueValue
    gen :: Ord1 v => ApiState v -> Maybe (gen (UpdateEntry v))
    gen state = if not (M.null state._stateUsers) && not (M.null state._stateEntries)
      then Just $ UpdateEntry
        <$> mkAuth state
        <*> Gen.element (M.keys state._stateEntries)
        <*> genText
        <*> genText
      else Nothing
    execute :: UpdateEntry Concrete -> m ()
    execute input = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete $ input ^. key) <> "/update"
      let req' = mkReq methodPut (formHeader : mkAuthHeader input) req
            { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm input }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status200

cUpdateEntryBadAuth :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cUpdateEntryBadAuth env = Command gen execute []
  where
    gen :: Ord1 v => ApiState v -> Maybe (gen (UpdateEntry v))
    gen state = if not (M.null state._stateUsers) && not (M.null state._stateEntries)
      then Just $ UpdateEntry
        <$> mkBadAuth state
        <*> Gen.element (M.keys state._stateEntries)
        <*> genText
        <*> genText
      else Nothing
    execute :: UpdateEntry Concrete -> m ()
    execute updateEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete $ updateEntry ^. key) <> "/update"
      let req' = mkReq methodPut (formHeader : mkAuthHeader updateEntry) req
            { H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm updateEntry }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status401

cCreateUser :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cCreateUser env = Command gen execute
  [ Update $ \state input output -> state
    & stateUsers %~ M.insert output (TestUser (input ^. email) (input ^. password) (input ^. cuGroup))
  , Ensure $ \old new input _output -> do
    length old._stateUsers + 1 === length new._stateUsers
    assert $ any (\u -> u ^. email == input ^. email) new._stateUsers
  ]
  where
    gen :: ApiState v -> Maybe (gen (CreateUser v))
    gen state = if M.null state._stateUsers
      then Nothing
      else Just $ do
        CreateUser
          <$> mkAuth state
          <*> genGroup
          <*> Gen.filterT (\e -> not $ any (\u -> u ^. email == e) state._stateUsers) genEmail
          <*> genPassword
    execute :: CreateUser Concrete -> m BS8.ByteString
    execute cUser = do
      req <- H.parseRequest $ env.baseUrl <> "/user"
      let req' = mkReq methodPost (formHeader : mkAuthHeader cUser) req
            { H.requestBody = H.RequestBodyLBS $ urlEncodeAsForm $ toForm cUser }
      res <- liftIO $ H.httpLbs req' env.manager
      res.responseStatus === status303
      maybe
        (fail "No Location header returned")
        (pure . snd)
        $ find (\(h, _) -> h == "Location")
        $ H.responseHeaders res

cUpdateUser :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cUpdateUser env = Command gen execute
  [ Update $ \state input _output ->
    let update u = u
          & tuPassword .~ maybe (u ^. tuPassword) (view newPassword) (input ^. uuPassword)
          & tuGroup .~ fromMaybe (u ^. tuGroup) (input ^. uuGroup)
    in state & stateUsers %~ M.update (pure . update) (input ^. key)
  ]
  where
    gen :: ApiState v -> Maybe (gen (UpdateUser v))
    gen state = if M.null state._stateUsers
      then Nothing
      else Just $ do
        (k, u) <- Gen.element $ M.toList state._stateUsers
        UpdateUser k
          <$> genPasswordUpdate u
          <*> fmap pure genGroup
          <*> pure (Normal $ AuthKey k u)
    execute :: UpdateUser Concrete -> m ()
    execute update = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete $ update ^. key) <> "/update"
      let req' = mkReq methodPut (formHeader : mkAuthHeader update) req
            { H.requestBody = H.RequestBodyLBS $ urlEncodeAsForm $ toForm update }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status200

-- cDeleteUser = _

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
cTestGetUsers env = Command gen execute
  [ Ensure $ \_oldState newState _input output ->
    let stateEmails = sort (M.elems $ view email <$> newState._stateUsers)
        apiEmails = sort ((.email) <$> output)
    in stateEmails === apiEmails
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetTestUsers Symbolic))
    gen _ = Just $ pure GetTestUsers
    execute :: GetTestUsers Concrete -> m [User]
    execute _ = do
      req <- H.parseRequest (env.baseUrl <> "/getUsers")
      let req' = req { H.method = methodGet }
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
    commands = ($ env) <$>
      [ cRegister
      , cLogin
      , cLoginFail
      , cTestGetUsers
      -- Entry commands
      , cGetEntry
      , cGetEntryBadAuth
      , cGetEntries
      , cGetEntriesBadAuth
      , cCreateEntry
      , cCreateEntryBadAuth
      , cDeleteEntry
      , cDeleteEntryBadAuth
      , cUpdateEntry
      , cUpdateEntryBadAuth
      -- User commands
      , cCreateUser
      , cUpdateUser
      ]