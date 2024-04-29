{-# LANGUAGE QuasiQuotes #-}

-- This helps us make sure that our commands are all added to the state machine
{-# OPTIONS_GHC -Wunused-top-binds #-}

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
import Network.HTTP.Types
    ( Header, status204, methodPost, methodGet, status200, status303, methodDelete, methodPut, status401 )
import Test.Db.Entry ()
import Test.Db.User ()
import Website.Auth.Authorisation qualified as Auth
import Website.Data.User
import Web.FormUrlEncoded
import Test.StateMachine.Types
import Data.Maybe (isJust, isNothing)
import Text.Regex.TDFA
import Text.RawString.QQ
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding.Base64.URL as B64
import Data.Base64.Types
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

-- Reference the following QFPL blog posts to refresh yourself.
-- https://qfpl.io/posts/intro-to-state-machine-testing-1/
-- https://qfpl.io/posts/intro-to-state-machine-testing-2/
-- https://qfpl.io/posts/intro-to-state-machine-testing-3/

--
-- Generators
--

chars :: MonadGen m => m Char
chars = Gen.element $ ['a' .. 'z'] <> ['A' .. 'Z']

genEmail :: MonadGen m => m Text
genEmail = Gen.text (Range.linear 1 10) $ Gen.filterT (/= ':') chars

genPassword :: MonadGen m => m Text
genPassword = Gen.text (Range.linear 1 10) chars

genGroup :: MonadGen m => m Auth.Group
genGroup = Gen.element [Auth.Admin, Auth.User]

genTestUser :: MonadGen m => m (TestUser v)
genTestUser = TestUser <$> genEmail <*> genPassword <*> genGroup <*> pure Nothing

--
-- Helper functions
--

mkAuthHeader :: TestUser Concrete -> Header
mkAuthHeader user = case user.testUserJwt of
  Nothing -> mkBasicAuthHeader user
  Just jwt -> mkJwtAuthHeader jwt

mkBasicAuthHeader :: TestUser v -> Header
mkBasicAuthHeader user =
  ( "Authorization"
  , "Basic " <> encodeUtf8 (extractBase64 @_ @T.Text $ B64.encodeBase64 (user.testUserEmail <> ":" <> user.testUserPassword))
  )

mkJwtAuthHeader :: Var BS8.ByteString Concrete -> Header
mkJwtAuthHeader jwt =
  ( "Authorization"
  , "Bearer " <> concrete jwt
  )

formHeader :: Header
formHeader = ("Content-Type", "application/x-www-form-urlencoded")

acceptHtml :: Header
acceptHtml = ("Accept", "text/html")
--
-- Main api commands
--

type CanStateM gen m = (MonadGen gen, MonadFail m, MonadThrow m, MonadIO m, MonadTest m)

cRegister :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cRegister env = Command gen execute
  [ Require $ \state input -> notElem input.testUserEmail $ testUserEmail <$> state.users
  , Update $ \state input _output -> state { users = input : state.users }
  , Ensure $ \oldState newState input output -> do
    output.responseStatus === status204
    assert $ notElem input.testUserEmail $ testUserEmail <$> oldState.users
    assert $ elem input.testUserEmail $ testUserEmail <$> newState.users
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (TestUser Symbolic))
    gen _apiState = pure genTestUser
    execute register = do
      req <- H.parseRequest $ env.baseUrl <> "/register"
      let req' = req
            { H.method = methodPost
            , H.requestHeaders =
                formHeader
              : acceptHtml
              : req.requestHeaders
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm $ toCreateUser register
            }
      annotate $ show req'
      liftIO $ H.httpNoBody req' env.manager

cLogin :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cLogin env = Command gen execute
  -- Check that the state is in a position where we can run this command
  [ Require $ \state input -> isJust $ find
      -- Tell hedgehog to actually ensure that the user we're trying to login from
      -- actually exists. Otherwise it can get funky ideas like having split values
      -- and putting new strings out of thin air.
      (\u -> u.testUserEmail == input.testUser && u.testUserPassword == input.testPass)
      state.users
  , Update $ \state input output ->
    let updateUser u = if u.testUserEmail == input.testUser && u.testUserPassword == input.testPass
          then u { testUserJwt = pure output }
          else u
    in state { users = updateUser <$> state.users }
  ]
  where
    -- From our state, generate a value to be used for the test.
    -- In this case, pick a user to log in from the list of users.
    gen :: ApiState Symbolic -> Maybe (gen (TestLogin Symbolic))
    gen apiState = if null apiState.users
      then Nothing
      else Just $ do
        user <- Gen.element apiState.users
        pure $ TestLogin user.testUserEmail user.testUserPassword
    -- What we want to do with this value
    execute login = do
      req <- H.parseRequest (env.baseUrl <> "/login")
      let req' = req
            { H.method = methodPost
            , H.requestHeaders = formHeader : acceptHtml : H.requestHeaders req
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm login
            }
      res <- liftIO $ H.httpNoBody req' env.manager
      annotate $ show res
      res.responseStatus === status204
      -- Find cookies
      let cookies = filter (\(k, v) -> k == mk "Set-Cookie" && not (BS.null v)) res.responseHeaders
      jwt <- maybe
        (fail "Could not extract JWT value")
        (pure . snd)
        $ find
        -- Find the JWT cookie
        (\(_, v) -> BS8.isPrefixOf "JWT" v)
        cookies
      -- Drop the cookie name and settings
      let jwt' = BS8.takeWhile (/= ';') $ BS8.drop 1 $ BS8.dropWhile (/= '=') jwt
      pure jwt'

cLoginFail :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cLoginFail env = Command gen execute
  [ Require $ \state input -> isNothing $ find
      -- Tell hedgehog to actually ensure that the user we're trying to login from
      -- actually exists. Otherwise it can get funky ideas like having split values
      -- and putting new strings out of thin air.
      (\u -> u.testUserEmail == input.testUser && u.testUserPassword == input.testPass)
      state.users
  ]
  where
    -- From our state, generate a value to be used for the test.
    -- In this case, pick a user to log in from the list of users.
    gen :: ApiState Symbolic -> Maybe (gen (TestLoginBad Symbolic))
    gen apiState = Just $
      -- We have 3 things we want to test.
      -- 1) unknown user-password pairs. This can be done if there are no users.
      -- 2) known user with a bad password
      -- 3) unknown user with a known password
      let genRandom = TestLoginBad Random <$> genEmail <*> genPassword
      in if null apiState.users
        then genRandom
        else do
          Gen.choice
            [ Gen.filterT
              ( \u -> u.testUser `notElem` fmap testUserEmail apiState.users
                   && u.testPass `notElem` fmap testUserPassword apiState.users
              )
              genRandom
            , do
              user <- Gen.element apiState.users
              TestLoginBad BadPassword user.testUserEmail
                <$> Gen.filterT (\p -> p /= user.testUserPassword) genPassword
            , do
              user <- Gen.element apiState.users
              TestLoginBad BadUser
                <$> Gen.filterT (\e -> e /= user.testUserEmail) genEmail
                <*> pure user.testUserPassword
            ]

    -- What we want to do with this value
    execute login = do
      req <- H.parseRequest (env.baseUrl <> "/login")
      let req' = req
            { H.method = methodPost
            , H.requestHeaders = formHeader : acceptHtml : H.requestHeaders req
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm login
            }
      res <- liftIO $ H.httpNoBody req' env.manager
      annotate $ show res
      res.responseStatus === status401

cGetEntries :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cGetEntries env = Command gen execute
  [ Require $ \state input -> input.getEntriesUser `elem` state.users
  , Ensure $ \_oldState newState _input output -> do
    length newState.entries === length output
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntries Symbolic))
    gen state = if null state.users
      then Nothing
      else pure $ GetEntries <$> Gen.element state.users
    execute :: GetEntries Concrete -> m [BSL.ByteString]
    execute getEntries = do
      req <- H.parseRequest (env.baseUrl <> "/entries")
      let req' = req
            { H.method = methodGet
            , H.requestHeaders
              = acceptHtml
              : mkAuthHeader getEntries.getEntriesUser
              : H.requestHeaders req
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status200
      let body = res.responseBody
      -- Commit the great sin of "parsing" html with regex
          matches = getAllTextMatches (body =~ ([r|"/entry/[[:digit:]]+"|] :: String))
      annotate $ show matches
      pure matches

cGetEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cGetEntry env = Command gen execute
  [ Require $ \state input -> input.getEntryAuth `elem` state.users
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntry Symbolic))
    gen state =
      let entries = testKey <$> state.entries
      in if null state.users || null entries
        then Nothing
        else pure $ GetEntry <$> Gen.element state.users <*> Gen.element entries
    execute :: GetEntry Concrete -> m BSL8.ByteString
    execute getEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete getEntry.getEntryId)
      let req' = req
            { H.method = methodGet
            , H.requestHeaders
              = acceptHtml
              : mkAuthHeader getEntry.getEntryAuth
              : H.requestHeaders req
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status200
      pure res.responseBody

cCreateEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cCreateEntry env = Command gen execute
  [ Require $ \state input -> input.createEntryAuth `elem` state.users
  , Update $ \state input output -> state
    { entries = TestEntry output input.createEntryTitle input.createEntryValue : state.entries
    }
  , Ensure $ \_old _new _input output -> do
    annotate $ show output
    assert $ BS8.isPrefixOf "/entry/" output
    assert $ BS8.length output > BS8.length "/entry/"
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (CreateEntry Symbolic))
    gen state = if null state.users
      then Nothing
      else pure $ CreateEntry
        <$> Gen.element state.users
        <*> Gen.string (Range.linear 1 10) chars
        <*> Gen.string (Range.linear 1 10) chars
    execute :: CreateEntry Concrete -> m BS8.ByteString
    execute createEntry = do
      req <- H.parseRequest (env.baseUrl <> "/entry")
      let req' = req
            { H.method = methodPost
            , H.redirectCount = 0
            , H.requestHeaders
              = formHeader
              : acceptHtml
              : mkAuthHeader createEntry.createEntryAuth
              : H.requestHeaders req
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm createEntry
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status303
      maybe
        (fail "No Location header returned")
        (pure . snd)
        $ find (\(h, _) -> h == "Location")
        $ H.responseHeaders res

cDeleteEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cDeleteEntry env = Command gen execute
  [ Require $ \state input ->
    input.deleteEntryAuth `elem` state.users &&
    input.deleteEntryId `elem` fmap testKey state.entries
  , Update $ \state input _output -> state
    { entries = filter (\e -> e.testKey /= input.deleteEntryId) state.entries
    }
  ]
  where
    gen :: ApiState v -> Maybe (gen (DeleteEntry v))
    gen state = if not (null state.users) && not (null state.entries)
      then Just $ DeleteEntry
        <$> Gen.element state.users
        <*> Gen.element (fmap testKey state.entries)
      else Nothing
    execute :: DeleteEntry Concrete -> m ()
    execute deleteEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete deleteEntry.deleteEntryId) <> "/delete"
      let req' = req
            { H.method = methodDelete
            , H.redirectCount = 0
            , H.requestHeaders
              = acceptHtml
              : mkAuthHeader deleteEntry.deleteEntryAuth
              : H.requestHeaders req
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status200

cUpdateEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cUpdateEntry env = Command gen execute
  [ Require $ \state input ->
    input.updateEntryAuth `elem` state.users &&
    input.updateEntryId `elem` fmap testKey state.entries
  , Update $ \state input _output ->
    let updateEntry u =
          if u.testKey == input.updateEntryId
          then u
            { testTitle = input.updateEntryTitle
            , testValue = input.updateEntryValue
            }
          else u
    in state { entries = fmap updateEntry state.entries }
  ]
  where
    gen :: ApiState v -> Maybe (gen (UpdateEntry v))
    gen state = if not (null state.users) && not (null state.entries)
      then Just $ UpdateEntry
        <$> Gen.element state.users
        <*> Gen.element (fmap testKey state.entries)
        <*> Gen.string (Range.linear 1 10) chars
        <*> Gen.string (Range.linear 1 10) chars
      else Nothing
    execute :: UpdateEntry Concrete -> m ()
    execute updateEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete updateEntry.updateEntryId) <> "/update"
      let req' = req
            { H.method = methodPut
            , H.redirectCount = 0
            , H.requestHeaders
              = acceptHtml
              : formHeader
              : mkAuthHeader updateEntry.updateEntryAuth
              : H.requestHeaders req
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm updateEntry
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status200

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
--       annotate $ show res
--       res.responseStatus === status204


cTestGetUsers :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cTestGetUsers env = Command gen execute
  [ Ensure $ \_oldState newState _input output ->
      sort (testUserEmail <$> newState.users) === sort (userEmail <$> output)
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
      , cGetEntry
      , cGetEntries
      , cCreateEntry
      , cDeleteEntry
      , cUpdateEntry
      ]