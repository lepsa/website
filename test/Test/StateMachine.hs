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
    ( Header, status204, methodPost, methodGet, status200, status303 )
import Test.Db.Entry ()
import Test.Db.User ()
import Website.Auth.Authorisation qualified as Auth
import Website.Data.User
import Web.FormUrlEncoded
import Test.StateMachine.Types
import Data.Maybe (isJust)
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
genTestUser = TestUser <$> genEmail <*> genPassword <*> genGroup

--
-- Helper functions
--

mkAuthHeaders :: TestUser v -> Header
mkAuthHeaders user =
  ( "Authorization"
  , "Basic " <> encodeUtf8 (extractBase64 @_ @T.Text $ B64.encodeBase64 (user.testUserEmail <> ":" <> user.testUserPassword))
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
  , Ensure $ \_old _new _in out -> do
    length out === 2
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
      let cookies = filter (\(k, v) -> k == mk "Set-Cookie" && not (BS.null v)) res.responseHeaders
      pure cookies

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
              : mkAuthHeaders getEntries.getEntriesUser
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
    execute :: GetEntry Concrete -> m BSL.ByteString
    execute getEntry = do
      loc <- maybe
        (fail "Could not get Location for entry")
        (pure . BS8.unpack . snd)
        $ find (\(h, _) -> h == "Location")
        $ H.responseHeaders
        $ concrete getEntry.getEntryId
      req <- H.parseRequest (env.baseUrl <> loc)
      let req' = req
            { H.method = methodGet
            , H.requestHeaders
              = acceptHtml
              : mkAuthHeaders getEntry.getEntryAuth
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
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (CreateEntry Symbolic))
    gen state = if null state.users
      then Nothing
      else pure $ CreateEntry
        <$> Gen.element state.users
        <*> Gen.string (Range.linear 1 10) chars
        <*> Gen.string (Range.linear 1 10) chars
    execute :: CreateEntry Concrete -> m (H.Response BSL8.ByteString)
    execute createEntry = do
      req <- H.parseRequest (env.baseUrl <> "/entry")
      let req' = req
            { H.method = methodPost
            , H.redirectCount = 0
            , H.requestHeaders
              = formHeader
              : acceptHtml
              : mkAuthHeaders createEntry.createEntryAuth
              : H.requestHeaders req
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm createEntry
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status303
      maybe
        (fail "No Location header returned")
        (const $ pure ())
        $ find (\(h, _) -> h == "Location")
        $ H.responseHeaders res
      pure res

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
      , cTestGetUsers
      , cGetEntry
      , cGetEntries
      , cCreateEntry
      ]