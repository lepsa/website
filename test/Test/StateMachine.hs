{-# LANGUAGE QuasiQuotes #-}

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
import Test.Db.Entry ()
import Test.Db.User ()
import Website.Auth.Authorisation qualified as Auth
import Website.Data.User
import Web.FormUrlEncoded
import Test.StateMachine.Types
import Data.Maybe (mapMaybe, fromMaybe, isJust)
import Text.Regex.TDFA
import Text.RawString.QQ
import qualified Data.ByteString.Lazy as BSL

-- Reference the following QFPL blog posts to refresh yourself.
-- https://qfpl.io/posts/intro-to-state-machine-testing-1/
-- https://qfpl.io/posts/intro-to-state-machine-testing-2/
-- https://qfpl.io/posts/intro-to-state-machine-testing-3/

--
-- Generators
--

genEmail :: MonadGen m => m Text
genEmail = Gen.text (Range.linear 1 100) Gen.unicode

genPassword :: MonadGen m => m Text
genPassword = Gen.text (Range.linear 1 100) Gen.unicode

genGroup :: MonadGen m => m Auth.Group
genGroup = Gen.element [Auth.Admin, Auth.User]

genTestUser :: MonadGen m => m (TestUser v)
genTestUser = TestUser <$> genEmail <*> genPassword <*> genGroup <*> pure Nothing

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
              [ ("Content-Type", "application/x-www-form-urlencoded")
              ]
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm $ toCreateUser register
            }
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
    -- Update the state as needed to reflect the new reality
  , Update $ \state _input _output -> state
    -- Test that the state updates and input/output matches what we are
    -- expecting to have occured.
  , Ensure $ \_oldState _newState _input output -> do
      output.responseStatus === status204
      let cookies = filter (\(k, v) -> k == mk "Set-Cookie" && not (BS.null v)) output.responseHeaders
      length cookies === 2
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
            , H.requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : H.requestHeaders req
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm login
            }
      liftIO $ H.httpNoBody req' env.manager

--
-- Side channel commands.
-- These are used to check that the server is doing what we expect in
-- multiple ways. Things like checking what the DB thinks the world
-- looks like vs what the API thinks the world looks like.
--

cTestReset :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cTestReset env = Command gen execute
  [ Update $ \_state _ _ -> initialState
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (Reset Symbolic))
    gen _apiState = Just $ pure Reset
    execute :: Reset Concrete -> m ()
    execute _reset = do
      req <- H.parseRequest (env.baseUrl <> "/reset")
      let req' = req {H.method = methodPost}
      annotate $ show req
      res <- liftIO $ H.httpNoBody req' env.manager
      annotate $ show res
      res.responseStatus === status204


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
      assert $ res.responseStatus == status200
      either fail pure $ eitherDecode @[User] res.responseBody

cGetEntries :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cGetEntries env = Command gen execute
  [ Require $ \state _input -> not $ null $ state.users >>= fromMaybe [] . testUserAuth
  , Ensure $ \_oldState newState _input output -> do
    length newState.entries === length output
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntries Symbolic))
    gen state =
      let auths = mapMaybe testUserAuth state.users
      in if null auths
        then Nothing
        else pure $ GetEntries <$> Gen.element auths
    execute :: GetEntries Concrete -> m [BSL.ByteString]
    execute getEntries = do
      req <- H.parseRequest (env.baseUrl <> "/entries")
      let req' = req
            { H.method = methodGet
            , H.requestHeaders = getEntries.getEntriesAuth <> H.requestHeaders req
            }
      res <- liftIO $ H.httpLbs req' env.manager
      assert $ res.responseStatus == status200
      let body = res.responseBody
      -- Commit the great sin of "parsing" html with regex
      pure $ getAllTextMatches (body =~ ([r|"/entry/\d+"|] :: String))


--
-- The prop test machine
--

propApiTests :: TestEnv -> IO Bool -> Property
propApiTests env reset = withTests 10 . property $ do
  let commands = ($ env) <$> [cLogin, cRegister, cTestGetUsers, cTestReset]
  actions <- forAll $ Gen.sequential (Range.linear 1 1000) initialState commands
  -- Once we have our set of actions, reset the API and begin to test it.
  -- The reset has to be performed here, not above `actions <- forAll...`
  -- otherwise it seems to only run for the first test, but any shrinks
  -- or changes don't get a fresh DB.
  worked' <- evalIO reset
  if worked' then pure () else fail "Could not reset the API, again"
  executeSequential initialState actions