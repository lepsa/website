{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.StateMachine where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString qualified as BS
import Data.CaseInsensitive (mk)
import Data.List (sort)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
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
import Data.Char (chr)

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
genTestUser = TestUser <$> genEmail <*> genPassword <*> genGroup

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
  [ Require $ \state _input -> not (null state.users),
    -- Update the state as needed to reflect the new reality
    Update $ \state _input _output -> state,
    -- Test that the state updates and input/output matches what we are
    -- expecting to have occured.
    Ensure $ \_oldState _newState _input output -> do
      output.responseStatus === status204
      let cookies = filter (\(k, v) -> k == mk "Set-Cookie" && not (BS.null v)) output.responseHeaders
      length cookies === 1
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
    execute (TestLogin user pass) = do
      req <- H.parseRequest (env.baseUrl <> "/login")
      let user' = encodeUtf8 user
          pass' = encodeUtf8 pass
          req' = H.applyBasicAuth user' pass' $ req { H.method = methodPost }
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
  [ Ensure $ \_oldState newState _input output -> sort (testUserEmail <$> newState.users) === sort (userEmail <$> output)
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetTestUsers Symbolic))
    gen _ = Just $ pure GetTestUsers
    execute :: GetTestUsers Concrete -> m [User]
    execute _ = do
      req <- H.parseRequest (env.baseUrl <> "/getUsers")
      let req' = req {H.method = methodGet}
      res <- liftIO $ H.httpLbs req' env.manager
      assert $ res.responseStatus == status200
      either fail pure $ eitherDecode @[User] res.responseBody

--
-- The prop test machine
--

propApiTests :: TestEnv -> IO Bool -> Property
propApiTests env reset = withTests 100 . property $ do
  worked <- evalIO reset
  if worked then pure () else fail "Could not reset the API"
  let commands = ($ env) <$> [cLogin, cRegister, cTestGetUsers, cTestReset]
  actions <- forAll $ Gen.sequential (Range.linear 1 1000) initialState commands
  executeSequential initialState actions
