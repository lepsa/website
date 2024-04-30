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
import Network.HTTP.Types
    ( Header, status204, methodPost, methodGet, status200, status303, methodDelete, methodPut, status401, Status )
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
import qualified Data.Text.Encoding.Base64 as B64
import Data.Base64.Types
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Char
import GHC.Records

-- Reference the following QFPL blog posts to refresh yourself.
-- https://qfpl.io/posts/intro-to-state-machine-testing-1/
-- https://qfpl.io/posts/intro-to-state-machine-testing-2/
-- https://qfpl.io/posts/intro-to-state-machine-testing-3/

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

genTestUser :: MonadGen m => m (TestUser v)
genTestUser = TestUser <$> genEmail <*> genPassword <*> genGroup <*> pure Nothing

--
-- Helper functions
--

emailExists :: (HasField "email" r Text) => ApiState v -> r -> Bool
emailExists state command = command.email `elem` fmap (.email) state.users

userExists :: (HasField "user" r (TestUser v), Eq1 v) => ApiState v -> r -> Bool
userExists state command = command.user `elem` state.users

entryExists :: (HasField "key" r (Var BS8.ByteString v), Eq1 v) => ApiState v -> r -> Bool
entryExists state command = command.key `elem` fmap (.key) state.entries

response204 :: (HasField "responseStatus" r Status, MonadTest m) => ApiState v -> ApiState v -> a -> r -> m ()
response204 _old _new _input output = output.responseStatus === status204

mkAuthHeader :: MonadTest m => TestUser Concrete -> m Header
mkAuthHeader user = do
  let auth = case user.jwt of
        Nothing -> mkBasicAuthHeader user
        Just jwt -> mkJwtAuthHeader jwt
  annotate $ show auth
  pure auth

mkBasicAuthHeader :: TestUser v -> Header
mkBasicAuthHeader user =
  ( "Authorization"
  , "Basic " <> encodeUtf8 (extractBase64 @_ @T.Text $ B64.encodeBase64 (user.email <> ":" <> user.password))
  )

mkJwtAuthHeader :: Var BS8.ByteString Concrete -> Header
mkJwtAuthHeader jwt =
  ( "Authorization"
  , "Bearer " <> concrete jwt
  )

mkBadAuth :: MonadGen m => ApiState v -> m (Maybe (TestUser v))
mkBadAuth state = Gen.choice
  [ pure Nothing
  , if null state.users
    then pure Nothing
    else do
      u <- Gen.element state.users
      let u' = u { jwt = Nothing }
      pure <$> Gen.choice
        [ do
          newPass <- Gen.filterT (/= u'.password) genPassword
          pure $ u' { password = newPass}
        , do
          newEmail <- Gen.filterT (/= u'.email) genEmail
          pure $ u' { email = newEmail}
        ]
  ]

mkBadAuthHeader :: (HasField "user" command (Maybe (TestUser Concrete)), MonadTest m) => command -> m ([Header] -> [Header])
mkBadAuthHeader command = maybe (pure id) (fmap (:) . mkAuthHeader) command.user

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
  [ Require emailExists
  , Update $ \state input _output -> state { users = input : state.users }
  , Ensure response204
  , Ensure $ \oldState newState input _output -> do
    assert $ notElem input.email $ (.email) <$> oldState.users
    assert $ elem input.email $ (.email) <$> newState.users
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
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm $ toUserCreate register
            }
      annotate $ show req'
      liftIO $ H.httpNoBody req' env.manager

cLogin :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cLogin env = Command gen execute
  -- Check that the state is in a position where we can run this command
  [ Require $ \state input -> isJust $ find
      (\u -> u.email == input.user && u.password == input.pass)
      state.users
  , Update $ \state input output ->
    let updateUser u = if u.email == input.user && u.password == input.pass
          then u { jwt = pure output }
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
        pure $ TestLogin user.email user.password
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
      -- and pulling new strings out of thin air.
      (\u -> u.email == input.user && u.password == input.pass)
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
              ( \u -> u.user `notElem` fmap (.email) apiState.users
                   && u.pass `notElem` fmap (.password) apiState.users
              )
              genRandom
            , do
              user <- Gen.element apiState.users
              TestLoginBad BadPassword user.email
                <$> Gen.filterT (\p -> p /= user.password) genPassword
            , do
              user <- Gen.element apiState.users
              TestLoginBad BadUser
                <$> Gen.filterT (\e -> e /= user.email) genEmail
                <*> pure user.password
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
  [ Require userExists
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
      auth <- mkAuthHeader getEntries.user
      let req' = req
            { H.method = methodGet
            , H.requestHeaders
              = acceptHtml
              : auth
              : H.requestHeaders req
            }
      annotate $ show auth
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status200
      let body = res.responseBody
      -- Commit the great sin of "parsing" html with regex
          matches = getAllTextMatches (body =~ ([r|"/entry/[[:digit:]]+"|] :: String))
      annotate $ show matches
      pure matches

cGetEntriesBadAuth :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cGetEntriesBadAuth env = Command gen execute
  []
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntriesBadAuth Symbolic))
    gen state = pure $ GetEntriesBadAuth <$> mkBadAuth state
    execute :: GetEntriesBadAuth Concrete -> m ()
    execute getEntries = do
      req <- H.parseRequest (env.baseUrl <> "/entries")
      auth <- mkBadAuthHeader getEntries
      let req' = req
            { H.method = methodGet
            , H.requestHeaders
              = auth $ acceptHtml : H.requestHeaders req
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status401

cGetEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cGetEntry env = Command gen execute
  [ Require userExists
  , Require entryExists
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntry Symbolic))
    gen state =
      let entries = (.key) <$> state.entries
      in if null state.users || null entries
        then Nothing
        else pure $ GetEntry <$> Gen.element state.users <*> Gen.element entries
    execute :: GetEntry Concrete -> m BSL8.ByteString
    execute getEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete getEntry.key)
      auth <- mkAuthHeader getEntry.user
      let req' = req
            { H.method = methodGet
            , H.requestHeaders
              = acceptHtml
              : auth
              : H.requestHeaders req
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status200
      pure res.responseBody

cGetEntryBadAuth :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cGetEntryBadAuth env = Command gen execute
  [ Require entryExists
  ]
  where
    gen :: ApiState Symbolic -> Maybe (gen (GetEntryBadAuth Symbolic))
    gen state =
      let entries = (.key) <$> state.entries
      in if null entries
        then Nothing
        else pure $ GetEntryBadAuth
          <$> Gen.element entries
          <*> mkBadAuth state
    execute :: GetEntryBadAuth Concrete -> m ()
    execute getEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete getEntry.key)
      auth <- mkBadAuthHeader getEntry
      let req' = req
            { H.method = methodGet
            , H.requestHeaders
              = auth $ acceptHtml : H.requestHeaders req
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status401

cCreateEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cCreateEntry env = Command gen execute
  [ Require userExists
  , Update $ \state input output -> state
    { entries = TestEntry output input.title input.title : state.entries
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
        <*> Gen.string textLength chars
        <*> Gen.string textLength chars
    execute :: CreateEntry Concrete -> m BS8.ByteString
    execute createEntry = do
      req <- H.parseRequest (env.baseUrl <> "/entry")
      auth <- mkAuthHeader createEntry.user
      let req' = req
            { H.method = methodPost
            , H.redirectCount = 0
            , H.requestHeaders
              = formHeader
              : acceptHtml
              : auth
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

cCreateEntryBadAuth :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cCreateEntryBadAuth env = Command gen execute
  []
  where
    gen :: ApiState Symbolic -> Maybe (gen (CreateEntryBadAuth Symbolic))
    gen state = if null state.users
      then Nothing
      else pure $ CreateEntryBadAuth
        <$> Gen.string textLength chars
        <*> Gen.string textLength chars
        <*> mkBadAuth state
    execute :: CreateEntryBadAuth Concrete -> m ()
    execute createEntry = do
      req <- H.parseRequest (env.baseUrl <> "/entry")
      auth <- mkBadAuthHeader createEntry
      let req' = req
            { H.method = methodPost
            , H.redirectCount = 0
            , H.requestHeaders
              = auth $ formHeader : acceptHtml : H.requestHeaders req
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm createEntry
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status401

cDeleteEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cDeleteEntry env = Command gen execute
  [ Require userExists
  , Require entryExists
  , Update $ \state input _output -> state
    { entries = filter (\e -> e.key /= input.key) state.entries
    }
  ]
  where
    gen :: ApiState v -> Maybe (gen (DeleteEntry v))
    gen state = if not (null state.users) && not (null state.entries)
      then Just $ DeleteEntry
        <$> Gen.element state.users
        <*> Gen.element ((.key) <$> state.entries)
      else Nothing
    execute :: DeleteEntry Concrete -> m ()
    execute deleteEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete deleteEntry.key) <> "/delete"
      auth <- mkAuthHeader deleteEntry.user
      let req' = req
            { H.method = methodDelete
            , H.redirectCount = 0
            , H.requestHeaders
              = acceptHtml
              : auth
              : H.requestHeaders req
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status200

cDeleteEntryBadAuth :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cDeleteEntryBadAuth env = Command gen execute
  [ Require entryExists
  ]
  where
    gen :: ApiState v -> Maybe (gen (DeleteEntryBadAuth v))
    gen state = if not (null state.users) && not (null state.entries)
      then Just $ DeleteEntryBadAuth
        <$> Gen.element ((.key) <$> state.entries)
        <*> mkBadAuth state
      else Nothing
    execute :: DeleteEntryBadAuth Concrete -> m ()
    execute deleteEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete deleteEntry.key) <> "/delete"
      auth <- mkBadAuthHeader deleteEntry
      let req' = req
            { H.method = methodDelete
            , H.redirectCount = 0
            , H.requestHeaders = auth $ acceptHtml : H.requestHeaders req
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status401

cUpdateEntry :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cUpdateEntry env = Command gen execute
  [ Require userExists
  , Require entryExists
  , Update $ \state input _output -> state { entries = updateEntry input <$> state.entries }
  ]
  where
    updateEntry :: Eq1 v => UpdateEntry v -> TestEntry v -> TestEntry v
    updateEntry input u =
      if u.key == input.key
      then u
        { title = input.title
        , value = input.value
        }
      else u
    gen :: ApiState v -> Maybe (gen (UpdateEntry v))
    gen state = if not (null state.users) && not (null state.entries)
      then Just $ UpdateEntry
        <$> Gen.element state.users
        <*> Gen.element ((.key) <$> state.entries)
        <*> Gen.string textLength chars
        <*> Gen.string textLength chars
      else Nothing
    execute :: UpdateEntry Concrete -> m ()
    execute input = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete input.key) <> "/update"
      auth <- mkAuthHeader input.user
      let req' = req
            { H.method = methodPut
            , H.redirectCount = 0
            , H.requestHeaders
              = acceptHtml
              : formHeader
              : auth
              : H.requestHeaders req
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm input
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
      res.responseStatus === status200

cUpdateEntryBadAuth :: forall gen m. CanStateM gen m => TestEnv -> Command gen m ApiState
cUpdateEntryBadAuth env = Command gen execute
  [ Require entryExists
  ]
  where
    gen :: ApiState v -> Maybe (gen (UpdateEntryBadAuth v))
    gen state = if not (null state.users) && not (null state.entries)
      then Just $ UpdateEntryBadAuth
        <$> Gen.element ((.key) <$> state.entries)
        <*> Gen.string textLength chars
        <*> Gen.string textLength chars
        <*> mkBadAuth state
      else Nothing
    execute :: UpdateEntryBadAuth Concrete -> m ()
    execute updateEntry = do
      req <- H.parseRequest $ env.baseUrl <> BS8.unpack (concrete updateEntry.key) <> "/update"
      auth <- mkBadAuthHeader updateEntry
      let req' = req
            { H.method = methodPut
            , H.redirectCount = 0
            , H.requestHeaders
              = auth $ acceptHtml : formHeader : H.requestHeaders req
            , H.requestBody = H.RequestBodyLBS $ urlEncodeForm $ toForm updateEntry
            }
      res <- liftIO $ H.httpLbs req' env.manager
      annotate $ show res
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
--       annotate $ show res
--       res.responseStatus === status204


cTestGetUsers :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m ApiState
cTestGetUsers env = Command gen execute
  [ Ensure $ \_oldState newState _input output ->
      sort ((.email) <$> newState.users) === sort ((.email) <$> output)
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
      , cGetEntryBadAuth
      , cGetEntries
      , cGetEntriesBadAuth
      , cCreateEntry
      , cCreateEntryBadAuth
      , cDeleteEntry
      , cDeleteEntryBadAuth
      , cUpdateEntry
      , cUpdateEntryBadAuth
      ]