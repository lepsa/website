{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.StateMachine where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Data.Text (Text)
import GHC.Generics
import Control.Monad.IO.Class

import Network.HTTP.Client qualified as H
import Control.Monad.Catch
import Data.ByteString qualified as BS
import Network.HTTP.Types
import Data.Text.Encoding (encodeUtf8)
import Data.CaseInsensitive (mk)

-- Reference the following QFPL blog posts to refresh yourself.
-- https://qfpl.io/posts/intro-to-state-machine-testing-1/
-- https://qfpl.io/posts/intro-to-state-machine-testing-2/
-- https://qfpl.io/posts/intro-to-state-machine-testing-3/

-- What we think that the state of the world should look like.
-- This will often end up mirroring the database in some way, as
-- we want to track the same information, to ensure that the server
-- is doing what we expect.
data ApiState v = ApiState
  { logins :: [Login v]
  , foo :: Int
  } deriving (Eq, Show, Generic)
instance FunctorB ApiState
instance TraversableB ApiState

data Env = Env
  { manager :: H.Manager
  , baseUrl :: String
  }

-- Log into the API with the given email and password
data Login v =
  Login Text Text
  deriving (Eq, Show, Generic)
instance FunctorB Login
instance TraversableB Login

-- Get the entries from the API
data GetEntries v = GetEntries
  deriving (Eq, Show, Generic)
instance FunctorB GetEntries
instance TraversableB GetEntries

genMkLogin :: MonadGen m => m (Login v)
genMkLogin = Login
  <$> Gen.text (Range.linear 1 100) Gen.unicode
  <*> Gen.text (Range.linear 1 100) Gen.unicode

cLogin :: forall gen m. (MonadGen gen, MonadThrow m, MonadIO m, MonadTest m) => Env -> Command gen m ApiState
cLogin env =
  -- From our state, generate a value to be used for the test.
  -- In this case, pick a user to log in from the list of users.
  let gen :: ApiState Symbolic -> Maybe (gen (Login Symbolic))
      gen apiState = if null (logins apiState)
        then Nothing
        else Just $ Gen.element $ logins apiState
      -- What we want to do with this value
      execute :: Login Concrete -> m (H.Response ())
      execute (Login user pass) = do
        req <- H.parseRequest (env.baseUrl <> "/login")
        let user' = encodeUtf8 user
            pass' = encodeUtf8 pass
            req' = H.applyBasicAuth user' pass' req
              { H.method = methodPost
              }
        liftIO $ H.httpNoBody req' env.manager
  in Command gen execute
    -- Check that the state is in a position where we can run this command
    [ Require $ \state _input -> not $ null $ logins state
    -- Update the state as needed to reflect the new reality
    , Update $ \state _input _output -> state
    -- Test that the state updates and input/output matches what we are
    -- expecting to have occured. 
    , Ensure $ \_oldState _newState _input output -> do
      output.responseStatus === status204
      let cookies = filter (\(k, v) -> k == mk "Set-Cookie" && not (BS.null v)) output.responseHeaders
      length cookies === 1
    ]

propApiTests :: Env -> IO () -> Property
propApiTests env reset = withTests 100 . property $ do
  let commands = ($ env) <$> [cLogin {-, cLogout, cGetEntries, cGetEntry, cDeleteEntry, ... -}]
      initialState = ApiState
        { logins = []
        , foo = 0
        }
  actions <- forAll $ Gen.sequential (Range.linear 1 1000) initialState commands
  evalIO reset
  executeSequential initialState actions
  