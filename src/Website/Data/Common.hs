module Website.Data.Common where

import Control.Monad.Reader
import Data.Proxy
import Website.Types

-- | Values for a given form field
data FieldData
  = FieldData
      { label :: String,
        name :: String,
        type_ :: String,
        value :: Maybe String
      }
  | StaticData
      { label :: String,
        name :: String,
        value :: Maybe String
      }

-- | Overall data for creation and update forms.
data FormData = FormData
  { title :: String,
    createUrl :: Maybe String,
    updateUrl :: Maybe String,
    fields :: [FieldData]
  }

class GenerateForm a where
  newForm :: (MonadReader Env m) => Proxy a -> m FormData
  updateForm :: (MonadReader Env m) => a -> m FormData

ensureSingleResult :: (Applicative m) => [a] -> m (Either Err a)
ensureSingleResult [] = pure $ Left $ DbError NotFound
ensureSingleResult [a] = pure $ pure a
ensureSingleResult _ = pure $ Left $ DbError TooManyResults

ensureSingleInsert :: (Applicative m) => [a] -> m (Either Err a)
ensureSingleInsert [] = pure $ Left $ DbError FailedToInsertRecord
ensureSingleInsert [a] = pure $ pure a
ensureSingleInsert _ = pure $ Left $ Other "insert query returned multiple rows"