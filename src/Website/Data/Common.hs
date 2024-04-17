module Website.Data.Common where

import Data.Proxy

-- |Values for a given form field
data FieldData = FieldData
  { label :: String,
    name :: String,
    type_ :: String,
    value :: Maybe String
  }

-- |Overall data for creation and update forms.
data FormData = FormData
  { title :: String,
    createUrl :: Maybe String,
    updateUrl :: Maybe String,
    fields :: [FieldData]
  }

class GenerateForm a where
  newForm :: Proxy a -> FormData
  updateForm :: a -> FormData