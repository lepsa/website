module Website.Data.Util where
import Website.Data.Error

ensureSingleResult :: (Applicative m) => [a] -> m (Either Err a)
ensureSingleResult [] = pure $ Left $ DbError NotFound
ensureSingleResult [a] = pure $ pure a
ensureSingleResult _ = pure $ Left $ DbError TooManyResults

ensureSingleInsert :: (Applicative m) => [a] -> m (Either Err a)
ensureSingleInsert [] = pure $ Left $ DbError FailedToInsertRecord
ensureSingleInsert [a] = pure $ pure a
ensureSingleInsert _ = pure $ Left $ Other "insert query returned multiple rows"