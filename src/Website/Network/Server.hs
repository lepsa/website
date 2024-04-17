module Website.Network.Server where

import Website.Network.API
import Website.Network.API.Types
import Website.Types
import Servant

server :: (CanAppM Env Err m) => FilePath -> ServerT TopAPI m
server currentDirectory =
  getIndex
    :<|> crudEntry
    :<|> getEntries
    :<|> serveDirectoryWebApp currentDirectory
  where
    crudEntry = 
           postEntry
      :<|> getEntryInitial
      :<|> getEntry
      :<|> putEntry
      :<|> getEntryForUpdate
      :<|> deleteEntry