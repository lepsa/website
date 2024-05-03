module Website.Data.Permission where
import Website.Types
import Website.Data.Env
import Website.Data.Error
import Website.Data.User
import Control.Monad.Reader
import Website.Auth.Authorisation
import Database.SQLite.Simple
import Control.Monad.Except

checkPermission :: CanAppM Env Err m => UserKey -> String -> Access -> m ()
checkPermission userKey name requested = do
  -- Permissions are checked each time, rather than baking them
  -- into the JWT or initial checkup, as we want to have the
  -- most up to date information when we are doing the check
  -- even if it does hit the database more.
  user <- getUser userKey
  permissions <- getPermissions name
  let check permission =
        -- Admins inherit all user permissions.
        -- This allows us to have "default" permissions
        -- while locking regular users out of some routes.
        user.group >= permission.permissionGroup &&
        -- Check that the permission is at least as powerful
        -- as what has been requested. The logic on this is
        -- that if you can write to a resource, you can basically
        -- do anything to it. Reading it has no side effects,
        -- and `None` means that the user should not be allowed
        -- to interact with it at all.
        requested <= permission.permissionAccess
      -- If no permissions are set, or none match, default to
      -- denying access to everyone. This is relying on the trade
      -- off of annoying a user or admin is far less costly than
      -- accidently leaving a route open somehow and having it be
      -- noticed and tampered with.
      allowed = any check permissions
  unless allowed $ throwError Unauthorised

data Permission = Permission
  { permissionName :: String
  , permissionGroup :: Group
  , permissionAccess :: Access
  } deriving (Eq, Show, Read, Ord)
instance FromRow Permission where
  fromRow = Permission <$> field <*> field <*> field

getPermissions :: CanAppM Env Err m => String -> m [Permission]
getPermissions name = do
  c <- asks conn
  liftIO $ query c "select name, group_name, access from permission where name = ?" (Only name)