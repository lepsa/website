module Website.Data.Permission where
import           Control.Monad              (unless)
import           Control.Monad.Except       (MonadError (throwError))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Logger       (logDebug)
import           Control.Monad.Reader       (asks)
import           Data.Text                  (show)
import           Database.SQLite.Simple     (FromRow, Only (Only), field,
                                             fromRow, query)
import           Prelude                    (Eq, Ord, Read, Show, String, any,
                                             maybe, ($), (&&), (.), (<$>),
                                             (<*>), (<=), (<>), (>=))
import           Website.Auth.Authorisation (Access, Group (Anon))
import           Website.Data.Env           (HasEnv (conn))
import           Website.Data.Error         (AsErr (fromErr),
                                             Err (Unauthorised))
import           Website.Data.User          (OptionalUser,
                                             UserLogin (unUserLogin), mAuth, group)
import           Website.Types              (CanAppM)

checkPermission :: (OptionalUser c, CanAppM c e m) => String -> Access -> m ()
checkPermission name requested = do
  $(logDebug) $ "checkPermission " <> show name <> " " <> show requested
  -- Permissions are checked each time, rather than baking them
  -- into the JWT or initial checkup, as we want to have the
  -- most up to date information when we are doing the check
  -- even if it does hit the database more.
  userLogin <- asks mAuth
  permissions <- getPermissions name
  let check permission =
        -- Admins inherit all user permissions.
        -- This allows us to have "default" permissions
        -- while locking regular users out of some routes.
        maybe Anon ((.group) . unUserLogin) userLogin >= permission.permissionGroup &&
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
  unless allowed $ throwError $ fromErr Unauthorised

data Permission = Permission
  { permissionName   :: String
  , permissionGroup  :: Group
  , permissionAccess :: Access
  } deriving (Eq, Show, Read, Ord)
instance FromRow Permission where
  fromRow = Permission <$> field <*> field <*> field

getPermissions :: CanAppM c e m => String -> m [Permission]
getPermissions name = do
  $(logDebug) $ "getPermissions " <> show name
  c <- asks conn
  liftIO $ query c "select name, group_name, access from permission where name = ?" (Only name)
