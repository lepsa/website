module Website.Content.Entry where

import           CMark                       (commonmarkToHtml)
import           Control.Monad.Logger        (MonadLogger, logDebug)
import           Control.Monad.Reader        (MonadReader, asks)
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (pack, show)
import           Data.Time                   (TimeZone)
import           Prelude                     (mappend, mconcat, pure, ($), (.),
                                              (<$>), (<>), (>>=), const)
import           Servant                     (Proxy (Proxy),
                                              ToHttpApiData (toUrlPiece),
                                              safeLink)
import           Text.Blaze.Html             (Html, preEscapedToHtml, toHtml,
                                              (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Website.Content.Common      (AuthEntry, basicPage,
                                              editDeleteButtons, htmlLink,
                                              whenLoggedIn)
import           Website.Content.Htmx        (hxBoost, hxOn)
import           Website.Data.Entry          (Entry (created, key, title, updated, value),
                                              EntryCreate, EntryKey,
                                              sortEntriesByDateDesc)
import           Website.Data.Env            (HasEnv (timeZone))
import           Website.Data.User           (OptionalUser)
import           Website.Data.Util           (timeFormat)
import           Website.Network.API.CRUD    (CRUDFormRead, CRUDFormUpdateGet, CRUDFormDelete, CRUDFormCreatePost,)
import           Website.Network.API.Types   (topAPI)

-- | Display an entry, with edit and delete buttons
entryDisplay :: (HasEnv c, MonadReader c m, OptionalUser c, MonadLogger m) => Entry -> m Html
entryDisplay entry = do
  $(logDebug) $ "entryDisplay " <> show entry
  tz <- asks timeZone
  editDelete <- whenLoggedIn $ \_ -> editDeleteButtons
    "#entry"
    (pure $ ": " <> entry.title)
    (mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDFormUpdateGet EntryKey))) entry.key)
    (mappend "/" . toUrlPiece $ safeLink topAPI (Proxy @(AuthEntry (CRUDFormDelete EntryKey))) entry.key)
  pure
    $ H.div
      ! HA.id "entry"
    $ mconcat $ catMaybes
      [ pure $ H.h3 $ toHtml entry.title,
        pure $ H.p $ toHtml $ "Created: " <> timeFormat tz entry.created,
        H.p . toHtml . mappend "Updated: " . timeFormat tz <$> entry.updated,
        -- This isn't great, but it is the easiest way to do what I want.
        -- We store the markdown in the DB for easy editing, but we display
        -- HTML when looking at the entry.
        -- TODO: Test this for XSS and related issues.
        --       Especially around JS.
        -- TODO: Maybe we can pre-render the HTML in the DB so we aren't
        --       processing it every time.
        pure $ preEscapedToHtml $ commonmarkToHtml [] entry.value,
        editDelete
      ]

-- | As 'entryDisplay' with 'basicPage' wrapping
entryDisplayFullPage :: (HasEnv c, OptionalUser c, MonadLogger m, MonadReader c m) => Entry -> m Html
entryDisplayFullPage e = do
  $(logDebug) $ "entryDisplayFullPage " <> show e
  entryDisplay e >>= basicPage

-- | List all entries as a page
entryList :: (OptionalUser c, HasEnv c, MonadReader c m, MonadLogger m) => [Entry] -> m Html
entryList entries = do
  $(logDebug) $ "entryList " <> show entries
  tz <- asks timeZone
  mNewEntry <- whenLoggedIn $ const newEntry
  basicPage $
    mconcat $ catMaybes
      [ pure $ H.h2 "Entries",
        mNewEntry,
        pure $ H.ul $
          mconcat $
            H.li . H.p . entryLink tz <$> sortEntriesByDateDesc entries
      ]
  where
    newEntry :: Html
    newEntry = H.a
      ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! htmlLink (Proxy @(AuthEntry (CRUDFormCreatePost EntryCreate))) $ "Create Entry"

entryLink :: TimeZone -> Entry -> Html
entryLink tz entry = mconcat
  [ H.a
    ! hxBoost
    ! hxOn "::config-request" "setXsrfHeader(event)"
    ! HA.href (H.textValue $ pack "/" <> toUrlPiece (safeLink topAPI (Proxy @(AuthEntry (CRUDFormRead EntryKey))) entry.key))
    $ toHtml entry.title,
    toHtml $ " " <> timeFormat tz entry.created
  ]
