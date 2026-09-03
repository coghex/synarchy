{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.Asset.YamlNotifications
    ( loadNotificationCfg
    , writeNotificationOverrides
    , OverridesFile
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.!=), (.=)
                  , withObject, object)
import System.Directory (doesFileExist)
import Engine.Core.ConfigWrite (writeConfigYaml)
import Engine.Core.Log (LoggerState, logInfo, logWarn, LogCategory(..))
import Engine.PlayerEvent (NotificationCfg, CategoryCfg(..))

-- | Per-category checkbox triple as the REGISTRY states it (under
--   "default_settings"): three concrete booleans. This is the BOTTOM
--   layer of the resolution, so a field the registry itself omits has
--   nothing beneath it to inherit and stays 'False' (#1938 requirement
--   4). The player's overrides file is a DIFFERENT shape — see
--   'CategoryOverride'.
data CategorySettings = CategorySettings
    { csLog   ∷ !Bool
    , csPopup ∷ !Bool
    , csPause ∷ !Bool
    } deriving (Show, Eq, Generic)

instance FromJSON CategorySettings where
    parseJSON = withObject "CategorySettings" $ \v → CategorySettings
        ⊚ v .:? "log"   .!= False
        ⊛ v .:? "popup" .!= False
        ⊛ v .:? "pause" .!= False

-- | One entry of the player's overrides file: an OPTIONAL value per
--   checkbox, layered ON TOP of that category's registry defaults
--   (#1938). 'Nothing' — the field omitted, or written as an explicit
--   YAML @null@, which aeson's '.:?' reads the same way — contributes
--   NO value and leaves the registry default standing; a present
--   boolean wins, @false@ included. A present but non-boolean value is
--   still a parse failure, so the fallback to the registry that
--   'loadOverrides' already performs is unchanged.
--
--   Splitting this from 'CategorySettings' is the whole fix: the two
--   types were one, so an omitted override field decoded as an explicit
--   @false@ and 'mkCategoryCfg' replaced the registry triple wholesale.
--   A file carrying only @{log: true}@ for a category defaulting to
--   @{log: true, popup: true, pause: true}@ therefore silently disabled
--   its popup and its automatic pause. This mirrors 'Engine.Save.Config'
--   (#913), which already resolves the @save@ family as a key-level
--   overlay for exactly this reason.
data CategoryOverride = CategoryOverride
    { coLog   ∷ !(Maybe Bool)
    , coPopup ∷ !(Maybe Bool)
    , coPause ∷ !(Maybe Bool)
    } deriving (Show, Eq, Generic)

instance FromJSON CategoryOverride where
    parseJSON = withObject "CategoryOverride" $ \v → CategoryOverride
        ⊚ v .:? "log"
        ⊛ v .:? "popup"
        ⊛ v .:? "pause"

-- | Emits only the fields the entry actually carries, so a sparse
--   override survives a read/write round trip as sparse rather than
--   being silently completed with the values it inherited. Every writer
--   in this module ('writeNotificationOverrides' and the absent-file
--   materialization) builds complete triples via 'fullOverride', so the
--   bytes they produce are unchanged.
instance ToJSON CategoryOverride where
    toJSON co = object $ catMaybes
        [ ("log"   .=) ⊚ coLog   co
        , ("popup" .=) ⊚ coPopup co
        , ("pause" .=) ⊚ coPause co
        ]

-- | The empty override: every field inherited. What a category ABSENT
--   from the overrides file resolves through.
noOverride ∷ CategoryOverride
noOverride = CategoryOverride Nothing Nothing Nothing

-- | A fully-authored override stating all three values explicitly —
--   what both writers here emit, so what they put on disk keeps
--   meaning exactly what it meant before #1938.
fullOverride ∷ CategorySettings → CategoryOverride
fullOverride cs = CategoryOverride
    { coLog   = Just (csLog   cs)
    , coPopup = Just (csPopup cs)
    , coPause = Just (csPause cs)
    }

-- | One row of the YAML registry — what's shipped with the game.
data RegistryEntry = RegistryEntry
    { reId          ∷ !Text
    , reDisplayName ∷ !Text
    , reDescription ∷ !Text
    , reTextColor   ∷ !(Float, Float, Float, Float)
    , reDefaults    ∷ !CategorySettings
    , rePopupCoalesceWindow ∷ !Double
    , reLogCoalesceWindow   ∷ !Double
    } deriving (Show, Eq, Generic)

instance FromJSON RegistryEntry where
    parseJSON = withObject "RegistryEntry" $ \v → do
        rid     ← v .:  "id"
        disp    ← v .:  "display_name"
        desc    ← v .:? "description" .!= ""
        rawCol  ← v .:? "text_color"  .!= [1.0, 1.0, 1.0, 1.0]
        defs    ← v .:? "default_settings"
                    .!= CategorySettings False False False
        popupCw ← v .:? "coalesce_window" .!= (0 ∷ Double)
        logCw   ← v .:? "log_coalesce_window" .!= (0 ∷ Double)
        col ← case rawCol of
            [r, g, b, a] → return (r, g, b, a)
            _            → fail $ "text_color must be [r,g,b,a]: "
                                    <> T.unpack rid
        return $ RegistryEntry rid disp desc col defs popupCw logCw

newtype RegistryFile = RegistryFile { rfCategories ∷ [RegistryEntry] }
    deriving (Show, Eq, Generic)

instance FromJSON RegistryFile where
    parseJSON = withObject "RegistryFile" $ \v → RegistryFile
        ⊚ v .: "categories"

-- | The overrides file's shape: @categories: { id: { log, popup, pause }, … }@,
--   where each of the three checkboxes is OPTIONAL (#1938).
newtype OverridesFile
    = OverridesFile { ofCategories ∷ HM.HashMap Text CategoryOverride }
    deriving (Show, Eq, Generic)

instance FromJSON OverridesFile where
    parseJSON = withObject "OverridesFile" $ \v → OverridesFile
        ⊚ v .:? "categories" .!= HM.empty

instance ToJSON OverridesFile where
    toJSON (OverridesFile cats) = object [ "categories" .= cats ]

-- | Load the YAML registry, merge the player's overrides on top
--   PER FIELD (#1938 — an override entry that omits a checkbox leaves
--   that checkbox at its registry default), and return a resolved
--   'NotificationCfg'. If the overrides file doesn't
--   exist, write it out using the registry defaults so the player has
--   a file to edit. If the registry itself is missing or unparseable,
--   log a warning and return an empty map — every 'emitEvent' will
--   then drop with an unknown-category warning, which is the safe
--   loud-fail mode rather than crashing the engine.
loadNotificationCfg ∷ LoggerState
                    → FilePath        -- ^ registry path
                                      --   (data/notification_categories.yaml)
                    → FilePath        -- ^ overrides path
                                      --   (config/notifications.local.yaml, #786)
                    → IO (NotificationCfg, [Text])
                       -- ^ (resolved map, registry-order list of ids)
loadNotificationCfg logger registryPath overridesPath = do
    eReg ← Yaml.decodeFileEither registryPath
    case eReg of
        Left err → do
            logWarn logger CatEvent $
                "Failed to load notification registry "
                  <> T.pack registryPath <> ": "
                  <> tshow err
            return (HM.empty, [])
        Right (RegistryFile entries) → do
            overrides ← loadOverrides logger overridesPath entries
            let resolvedPairs =
                    [ (reId e, mkCategoryCfg e overrides) | e ← entries ]
            let resolved = HM.fromList resolvedPairs
                order    = map reId entries
            logInfo logger CatEvent $
                "Notification registry loaded: "
                  <> tshow (HM.size resolved)
                  <> " categories"
            return (resolved, order)

-- | Persist the current notification overrides to disk. Strips
--   registry-derived fields (display_name, description, text_color)
--   and writes only the per-category @{log, popup, pause}@ triples
--   under the @categories:@ key, matching the shape that
--   'loadOverrides' reads. Called from the settings tab on every
--   checkbox toggle.
--
--   @Right ()@ when the file was durably replaced; @Left@ naming the
--   path and the cause when it was not (#2202). The write goes through
--   'writeConfigYaml', which also creates @config/@ when it is absent —
--   so a crash part way through can never leave a truncated
--   @config/notifications.local.yaml@ for the next boot to reject.
--
--   __Live state on a failed write (#2202).__ Unchanged from #786: the
--   caller merges into the live notification config FIRST and persists
--   afterwards, and a failed write does not roll that merge back. The
--   in-memory config is what routes the next emit; the YAML is the
--   next-session record.
writeNotificationOverrides ∷ FilePath → NotificationCfg → IO (Either Text ())
writeNotificationOverrides path cfg =
    writeConfigYaml path (OverridesFile overrides)
  where
    toSettings c = CategorySettings
        { csLog   = ccLog   c
        , csPopup = ccPopup c
        , csPause = ccPause c
        }
    overrides = HM.map (fullOverride . toSettings) cfg

-- | Resolve one registry row against the player overrides, FIELD BY
--   FIELD (#1938). The registry row is the base; each checkbox the
--   override entry actually states replaces its base value, and each one
--   it omits keeps it. A category with no override entry at all
--   therefore resolves entirely from the registry, exactly as before.
mkCategoryCfg ∷ RegistryEntry
              → HM.HashMap Text CategoryOverride
              → CategoryCfg
mkCategoryCfg e overrides =
    let base = reDefaults e
        co   = HM.lookupDefault noOverride (reId e) overrides
    in CategoryCfg
        { ccId          = reId e
        , ccDisplayName = reDisplayName e
        , ccDescription = reDescription e
        , ccTextColor   = reTextColor e
        , ccLog         = fromMaybe (csLog   base) (coLog   co)
        , ccPopup       = fromMaybe (csPopup base) (coPopup co)
        , ccPause       = fromMaybe (csPause base) (coPause co)
        , ccPopupCoalesceWindow = rePopupCoalesceWindow e
        , ccLogCoalesceWindow   = reLogCoalesceWindow e
        }

-- | Load 'config/notifications.local.yaml' (#786) if present, else
--   materialize it from the registry defaults so the player has a file
--   to edit. Hand-editing that file is an intended workflow, which is
--   why deleting a line from it must not change behaviour: the returned
--   map is the OVERRIDE layer, and a checkbox it does not mention is
--   left to 'mkCategoryCfg' to inherit (#1938).
loadOverrides ∷ LoggerState
              → FilePath
              → [RegistryEntry]
              → IO (HM.HashMap Text CategoryOverride)
loadOverrides logger path entries = do
    exists ← doesFileExist path
    if not exists
        then do
            let defaults = HM.fromList
                    [ (reId e, fullOverride (reDefaults e)) | e ← entries ]
            -- #2202: the defaults are already derived at this point, so
            -- a failed materialization costs the player only the
            -- editable file, never the boot. Report it and carry on with
            -- them; do NOT log the success line after a 'Left', and do
            -- not leave a partial file behind ('writeConfigYaml'
            -- publishes by rename or not at all).
            written ← writeConfigYaml path (OverridesFile defaults)
            case written of
                Right () → logInfo logger CatEvent $
                    "Wrote default notification overrides to "
                      <> T.pack path
                Left err → logWarn logger CatEvent $
                    "Could not write default notification overrides: "
                      <> err <> "; continuing with the registry defaults"
            return defaults
        else do
            eOv ← Yaml.decodeFileEither path
            case eOv of
                Left err → do
                    logWarn logger CatEvent $
                        "Failed to parse notification overrides "
                          <> T.pack path <> ": "
                          <> tshow err
                          <> "; using registry defaults"
                    return HM.empty
                Right (OverridesFile cats) → do
                    -- Drop entries that reference a category not in
                    -- the registry. The plan calls these out with a
                    -- dev-log warning rather than a silent drop.
                    let knownIds = HM.fromList
                            [ (reId e, ()) | e ← entries ]
                        (kept, dropped) =
                            HM.foldrWithKey
                                (\k v (k', d') →
                                    if HM.member k knownIds
                                       then (HM.insert k v k', d')
                                       else (k', k : d'))
                                (HM.empty, [])
                                cats
                    forM_ dropped $ \unknown →
                        logWarn logger CatEvent $
                            "Notification overrides reference unknown \
                            \category '" <> unknown <> "' in "
                              <> T.pack path <> "; ignored"
                    return kept
