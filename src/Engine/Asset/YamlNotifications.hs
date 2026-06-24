{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlNotifications
    ( loadNotificationCfg
    , writeNotificationOverrides
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.!=), (.=)
                  , withObject, object)
import qualified Data.Aeson as Aeson
import Data.Foldable (toList)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Engine.Core.Log (LoggerState, logInfo, logWarn, LogCategory(..))
import Engine.PlayerEvent (NotificationCfg, CategoryCfg(..)
                          , PopupButton(..), PopupAction(..)
                          , defaultPopupButtons, parsePopupAction)

-- | Per-category checkbox triple. Used both for registry defaults
--   (under "default_settings") and for the player's overrides file
--   (one entry per category id at top level).
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

instance ToJSON CategorySettings where
    toJSON cs = object
        [ "log"   .= csLog cs
        , "popup" .= csPopup cs
        , "pause" .= csPause cs
        ]

-- | One button entry as it appears in the YAML registry under the
--   @buttons:@ list. Two fields: a visible label and an action tag
--   that maps to 'PopupAction' via 'parsePopupAction'.
data ButtonYaml = ButtonYaml
    { byLabel  ∷ !Text
    , byAction ∷ !Text
    } deriving (Show, Eq, Generic)

instance FromJSON ButtonYaml where
    parseJSON = withObject "ButtonYaml" $ \v → ButtonYaml
        ⊚ v .:  "label"
        ⊛ v .:  "action"

-- | One row of the YAML registry — what's shipped with the game.
data RegistryEntry = RegistryEntry
    { reId          ∷ !Text
    , reDisplayName ∷ !Text
    , reDescription ∷ !Text
    , reTextColor   ∷ !(Float, Float, Float, Float)
    , reDefaults    ∷ !CategorySettings
    , reButtons     ∷ ![ButtonYaml]
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
        btns    ← v .:? "buttons" .!= []
        popupCw ← v .:? "coalesce_window" .!= (0 ∷ Double)
        logCw   ← v .:? "log_coalesce_window" .!= (0 ∷ Double)
        col ← case rawCol of
            [r, g, b, a] → return (r, g, b, a)
            _            → fail $ "text_color must be [r,g,b,a]: "
                                    <> T.unpack rid
        return $ RegistryEntry rid disp desc col defs btns popupCw logCw

newtype RegistryFile = RegistryFile { rfCategories ∷ [RegistryEntry] }
    deriving (Show, Eq, Generic)

instance FromJSON RegistryFile where
    parseJSON = withObject "RegistryFile" $ \v → RegistryFile
        ⊚ v .: "categories"

-- | The overrides file's shape: @categories: { id: { log, popup, pause }, … }@.
newtype OverridesFile
    = OverridesFile { ofCategories ∷ HM.HashMap Text CategorySettings }
    deriving (Show, Eq, Generic)

instance FromJSON OverridesFile where
    parseJSON = withObject "OverridesFile" $ \v → OverridesFile
        ⊚ v .:? "categories" .!= HM.empty

instance ToJSON OverridesFile where
    toJSON (OverridesFile cats) = object [ "categories" .= cats ]

-- | Load the YAML registry, merge the player's overrides on top, and
--   return a resolved 'NotificationCfg'. If the overrides file doesn't
--   exist, write it out using the registry defaults so the player has
--   a file to edit. If the registry itself is missing or unparseable,
--   log a warning and return an empty map — every 'emitEvent' will
--   then drop with an unknown-category warning, which is the safe
--   loud-fail mode rather than crashing the engine.
loadNotificationCfg ∷ LoggerState
                    → FilePath        -- ^ registry path
                                      --   (data/notification_categories.yaml)
                    → FilePath        -- ^ overrides path
                                      --   (config/notifications.yaml)
                    → IO (NotificationCfg, [Text])
                       -- ^ (resolved map, registry-order list of ids)
loadNotificationCfg logger registryPath overridesPath = do
    eReg ← Yaml.decodeFileEither registryPath
    case eReg of
        Left err → do
            logWarn logger CatEvent $
                "Failed to load notification registry "
                  <> T.pack registryPath <> ": "
                  <> T.pack (show err)
            return (HM.empty, [])
        Right (RegistryFile entries) → do
            overrides ← loadOverrides logger overridesPath entries
            resolvedPairs ← forM entries $ \e → do
                cfg ← mkCategoryCfg logger e overrides
                return (reId e, cfg)
            let resolved = HM.fromList resolvedPairs
                order    = map reId entries
            logInfo logger CatEvent $
                "Notification registry loaded: "
                  <> T.pack (show (HM.size resolved))
                  <> " categories"
            return (resolved, order)

-- | Persist the current notification overrides to disk. Strips
--   registry-derived fields (display_name, description, text_color)
--   and writes only the per-category @{log, popup, pause}@ triples
--   under the @categories:@ key, matching the shape that
--   'loadOverrides' reads. Called from the settings tab on every
--   checkbox toggle.
writeNotificationOverrides ∷ FilePath → NotificationCfg → IO ()
writeNotificationOverrides path cfg = do
    createDirectoryIfMissing True (takeDirectory path)
    let toSettings c = CategorySettings
            { csLog   = ccLog   c
            , csPopup = ccPopup c
            , csPause = ccPause c
            }
        overrides = HM.map toSettings cfg
    Yaml.encodeFile path (OverridesFile overrides)

-- | Resolve one registry row against the player overrides. Missing
--   override entry → use registry defaults.
mkCategoryCfg ∷ LoggerState
              → RegistryEntry
              → HM.HashMap Text CategorySettings
              → IO CategoryCfg
mkCategoryCfg logger e overrides = do
    let cs = HM.lookupDefault (reDefaults e) (reId e) overrides
    buttons ← resolveButtons logger e
    return $ CategoryCfg
        { ccId          = reId e
        , ccDisplayName = reDisplayName e
        , ccDescription = reDescription e
        , ccTextColor   = reTextColor e
        , ccLog         = csLog   cs
        , ccPopup       = csPopup cs
        , ccPause       = csPause cs
        , ccButtons     = buttons
        , ccPopupCoalesceWindow = rePopupCoalesceWindow e
        , ccLogCoalesceWindow   = reLogCoalesceWindow e
        }

-- | Translate the YAML button list into 'PopupButton's, warning on
--   unknown action tags. An empty list (the YAML default if the
--   @buttons:@ key is omitted) falls back to 'defaultPopupButtons'
--   so every category always renders at least an OK.
resolveButtons ∷ LoggerState → RegistryEntry → IO [PopupButton]
resolveButtons logger e
    | null (reButtons e) = return defaultPopupButtons
    | otherwise = do
        resolved ← forM (reButtons e) $ \b →
            case parsePopupAction (byAction b) of
                Just act → return $ Just (PopupButton (byLabel b) act)
                Nothing  → do
                    logWarn logger CatEvent $
                        "Notification category '" <> reId e
                          <> "': unknown button action '"
                          <> byAction b
                          <> "'; button '" <> byLabel b
                          <> "' dropped"
                    return Nothing
        let buttons = catMaybes resolved
        if null buttons
            then return defaultPopupButtons
            else return buttons

-- | Load 'config/notifications.yaml' if present, else materialize it
--   from the registry defaults so the player has a file to edit.
loadOverrides ∷ LoggerState
              → FilePath
              → [RegistryEntry]
              → IO (HM.HashMap Text CategorySettings)
loadOverrides logger path entries = do
    exists ← doesFileExist path
    if not exists
        then do
            let defaults = HM.fromList
                    [ (reId e, reDefaults e) | e ← entries ]
            createDirectoryIfMissing True (takeDirectory path)
            Yaml.encodeFile path (OverridesFile defaults)
            logInfo logger CatEvent $
                "Wrote default notification overrides to "
                  <> T.pack path
            return defaults
        else do
            eOv ← Yaml.decodeFileEither path
            case eOv of
                Left err → do
                    logWarn logger CatEvent $
                        "Failed to parse notification overrides "
                          <> T.pack path <> ": "
                          <> T.pack (show err)
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
