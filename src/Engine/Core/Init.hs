{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Engine.Core.Init
  ( initializeEngine
  , initializeEngineHeadless
  , initializeEngineHeadlessWith
  , EngineInitResult(..)
  , resolveConfigPath
  , migrateLegacyConfig
  , LegacyNeutralityCheck(..)
  ) where

import UPrelude
import Data.IORef (newIORef)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON)
import Data.Proxy (Proxy(..))
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, try, displayException)
import qualified System.Random as Random
import qualified Combat.Types
import Engine.ActionOutcome (emptyActionOutcomeQueue)
import Engine.Asset.Types (defaultAssetPool)
import Engine.Asset.YamlNotifications (loadNotificationCfg, OverridesFile)
import Engine.PlayerEvent (emptyEventStore)
import Engine.Asset.TextureNameRegistry (emptyTextureNameRegistry)
import Engine.Core.ConfigWrite (copyConfigFile, writeConfigBytes)
import Engine.Core.Defaults
import Engine.Core.SessionEpoch (freshSessionGameTime)
import Engine.Core.Log (initLogger, defaultLogConfig, LogConfig(..)
                       , LogBackend(..), LoggerState, logInfo, logWarn
                       , LogCategory(..))
import System.IO (stdout)
import System.Directory (doesFileExist)
import qualified Data.ByteString as BS
import Engine.Core.State
import Engine.Save.Barrier (newSaveBarrier)
import Engine.Load.Status (newLoadStatusRef)
import Engine.Scene.Types (emptyLayeredQuads)
import Engine.Graphics.Solar (publishedSolar)
import Engine.Graphics.Vulkan.Sampler.Types (emptySamplerCache)
import Engine.Core.Types
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (defaultCamera, defaultUICamera)
import Engine.Graphics.Config (loadVideoConfig, VideoConfig(..), VideoConfigFile)
import Engine.Graphics.Font.Data (defaultFontCache)
import Engine.Input.Bindings (loadKeyBindings, KeyBindingConfig)
import Engine.Input.Types (defaultInputState)
import UI.ShellFocus (createFocusManager)
import UI.Types (emptyUIPageManager)
import Unit.Types (emptyUnitManager)
import Unit.Sim.Types (emptyUnitThreadState)
import Building.Types (emptyBuildingManager)
import Structure.Palette (emptyTexPalette)
import Structure.WallCatalog (emptyStructureWallCatalog)
import Structure.ArtCatalog (emptyStructureArtCatalog)
import Item.Types (emptyItemManager)
import Equipment.Types (emptyEquipmentClassManager)
import Substance.Types (emptySubstanceManager)
import Infection.Types (emptyInfectionManager)
import Craft.Types (emptyRecipeManager)
import Location.Types (emptyLocationRegistry)
import LootTable.Types (emptyLootTableRegistry)
import Tutorial.Types (emptyTutorialRegistry)
import World.Types (emptyWorldManager, emptyFloraCatalog)
import World.Material (emptyMaterialRegistry)
import World.Generate.Config (loadWorldGenConfig)
import Unit.Pathing.Config (loadPathingConfig)

data EngineInitResult = EngineInitResult
  { eirEnv ∷ EngineEnv }

-- | Prefer a local runtime config file over its versioned default
--   template (#638). @config/video.local.yaml@ / @config/keybinds.local.yaml@
--   (#786) are gitignored player state written by the settings UI's
--   Save actions; a fresh clone has neither, so boot falls back to the
--   tracked @_default.yaml@ template until the first Save creates the
--   local file.
--
--   A 'migrateLegacyConfig' upgrade also creates it — but ONLY when the
--   legacy file it found actually carries player state (#1937). A legacy
--   file that merely restates the template leaves this resolving the
--   template, on that boot and on every later one, so a revision of the
--   template still reaches a player who has never saved.
resolveConfigPath ∷ FilePath → FilePath → IO FilePath
resolveConfigPath localPath defaultPath = do
  hasLocal ← doesFileExist localPath
  return $ if hasLocal then localPath else defaultPath

-- | The two reference points that let 'migrateLegacyConfig' tell a
--   NEUTRAL PLACEHOLDER legacy file apart from one carrying real player
--   state (#1937). A subsystem that has no such reference (notifications,
--   which resolves an absent overrides file from the
--   @data/notification_categories.yaml@ registry rather than from a
--   tracked template) passes 'Nothing' and keeps the unconditional
--   copy-if-valid behaviour.
data LegacyNeutralityCheck = LegacyNeutralityCheck
  { lncDefaultPath ∷ FilePath
    -- ^ The tracked @_default.yaml@ template 'resolveConfigPath' would
    --   resolve if the legacy file were not there. Comparison is against
    --   this file DECODED as the subsystem's own config type, never
    --   against its bytes: formatting, key order, comments and omitted
    --   fields that the decoder defaults are all irrelevant to whether
    --   the legacy file says anything the template does not.
  , lncRecordPath  ∷ FilePath
    -- ^ Gitignored record of a legacy file already judged neutral, so
    --   the determination SURVIVES a later revision of the template. It
    --   is a verbatim copy of the legacy file as it stood when the
    --   judgement was made, and it is compared DECODED too — a legacy
    --   file whose meaning is unchanged is still the placeholder that
    --   was judged, however it is spelled. Without it, revising the
    --   template would make the untouched placeholder look like player
    --   state on the very next boot and get promoted after all, which
    --   is the defect this whole mechanism exists to stop.
    --
    --   Its name ends in @.local.yaml@ deliberately: it is gitignored
    --   per-machine runtime state that can decide what a boot resolves,
    --   so every fixture that already hides the developer's own
    --   @.local.yaml@ files under @config@ from an isolated run (the
    --   probe resource-root builders' @ignore_patterns@, the de-flake
    --   config manifest) covers it without being widened.
  }

-- | One-time upgrade from the pre-#786 tracked config layout.
--   @config/video.yaml@ / @config/keybinds.yaml@ / @config/notifications.yaml@
--   are kept tracked with content equal to the versioned default/registry
--   (never a real player's values) purely so a readable legacy file
--   always exists at first boot for this to migrate. A player who
--   actually saved through the old settings UI has a LOCALLY MODIFIED
--   copy of one of these paths, which git itself refuses to silently
--   overwrite during an update (it errors rather than deleting), so
--   their real values survive on disk until the update is resolved by
--   hand — at which point this picks them up.
--
--   Copying that tracked stand-in was never a no-op, because the copy
--   is DURABLE (#1937): it made the then-current template into
--   gitignored local state that outranks the template for ever after,
--   so a later revision of a shipped value never reached anyone who
--   booted once and never saved. (A newly ADDED field or action still
--   reached them — both decoders default what the file omits — but a
--   changed value for an already-shipped key did not.) So when a
--   'LegacyNeutralityCheck' is supplied, a legacy file that says
--   nothing its subsystem would not already resolve without it is
--   RECOGNISED rather than copied: the local file is left absent, the
--   determination is recorded, and the log line says so in words that
--   are not the migration line. A legacy file with genuinely different
--   content still migrates, exactly as before, and still logs
--   @Migrated legacy config \<legacy\> -> \<local\>@.
--
--   If the current-format local file is already present, this never
--   runs — that single existence gate is what makes migration
--   idempotent (the copy it makes is what every later boot finds) and
--   what guarantees a newer local file always wins over a stale legacy
--   one; nothing below re-examines, rewrites or removes it. The legacy
--   file is validated by decoding it as the SAME type its subsystem's
--   own loader expects (passed in via @Proxy@) rather than merely
--   checking it's syntactically valid YAML — a file that parses fine as
--   YAML but is missing a field the real loader requires (e.g. video's
--   @resolution@) must not be copied and logged as a successful
--   migration, since that would silently mask a load failure the loader
--   would otherwise report, AND permanently block any future migration
--   attempt (the existence gate above would see the copied-but-unusable
--   local file and never look at legacy again). A legacy file that
--   fails this check is left untouched and logged rather than copied.
--
--   Those SOURCE-SIDE failures (malformed, partial, schema-incomplete,
--   or unreadable — the last an access failure around the decode rather
--   than a property of the content) are not the only way a migration
--   fails, so they are diagnosed apart from the other class (#2210).
--   Once the check has PASSED, the copy to @localPath@ can still fail
--   on the DESTINATION — a read-only @config@ directory, a full disk,
--   the wrong permissions — which says nothing at all about the legacy
--   file, and gets its own warning naming the local path instead of
--   accusing a file that is fine.
--
--   Both classes reach the same outcome: the legacy file is untouched
--   ('copyConfigFile' never writes its source), no local file appears
--   (that copy publishes by atomic rename or not at all, #2202, so a
--   failed one leaves nothing behind to poison the existence gate
--   above), the boot falls back to the versioned default/registry
--   exactly like a missing legacy file, and the migration is
--   re-attempted on the next boot.
migrateLegacyConfig ∷ ∀ a. (FromJSON a, Eq a)
                    ⇒ Proxy a → LoggerState → Maybe LegacyNeutralityCheck
                    → FilePath → FilePath → IO ()
migrateLegacyConfig _ logger mCheck legacyPath localPath = do
  hasLocal ← doesFileExist localPath
  unless hasLocal $ do
    hasLegacy ← doesFileExist legacyPath
    when hasLegacy $ do
      -- SOURCE-SIDE only: reading the legacy file, decoding it as the
      -- subsystem's own type, and (when a check is supplied) judging it
      -- against the template. Every failure this spans is a property of
      -- the legacy file or of reading it, which is exactly what the
      -- warning below diagnoses. 'True' means "copy it"; the copy is
      -- deliberately NOT inside this 'try' (#2210), because a
      -- destination write failure is not a bad legacy file.
      outcome ← try $ do
        eVal ← Yaml.decodeFileEither legacyPath
        case (eVal ∷ Either Yaml.ParseException a) of
          Left err → ioError $ userError $ show err
          Right val → case mCheck of
            Nothing → return True
            Just check → do
              neutral ← legacyIsNeutral check val
              if neutral
                then False <$ recordNeutralLegacy logger legacyPath
                                                  (lncRecordPath check)
                else return True
      case (outcome ∷ Either SomeException Bool) of
        -- #2202: the copy is DURABLE — a temporary in the destination's
        -- own directory, fsync, atomic rename, fsync the directory — so
        -- an interrupted copy can never leave a partial local file. That
        -- matters most HERE: migration is gated on the local file's mere
        -- EXISTENCE, so one partial copy used to suppress every later
        -- migration attempt permanently. 'copyConfigFile' reports rather
        -- than throws, which is exactly the shape #2210's
        -- destination-blaming arm already wanted.
        Right True → do
          copied ← copyConfigFile legacyPath localPath
          case copied of
            Right () → logInfo logger CatInit $
              "Migrated legacy config " <> T.pack legacyPath
                <> " -> " <> T.pack localPath
            Left err → logWarn logger CatInit $
              "Legacy config " <> T.pack legacyPath
                <> " is valid, but writing it to " <> T.pack localPath
                <> " failed; the destination could not be written. The "
                <> "legacy file is untouched and the boot falls back to "
                <> "the versioned default, so the migration is retried "
                <> "on the next boot once the destination is writable: "
                <> err
        Right False → logInfo logger CatInit $
          "Legacy config " <> T.pack legacyPath
            <> " carries no player state (it resolves to the same "
            <> "values as the versioned default); leaving "
            <> T.pack localPath
            <> " absent so later default changes still apply"
        Left e → logWarn logger CatInit $
          "Legacy config " <> T.pack legacyPath
            <> " could not be migrated (malformed, partial, "
            <> "schema-incomplete, or unreadable); falling back to "
            <> "the versioned default: " <> T.pack (displayException e)

-- | Decode @path@ as one subsystem's own config type. 'Nothing' when the
--   file is absent, unreadable, or fails that decode — none of which can
--   PROVE a legacy file neutral, so every one of them leaves
--   'migrateLegacyConfig' migrating exactly as it did before #1937.
--   Suppression is only ever reached by a positive match.
decodeConfigMaybe ∷ ∀ b. FromJSON b ⇒ FilePath → IO (Maybe b)
decodeConfigMaybe path = do
  exists ← doesFileExist path
  if not exists
    then return Nothing
    else do
      outcome ← try (Yaml.decodeFileEither path)
      return $ case (outcome ∷ Either SomeException
                              (Either Yaml.ParseException b)) of
        Right (Right v) → Just v
        _               → Nothing

-- | Is this decoded legacy value a neutral placeholder — either
--   equivalent to the template the boot would resolve without it, or
--   equivalent to the legacy content a previous boot already judged
--   neutral? The second arm is what keeps a revision of the template
--   from re-promoting an untouched placeholder.
legacyIsNeutral ∷ ∀ b. (FromJSON b, Eq b) ⇒ LegacyNeutralityCheck → b → IO Bool
legacyIsNeutral check val = do
  mDefault ← decodeConfigMaybe (lncDefaultPath check)
  if mDefault ≡ Just val
    then return True
    else do
      mRecorded ← decodeConfigMaybe (lncRecordPath check)
      return (mRecorded ≡ Just val)

-- | Persist "this exact legacy content was judged neutral" as a verbatim
--   copy of the legacy file at the gitignored record path. Rewritten
--   only when the content actually differs, so a repeat boot touches no
--   file at all. A failure to write is a warning, never a boot failure:
--   the next boot simply re-derives the judgement from the template.
recordNeutralLegacy ∷ LoggerState → FilePath → FilePath → IO ()
recordNeutralLegacy logger legacyPath recordPath = do
  outcome ← try $ do
    legacyBytes ← BS.readFile legacyPath
    hasRecord ← doesFileExist recordPath
    stale ← if hasRecord
              then (≢ legacyBytes) ⊚ BS.readFile recordPath
              else return True
    when stale $ writeConfigOrFail recordPath legacyBytes
  case (outcome ∷ Either SomeException ()) of
    Right () → return ()
    Left e   → logWarn logger CatInit $
      "Could not record the neutral-placeholder determination for "
        <> T.pack legacyPath <> " at " <> T.pack recordPath
        <> "; a later change to the versioned default may re-examine it: "
        <> T.pack (displayException e)

-- | Durably write the neutrality record (#2202), raising on failure so
--   'recordNeutralLegacy's existing warning-and-continue handler reports
--   the cause — the throw is how the outcome is CONSUMED rather than
--   discarded. Durability matters here for the same reason it matters
--   for the migration copy above: a truncated record decodes as
--   'Nothing', which re-promotes the very placeholder #1937 exists to
--   suppress.
writeConfigOrFail ∷ FilePath → BS.ByteString → IO ()
writeConfigOrFail path bytes = do
  written ← writeConfigBytes path bytes
  either (ioError ∘ userError ∘ T.unpack) pure written

-- | Allocate every 'IORef', queue, and subsystem, then bundle into
--   'EngineEnv'. Logs to stdout (the graphical default).
initializeEngine ∷ IO EngineInitResult
initializeEngine = initializeEngineWith (LogToHandle stdout)

-- | As 'initializeEngine' but with an explicit log backend. Dump mode
--   passes 'stderr' here so the logger is born writing to stderr —
--   init-time logging (e.g. 'loadNotificationCfg') can never reach
--   stdout, which dump mode reserves for clean JSON. Redirecting the
--   backend after init returns would be too late (the line is already
--   emitted).
initializeEngineWith ∷ LogBackend → IO EngineInitResult
initializeEngineWith logBackend = do
  inputQueue ← Q.newQueue
  inputBarrierNextRef ← newTVarIO (0 ∷ Int)
  inputBarrierRef ← newTVarIO (0 ∷ Int)
  worldQueue ← Q.newQueue
  simQueue ← Q.newQueue
  luaToEngineQueue ← Q.newQueue
  engineToLuaQueue ← Q.newQueue
  screenshotRequestQueue ← Q.newQueue
  bloodDisposeQueue ← Q.newQueue
  
  lifecycleRef ← newIORef EngineStarting
  fpsRef ← newIORef 0.0
 
  logger ← initLogger defaultLogConfig { lcBackend = logBackend }
  loggerRef ← newIORef logger
  
  assetPool ← defaultAssetPool
  assetPoolRef ← newIORef assetPool
  nextObjectIdRef ← newIORef 0
  -- Item-instance ids start at 1 (0 is the "unassigned" sentinel); a
  -- load ASSIGNS the counter from sdNextItemInstanceId, never max'ing
  -- against this session's value (#67, reshaped by #763 -- see
  -- 'World.Load.Publish').
  nextItemInstanceIdRef ← newIORef 1
  texNameRegRef ← newIORef emptyTextureNameRegistry
  
  inputStateRef ← newIORef defaultInputState
  migrateLegacyConfig (Proxy ∷ Proxy KeyBindingConfig) logger
    (Just LegacyNeutralityCheck
       { lncDefaultPath = "config/keybinds_default.yaml"
       , lncRecordPath  = "config/keybinds.legacy-neutral.local.yaml" })
    "config/keybinds.yaml" "config/keybinds.local.yaml"
  keybindsPath ← resolveConfigPath "config/keybinds.local.yaml" "config/keybinds_default.yaml"
  keyBindings ← loadKeyBindings logger keybindsPath
  keyBindingsRef ← newIORef keyBindings
  currentKeyDownRef ← newIORef Nothing

  migrateLegacyConfig (Proxy ∷ Proxy VideoConfigFile) logger
    (Just LegacyNeutralityCheck
       { lncDefaultPath = "config/video_default.yaml"
       , lncRecordPath  = "config/video.legacy-neutral.local.yaml" })
    "config/video.yaml" "config/video.local.yaml"
  videoConfigPath ← resolveConfigPath "config/video.local.yaml" "config/video_default.yaml"
  videoConfig ← loadVideoConfig logger videoConfigPath
  videoConfigRef ← newIORef $ videoConfig
  windowSizeRef ← newIORef (vcWidth videoConfig, vcHeight videoConfig)
  windowPosRef ← newIORef (0, 0)
  -- Seeded by Engine.Graphics.Window.GLFW.createWindow from what GLFW
  -- actually did, not from vcWindowMode: a fullscreen OR borderless
  -- request can degrade to a plain window, and a creation that DID apply
  -- either mode also seeds the windowed cache there, from the decorated
  -- window it sampled before mutating (#907, #1731, #1882).
  windowStateRef ← newIORef defaultWindowState
  framebufferSizeRef ← newIORef (vcWidth videoConfig, vcHeight videoConfig)
  framebufferMinimizeGenRef ← newIORef 0
  brightnessRef ← newIORef (vcBrightness videoConfig)
  pixelSnapRef ← newIORef (vcPixelSnap videoConfig)
  textureFilterRef ← newIORef (vcTextureFilter videoConfig)
  
  cameraRef ← newIORef defaultCamera
  uiCameraRef ← newIORef $ defaultUICamera (fromIntegral (vcWidth videoConfig))
                                           (fromIntegral (vcHeight videoConfig))
  uiManagerRef ← newIORef emptyUIPageManager
  worldManagerRef ← newIORef emptyWorldManager
  hudActivePageRef ← newIORef Nothing
  loadStatusRef ← newLoadStatusRef
  pendingLoadRef ← newIORef Nothing
  focusMgrRef ← newIORef createFocusManager
  textBuffersRef ← newIORef Map.empty
  fontCache ← newIORef defaultFontCache
  -- Start at noon, published rather than overridden (#1869).
  sunAngleRef ← newIORef (publishedSolar 0.25)
  worldPreviewRef ← newIORef Nothing
  worldPreviewGenerationRef ← newIORef 0
  zoomAtlasDataRef ← newIORef Nothing
  worldQuadsRef ← newIORef emptyLayeredQuads
  -- Scene-assembly telemetry (#1921): unavailable until the first
  -- completed 'updateWorldTiles' pass publishes into it.
  sceneStatsRef ← newIORef Nothing
  textureSystemRef ← newIORef Nothing
  samplerCacheRef ← newIORef emptySamplerCache
  texSizeRef ← newIORef HM.empty
  -- #2020: no device exists yet at engine-init time, so the map-image
  -- ceiling starts absent. Vulkan init publishes it; a GPU-free boot
  -- mode leaves it absent on purpose.
  maxImgDimRef ← newIORef Nothing
  defaultFaceMapSlotRef ← newIORef 0
  floraCatRef ← newIORef emptyFloraCatalog
  materialRegistryRef ← newIORef emptyMaterialRegistry
  unitManagerRef ← newIORef emptyUnitManager
  unitQueue ← Q.newQueue
  utsRef ← newIORef emptyUnitThreadState
  statRNGRef ← Random.newStdGen ⌦ newIORef
  buildingManagerRef ← newIORef emptyBuildingManager
  texPaletteRef ← newIORef emptyTexPalette
  structureWallCatalogRef ← newIORef emptyStructureWallCatalog
  structureArtCatalogRef ← newIORef emptyStructureArtCatalog
  texPaletteHandlesRef ← newIORef HM.empty
  buildingQueue ← Q.newQueue
  buildingGhostRef ← newIORef Nothing
  combatQueue ← Q.newQueue
  combatEventsRef ← newIORef Combat.Types.emptyEventQueue
  injuryEventsRef ← newIORef Combat.Types.emptyEventQueue
  thoughtEventsRef ← newIORef Combat.Types.emptyEventQueue
  actionOutcomeRef ← newIORef emptyActionOutcomeQueue
  worldGenConfig ← loadWorldGenConfig logger "config/world_gen_default.yaml"
  worldGenConfigRef ← newIORef worldGenConfig
  pathingConfig ← loadPathingConfig logger "config/pathing.yaml"
  pathingConfigRef ← newIORef pathingConfig

  enginePausedRef ← newIORef False
  -- #913: starts at 0 and only ever counts UP; an autosave compares a
  -- snapshot of it, never its absolute value.
  playerIntentGenRef ← newMVar (0 ∷ Word64)
  -- #1730: the same shape for pause assertions the engine makes on its
  -- own behalf, read only under the mutex above.
  enginePauseGenRef ← newIORef (0 ∷ Word64)
  -- #2291: the fresh-session epoch, shared with the Exit-to-Menu
  -- reset in "Unit.Thread" so a world created after a session
  -- teardown starts from the same reading the first one did.
  gameTimeRef     ← newIORef freshSessionGameTime
  saveBarrierRef  ← newSaveBarrier
  inputThreadActiveRef ← newIORef False
  -- Seeded to the POSIX epoch so the first save uses the real wall
  -- clock; subsequent saves clamp against it for monotonic, distinct
  -- timestamps (#98).
  lastSaveTimeRef ← newIORef (UTCTime (fromGregorian 1970 1 1) 0)
  itemManagerRef  ← newIORef emptyItemManager
  equipmentClassManagerRef ← newIORef emptyEquipmentClassManager
  substanceManagerRef ← newIORef emptySubstanceManager
  infectionManagerRef ← newIORef emptyInfectionManager
  recipeManagerRef ← newIORef emptyRecipeManager
  locationDefsRef ← newIORef emptyLocationRegistry
  lootTableRegistryRef ← newIORef emptyLootTableRegistry
  tutorialRegistryRef ← newIORef emptyTutorialRegistry
  -- Player Events: load the notification registry (data/) merged
  -- with player overrides (config/), allocate the ring buffer and
  -- popup queue. Both are STM TVars, so a push from any thread is
  -- safe; the emitters that exist today are the world thread and the
  -- Lua thread, via Engine.PlayerEvent.emitEvent. The cfg IORef
  -- is updated at runtime by the Phase 2 notifications settings tab.
  -- Notifications get no 'LegacyNeutralityCheck' (#1937): they have no
  -- tracked @_default.yaml@ to be neutral AGAINST, and an absent
  -- overrides file already defers to the
  -- @data/notification_categories.yaml@ registry both before and after
  -- any registry revision, so the copy here was already harmless.
  migrateLegacyConfig (Proxy ∷ Proxy OverridesFile) logger Nothing
    "config/notifications.yaml" "config/notifications.local.yaml"
  (notificationCfg0, notificationOrder) ← loadNotificationCfg logger
                        "data/notification_categories.yaml"
                        "config/notifications.local.yaml"
  notificationCfgRef ← newIORef notificationCfg0
  eventStoreRef ← newTVarIO emptyEventStore
  popupQueueRef ← newTVarIO Seq.empty
  engineStateRef ← newIORef defaultEngineState
  let env = EngineEnv
        { engineConfig       = defaultEngineConfig
        , engineStateRef     = engineStateRef
        , videoConfigRef     = videoConfigRef
        , windowSizeRef      = windowSizeRef
        , windowPosRef       = windowPosRef
        , windowStateRef     = windowStateRef
        , framebufferSizeRef = framebufferSizeRef
        , framebufferMinimizeGenRef = framebufferMinimizeGenRef
        , fpsRef             = fpsRef
        , brightnessRef      = brightnessRef
        , pixelSnapRef       = pixelSnapRef
        , textureFilterRef   = textureFilterRef
        , inputQueue         = inputQueue
        , inputBarrierNextRef = inputBarrierNextRef
        , inputBarrierRef    = inputBarrierRef
        , loggerRef          = loggerRef
        , luaToEngineQueue   = luaToEngineQueue
        , luaQueue           = engineToLuaQueue
        , lifecycleRef       = lifecycleRef
        , assetPoolRef       = assetPoolRef
        , textureNameRegistryRef = texNameRegRef
        , nextObjectIdRef    = nextObjectIdRef
        , nextItemInstanceIdRef = nextItemInstanceIdRef
        , fontCacheRef       = fontCache
        , inputStateRef      = inputStateRef
        , keyBindingsRef     = keyBindingsRef
        , currentKeyDownRef  = currentKeyDownRef
        , textBuffersRef     = textBuffersRef
        , cameraRef          = cameraRef
        , uiCameraRef        = uiCameraRef
        , uiManagerRef       = uiManagerRef
        , worldManagerRef    = worldManagerRef
        , hudActivePageRef   = hudActivePageRef
        , loadStatusRef      = loadStatusRef
        , pendingLoadRef     = pendingLoadRef
        , worldQueue         = worldQueue
        , focusManagerRef    = focusMgrRef
        , sunAngleRef        = sunAngleRef
        , worldPreviewRef    = worldPreviewRef
        , worldPreviewGenerationRef = worldPreviewGenerationRef
        , zoomAtlasDataRef   = zoomAtlasDataRef
        , screenshotRequestQueue = screenshotRequestQueue
        , worldQuadsRef      = worldQuadsRef
        , sceneStatsRef      = sceneStatsRef
        , textureSystemRef   = textureSystemRef
        , samplerCacheRef    = samplerCacheRef
        , textureSizeRef     = texSizeRef
        , maxImageDimensionRef = maxImgDimRef
        , bloodDisposeQueue  = bloodDisposeQueue
        , defaultFaceMapSlotRef = defaultFaceMapSlotRef
        , floraCatalogRef    = floraCatRef
        , materialRegistryRef = materialRegistryRef
        , unitManagerRef     = unitManagerRef
        , unitQueue          = unitQueue
        , utsRef             = utsRef
        , statRNGRef         = statRNGRef
        , buildingManagerRef = buildingManagerRef
        , texPaletteRef      = texPaletteRef
        , structureWallCatalogRef = structureWallCatalogRef
        , structureArtCatalogRef = structureArtCatalogRef
        , texPaletteHandlesRef = texPaletteHandlesRef
        , buildingQueue      = buildingQueue
        , combatQueue        = combatQueue
        , combatEventsRef    = combatEventsRef
        , injuryEventsRef    = injuryEventsRef
        , thoughtEventsRef   = thoughtEventsRef
        , actionOutcomeRef   = actionOutcomeRef
        , buildingGhostRef   = buildingGhostRef
        , worldGenConfigRef  = worldGenConfigRef
        , pathingConfigRef   = pathingConfigRef
        , simQueue          = simQueue
        , enginePausedRef   = enginePausedRef
        , playerIntentGenRef = playerIntentGenRef
        , enginePauseGenRef  = enginePauseGenRef
        , gameTimeRef       = gameTimeRef
        , saveBarrierRef    = saveBarrierRef
        , inputThreadActiveRef = inputThreadActiveRef
        , lastSaveTimeRef   = lastSaveTimeRef
        , itemManagerRef    = itemManagerRef
        , equipmentClassManagerRef = equipmentClassManagerRef
        , substanceManagerRef      = substanceManagerRef
        , infectionManagerRef      = infectionManagerRef
        , recipeManagerRef         = recipeManagerRef
        , locationDefsRef    = locationDefsRef
        , lootTableRegistryRef = lootTableRegistryRef
        , tutorialRegistryRef = tutorialRegistryRef
        , eventStoreRef      = eventStoreRef
        , notificationCfgRef = notificationCfgRef
        , notificationOrder  = notificationOrder
        , popupQueueRef      = popupQueueRef
        }

  pure $ EngineInitResult env

-- | Like 'initializeEngine' but sets 'ecHeadless' — no window or GPU.
--   Logs to stdout (the shell redirects it to a file in the --headless
--   workflow).
initializeEngineHeadless ∷ IO EngineInitResult
initializeEngineHeadless = initializeEngineHeadlessWith (LogToHandle stdout)

-- | As 'initializeEngineHeadless' but with an explicit log backend.
--   Dump mode passes 'stderr' so stdout stays clean JSON.
initializeEngineHeadlessWith ∷ LogBackend → IO EngineInitResult
initializeEngineHeadlessWith logBackend = do
  result ← initializeEngineWith logBackend
  let env = eirEnv result
      headlessEnv = env { engineConfig = (engineConfig env)
                            { ecHeadless = True
                            -- Keep 'ecBootMode' honest for anything that
                            -- reads this env before a boot path stamps
                            -- its own (the headless test harness never
                            -- calls App.Boot.bootConfig at all); --dump
                            -- shares this initializer and overwrites it
                            -- with 'ModeDump'.
                            , ecBootMode = ModeHeadless } }
  pure $ result { eirEnv = headlessEnv }
