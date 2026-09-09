module Engine.Scripting.Lua.API.Register.Engine
  ( registerEngineAPI
  ) where

import UPrelude
import Engine.Scripting.Lua.Types (LuaBackendState)
import Engine.Scripting.Lua.CallStats (LuaCallStats)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Core (loadScriptFn, killScriptFn,
                                      setTickIntervalFn, pauseScriptFn,
                                      resumeScriptFn, quitFn, getFPSFn,
                                      listFilesFn, listFilesRecursiveFn,
                                      setPausedFn, isPausedFn,
                                      getBootProfileFn, getPreviewTargetFn,
                                      getPreviewBrowseFn,
                                      realTimeFn, gameTimeFn)
import Engine.Scripting.Lua.API.Debug (showDebugFn, hideDebugFn, toggleDebugFn)
import Engine.Scripting.Lua.API.Config (getVideoConfigFn, setVideoConfigFn
                                       , saveVideoConfigFn, setUIScaleFn
                                       , setFrameLimitFn, setResolutionFn
                                       , setWindowModeFn, setVSyncFn
                                       , setMSAAFn, setBrightnessFn
                                       , setPixelSnapFn, setTextureFilterFn
                                       , loadDefaultConfigFn
                                       , getTooltipDwellMsFn
                                       , setTooltipDwellMsFn
                                       , getTooltipHintDelayMsFn
                                       , setTooltipHintDelayMsFn)
import Engine.Scripting.Lua.API.Graphics (loadTextureFn, getTextureSizeFn,
                                           getLoadedTexturePathsFn,
                                           spawnSpriteFn, setPosFn,
                                           setColorFn, setSizeFn, setVisibleFn
                                           , destroyFn, getUIScaleFn)
import Engine.Scripting.Lua.API.YamlTextures (loadMaterialYamlFn, loadVegetationYamlFn
                                             , getTextureHandleFn, loadFloraYamlFn)
import Engine.Scripting.Lua.API.Log (logInfoFn, logWarnFn, logErrorFn, logDebugFn)
import Engine.Scripting.Lua.API.Input (isKeyDownFn, isActionDownFn,
                                       getMousePositionFn,
                                       isMouseButtonDownFn, getWindowSizeFn,
                                       getFramebufferSizeFn, getWorldCoordFn)
import Engine.Scripting.Lua.API.Keybinds (getKeybindsFn, setActionKeysFn,
                                          addActionKeyFn, removeActionKeyFn,
                                          removeActionKeysMatchingFn,
                                          saveKeybindsFn, loadDefaultKeybindsFn,
                                          keyMatchesActionFn, getCurrentKeyNameFn)
import Engine.Scripting.Lua.API.Text (loadFontFn, spawnTextFn, setTextFn,
                                       getTextFn, getTextWidthFn)
import Engine.Scripting.Lua.API.ShellFocus (registerFocusableFn, requestFocusFn,
                                            releaseFocusFn, getFocusIdFn)
import Engine.Scripting.Lua.API.Shell (shellExecuteFn)
import Engine.Scripting.Lua.API.Save
    (saveListFn, saveWorldFn, saveStatusFn, loadSaveFn, loadStatusFn
    , saveConfigFn, defaultSaveConfigFn, setSaveConfigFn
    , prepareAutosaveCycleFn, finalizeAutosaveRotationFn)
import Engine.Scripting.Lua.API.PlayerEvent (emitEventFn, emitEventAtFn
                                            , emitEventForUnitFn
                                            , getEventLogFn
                                            , getEventLogProgressFn
                                            , getNotificationCfgFn
                                            , setNotificationOverridesFn)
import Engine.Scripting.Lua.API.Units (loadUnitYamlFn)
import Engine.Scripting.Lua.API.Buildings (loadBuildingYamlFn)
import Engine.Scripting.Lua.API.Items (loadItemYamlFn)
import Engine.Scripting.Lua.API.Equipment (loadEquipmentYamlFn)
import Engine.Scripting.Lua.API.Substance (loadSubstanceYamlFn)
import Engine.Scripting.Lua.API.Infection (loadInfectionYamlFn)
import Engine.Scripting.Lua.API.Craft (loadRecipeYamlFn)
import Engine.Scripting.Lua.API.Locations (loadLocationYamlFn, locationListDefsFn)
import Engine.Scripting.Lua.API.LootTables (loadLootTableYamlFn)
import Engine.Scripting.Lua.API.LootProfiles (loadLootProfileYamlFn)
import Engine.Scripting.Lua.API.Tutorial (loadTutorialDirFn, getTutorialTreeFn)
import Engine.Scripting.Lua.API.Yaml (loadYamlFn)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Core (toCoreCapability)
import Engine.Core.Capability.ContentRegistries
  (toContentRegistriesCapability)
import Engine.Core.Capability.ContentRegistriesView
  (toContentRegistriesViewCapability)
import qualified HsLua as Lua

-- | engine.debugThrow() — deliberately throws a non-Lua Haskell
--   exception. Exists to regression-test the registerLuaFunction
--   guard: pre-guard, this exact call killed the engine.
debugThrowFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
debugThrowFn = error "debugThrow: deliberate test exception"

-- | Populate and install the @engine@ global table.
registerEngineAPI ∷ LuaCallStats → EngineEnv → LuaBackendState
                  → Lua.LuaE Lua.Exception ()
registerEngineAPI callStats env backendState = do
  -- The content-registry loaders are narrowed to the `content-registries`
  -- capability (#890) — projected once here, alongside `core-init` for
  -- their logging. See docs/engineenv_capability_inventory.md SS7.6.
  let core = toCoreCapability env
      regs = toContentRegistriesCapability env
      -- #2499: the loot-profile loader WRITES its own registry through
      -- `regs` but only READS the item registry, so it takes the #1896
      -- reader view for that one rather than the raw handle.
      regsView = toContentRegistriesViewCapability env
  Lua.newtable

  registerLuaFunction callStats "engine" "quit"              (quitFn env)
  registerLuaFunction callStats "engine" "debugThrow"        debugThrowFn
  registerLuaFunction callStats "engine" "logInfo"           (logInfoFn core)
  registerLuaFunction callStats "engine" "logWarn"           (logWarnFn core)
  registerLuaFunction callStats "engine" "logError"          (logErrorFn core)
  registerLuaFunction callStats "engine" "logDebug"          (logDebugFn core)
  registerLuaFunction callStats "engine" "showDebug"         (showDebugFn backendState)
  registerLuaFunction callStats "engine" "hideDebug"         (hideDebugFn backendState)
  registerLuaFunction callStats "engine" "toggleDebug"       (toggleDebugFn backendState)
  registerLuaFunction callStats "engine" "getFPS"            (getFPSFn env)
  registerLuaFunction callStats "engine" "getBootProfile"    (getBootProfileFn env)
  registerLuaFunction callStats "engine" "getPreviewTarget"  (getPreviewTargetFn env)
  registerLuaFunction callStats "engine" "getPreviewBrowse"  (getPreviewBrowseFn env)
  registerLuaFunction callStats "engine" "setPaused"         (setPausedFn env)
  registerLuaFunction callStats "engine" "isPaused"          (isPausedFn env)
  registerLuaFunction callStats "engine" "getSaveStatus"     (saveStatusFn env)
  registerLuaFunction callStats "engine" "getLoadStatus"     (loadStatusFn env)
  registerLuaFunction callStats "engine" "realTime"          realTimeFn
  registerLuaFunction callStats "engine" "gameTime"          (gameTimeFn env)
  registerLuaFunction callStats "engine" "loadScript"        (loadScriptFn env backendState)
  registerLuaFunction callStats "engine" "killScript"        (killScriptFn env backendState)
  registerLuaFunction callStats "engine" "pauseScript"       (pauseScriptFn backendState)
  registerLuaFunction callStats "engine" "resumeScript"      (resumeScriptFn backendState)
  registerLuaFunction callStats "engine" "setTickInterval"   (setTickIntervalFn env backendState)
  registerLuaFunction callStats "engine" "listFiles"         (listFilesFn)
  registerLuaFunction callStats "engine" "listFilesRecursive" (listFilesRecursiveFn)
  registerLuaFunction callStats "engine" "loadYaml"          loadYamlFn

  registerLuaFunction callStats "engine" "getVideoConfig"    (getVideoConfigFn env)
  registerLuaFunction callStats "engine" "setVideoConfig"    (setVideoConfigFn env)
  registerLuaFunction callStats "engine" "saveVideoConfig"   (saveVideoConfigFn env)
  registerLuaFunction callStats "engine" "loadDefaultConfig" (loadDefaultConfigFn env)
  registerLuaFunction callStats "engine" "setUIScale"        (setUIScaleFn env)
  registerLuaFunction callStats "engine" "setFrameLimit"     (setFrameLimitFn env)
  registerLuaFunction callStats "engine" "setResolution"     (setResolutionFn env)
  registerLuaFunction callStats "engine" "setWindowMode"     (setWindowModeFn env)
  registerLuaFunction callStats "engine" "setVSync"          (setVSyncFn env)
  registerLuaFunction callStats "engine" "setMSAA"           (setMSAAFn env)
  registerLuaFunction callStats "engine" "setBrightness"     (setBrightnessFn env)
  registerLuaFunction callStats "engine" "setPixelSnap"      (setPixelSnapFn env)
  registerLuaFunction callStats "engine" "setTextureFilter"  (setTextureFilterFn env)
  registerLuaFunction callStats "engine" "getTooltipDwellMs"     (getTooltipDwellMsFn env)
  registerLuaFunction callStats "engine" "setTooltipDwellMs"     (setTooltipDwellMsFn env)
  registerLuaFunction callStats "engine" "getTooltipHintDelayMs" (getTooltipHintDelayMsFn env)
  registerLuaFunction callStats "engine" "setTooltipHintDelayMs" (setTooltipHintDelayMsFn env)

  registerLuaFunction callStats "engine" "loadTexture"   (loadTextureFn backendState)
  registerLuaFunction callStats "engine" "getTextureSize" (getTextureSizeFn env)
  registerLuaFunction callStats "engine" "getLoadedTexturePaths" (getLoadedTexturePathsFn env)
  registerLuaFunction callStats "engine" "getTextureHandle" (getTextureHandleFn env)
  registerLuaFunction callStats "engine" "spawnSprite"   (spawnSpriteFn env backendState)
  registerLuaFunction callStats "engine" "setPos"        (setPosFn env backendState)
  registerLuaFunction callStats "engine" "setColor"      (setColorFn env backendState)
  registerLuaFunction callStats "engine" "setSize"       (setSizeFn env backendState)
  registerLuaFunction callStats "engine" "setVisible"    (setVisibleFn env backendState)
  registerLuaFunction callStats "engine" "destroy"       (destroyFn env backendState)
  registerLuaFunction callStats "engine" "getUIScale"    (getUIScaleFn env)

  registerLuaFunction callStats "engine" "loadMaterialYaml" (loadMaterialYamlFn env backendState)
  registerLuaFunction callStats "engine" "loadVegetationYaml" (loadVegetationYamlFn env backendState)
  registerLuaFunction callStats "engine" "loadFloraYaml" (loadFloraYamlFn env backendState)
  registerLuaFunction callStats "engine" "loadUnitYaml" (loadUnitYamlFn env backendState)
  registerLuaFunction callStats "engine" "loadBuildingYaml" (loadBuildingYamlFn env backendState)
  registerLuaFunction callStats "engine" "loadItemYaml" (loadItemYamlFn core regs env backendState)
  registerLuaFunction callStats "engine" "loadEquipmentYaml"
                          (loadEquipmentYamlFn core regs env backendState)
  registerLuaFunction callStats "engine" "loadSubstanceYaml" (loadSubstanceYamlFn core regs)
  registerLuaFunction callStats "engine" "loadInfectionYaml" (loadInfectionYamlFn core regs)
  registerLuaFunction callStats "engine" "loadRecipeYaml" (loadRecipeYamlFn core regs)
  registerLuaFunction callStats "engine" "loadLocationYaml"
                          (loadLocationYamlFn core regs env backendState)
  registerLuaFunction callStats "engine" "listLocationDefs" (locationListDefsFn regs)
  registerLuaFunction callStats "engine" "loadLootTableYaml" (loadLootTableYamlFn core regs)
  registerLuaFunction callStats "engine" "loadLootProfileYaml"
                          (loadLootProfileYamlFn core regs regsView)
  registerLuaFunction callStats "engine" "loadTutorialDir" (loadTutorialDirFn core regs)
  registerLuaFunction callStats "engine" "getTutorialTree" (getTutorialTreeFn regs)

  registerLuaFunction callStats "engine" "isKeyDown"         (isKeyDownFn backendState)
  registerLuaFunction callStats "engine" "isActionDown"      (isActionDownFn env backendState)
  registerLuaFunction callStats "engine" "getMousePosition"  (getMousePositionFn backendState)
  registerLuaFunction callStats "engine" "isMouseButtonDown" (isMouseButtonDownFn backendState)
  registerLuaFunction callStats "engine" "getWindowSize"     (getWindowSizeFn env backendState)
  registerLuaFunction callStats "engine" "getFramebufferSize" (getFramebufferSizeFn env backendState)
  registerLuaFunction callStats "engine" "getWorldCoord"     (getWorldCoordFn env backendState)

  registerLuaFunction callStats "engine" "getKeybinds"        (getKeybindsFn env)
  registerLuaFunction callStats "engine" "setActionKeys"      (setActionKeysFn env)
  registerLuaFunction callStats "engine" "addActionKey"       (addActionKeyFn env)
  registerLuaFunction callStats "engine" "removeActionKey"    (removeActionKeyFn env)
  registerLuaFunction callStats "engine" "removeActionKeysMatching" (removeActionKeysMatchingFn env)
  registerLuaFunction callStats "engine" "saveKeybinds"       (saveKeybindsFn env)
  registerLuaFunction callStats "engine" "loadDefaultKeybinds" (loadDefaultKeybindsFn env)
  registerLuaFunction callStats "engine" "keyMatchesAction"   (keyMatchesActionFn env)
  registerLuaFunction callStats "engine" "getCurrentKeyName"  (getCurrentKeyNameFn env)

  registerLuaFunction callStats "engine" "loadFont"     (loadFontFn env backendState)
  registerLuaFunction callStats "engine" "spawnText"    (spawnTextFn env backendState)
  registerLuaFunction callStats "engine" "setText"      (setTextFn env)
  registerLuaFunction callStats "engine" "getText"      (getTextFn env)
  registerLuaFunction callStats "engine" "getTextWidth" (getTextWidthFn env)

  registerLuaFunction callStats "engine" "registerFocusable" (registerFocusableFn env)
  registerLuaFunction callStats "engine" "requestFocus"      (requestFocusFn env)
  registerLuaFunction callStats "engine" "releaseFocus"      (releaseFocusFn env)
  registerLuaFunction callStats "engine" "getFocusId"        (getFocusIdFn env)
  registerLuaFunction callStats "engine" "shellExecute" shellExecuteFn

  registerLuaFunction callStats "engine" "listSaves" (saveListFn env)
  registerLuaFunction callStats "engine" "saveWorld" (saveWorldFn env)
  -- #913 autosave: config accessors + the reserved-slot rotation the
  -- Lua scheduler runs immediately before each autosave request.
  registerLuaFunction callStats "engine" "getSaveConfig" (saveConfigFn env)
  registerLuaFunction callStats "engine" "getDefaultSaveConfig" (defaultSaveConfigFn env)
  registerLuaFunction callStats "engine" "setSaveConfig" (setSaveConfigFn env)
  registerLuaFunction callStats "engine" "prepareAutosaveCycle" (prepareAutosaveCycleFn env)
  registerLuaFunction callStats "engine" "finalizeAutosaveRotation"
      (finalizeAutosaveRotationFn env)
  registerLuaFunction callStats "engine" "loadSave"  (loadSaveFn env)

  registerLuaFunction callStats "engine" "emitEvent"   (emitEventFn env)
  registerLuaFunction callStats "engine" "emitEventAt" (emitEventAtFn env)
  registerLuaFunction callStats "engine" "emitEventForUnit" (emitEventForUnitFn env)
  registerLuaFunction callStats "engine" "getEventLog" (getEventLogFn env)
  registerLuaFunction callStats "engine" "getEventLogProgress" (getEventLogProgressFn env)
  registerLuaFunction callStats "engine" "getNotificationCfg"      (getNotificationCfgFn env)
  registerLuaFunction callStats "engine" "setNotificationOverrides"
    (setNotificationOverridesFn env)

  Lua.setglobal (Lua.Name "engine")
