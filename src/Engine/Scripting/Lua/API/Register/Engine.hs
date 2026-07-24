module Engine.Scripting.Lua.API.Register.Engine
  ( registerEngineAPI
  ) where

import UPrelude
import Engine.Scripting.Lua.Types (LuaBackendState)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Core (loadScriptFn, killScriptFn,
                                      setTickIntervalFn, pauseScriptFn,
                                      resumeScriptFn, quitFn, getFPSFn,
                                      listFilesFn, setPausedFn, isPausedFn,
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
import Engine.Scripting.Lua.API.Focus (registerFocusableFn, requestFocusFn,
                                        releaseFocusFn, getFocusIdFn)
import Engine.Scripting.Lua.API.Shell (shellExecuteFn)
import Engine.Scripting.Lua.API.Save
    (saveListFn, saveWorldFn, saveStatusFn, loadSaveFn, loadStatusFn)
import Engine.Scripting.Lua.API.PlayerEvent (emitEventFn, emitEventAtFn
                                            , emitEventForUnitFn
                                            , getEventLogFn
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
import Engine.Scripting.Lua.API.Yaml (loadYamlFn)
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | engine.debugThrow() — deliberately throws a non-Lua Haskell
--   exception. Exists to regression-test the registerLuaFunction
--   guard: pre-guard, this exact call killed the engine.
debugThrowFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
debugThrowFn = error "debugThrow: deliberate test exception"

-- | Populate and install the @engine@ global table.
registerEngineAPI ∷ Lua.State → EngineEnv → LuaBackendState
                  → Lua.LuaE Lua.Exception ()
registerEngineAPI lst env backendState = do
  Lua.newtable

  registerLuaFunction "quit"              (quitFn env)
  registerLuaFunction "debugThrow"        debugThrowFn
  registerLuaFunction "logInfo"           (logInfoFn env)
  registerLuaFunction "logWarn"           (logWarnFn env)
  registerLuaFunction "logError"          (logErrorFn env)
  registerLuaFunction "logDebug"          (logDebugFn env)
  registerLuaFunction "showDebug"         (showDebugFn backendState)
  registerLuaFunction "hideDebug"         (hideDebugFn backendState)
  registerLuaFunction "toggleDebug"       (toggleDebugFn backendState)
  registerLuaFunction "getFPS"            (getFPSFn env)
  registerLuaFunction "getBootProfile"    (getBootProfileFn env)
  registerLuaFunction "getPreviewTarget"  (getPreviewTargetFn env)
  registerLuaFunction "getPreviewBrowse"  (getPreviewBrowseFn env)
  registerLuaFunction "setPaused"         (setPausedFn env)
  registerLuaFunction "isPaused"          (isPausedFn env)
  registerLuaFunction "getSaveStatus"     (saveStatusFn env)
  registerLuaFunction "getLoadStatus"     (loadStatusFn env)
  registerLuaFunction "realTime"          realTimeFn
  registerLuaFunction "gameTime"          (gameTimeFn env)
  registerLuaFunction "loadScript"        (loadScriptFn env backendState lst)
  registerLuaFunction "killScript"        (killScriptFn env backendState lst)
  registerLuaFunction "pauseScript"       (pauseScriptFn backendState)
  registerLuaFunction "resumeScript"      (resumeScriptFn backendState)
  registerLuaFunction "setTickInterval"   (setTickIntervalFn env backendState)
  registerLuaFunction "listFiles"         (listFilesFn)
  registerLuaFunction "loadYaml"          loadYamlFn

  registerLuaFunction "getVideoConfig"    (getVideoConfigFn env)
  registerLuaFunction "setVideoConfig"    (setVideoConfigFn env)
  registerLuaFunction "saveVideoConfig"   (saveVideoConfigFn env)
  registerLuaFunction "loadDefaultConfig" (loadDefaultConfigFn env)
  registerLuaFunction "setUIScale"        (setUIScaleFn env)
  registerLuaFunction "setFrameLimit"     (setFrameLimitFn env)
  registerLuaFunction "setResolution"     (setResolutionFn env)
  registerLuaFunction "setWindowMode"     (setWindowModeFn env)
  registerLuaFunction "setVSync"          (setVSyncFn env)
  registerLuaFunction "setMSAA"           (setMSAAFn env)
  registerLuaFunction "setBrightness"     (setBrightnessFn env)
  registerLuaFunction "setPixelSnap"      (setPixelSnapFn env)
  registerLuaFunction "setTextureFilter"  (setTextureFilterFn env)
  registerLuaFunction "getTooltipDwellMs"     (getTooltipDwellMsFn env)
  registerLuaFunction "setTooltipDwellMs"     (setTooltipDwellMsFn env)
  registerLuaFunction "getTooltipHintDelayMs" (getTooltipHintDelayMsFn env)
  registerLuaFunction "setTooltipHintDelayMs" (setTooltipHintDelayMsFn env)

  registerLuaFunction "loadTexture"   (loadTextureFn backendState)
  registerLuaFunction "getTextureSize" (getTextureSizeFn env)
  registerLuaFunction "getTextureHandle" (getTextureHandleFn env)
  registerLuaFunction "spawnSprite"   (spawnSpriteFn env backendState)
  registerLuaFunction "setPos"        (setPosFn env backendState)
  registerLuaFunction "setColor"      (setColorFn env backendState)
  registerLuaFunction "setSize"       (setSizeFn env backendState)
  registerLuaFunction "setVisible"    (setVisibleFn env backendState)
  registerLuaFunction "destroy"       (destroyFn env backendState)
  registerLuaFunction "getUIScale"    (getUIScaleFn env)

  registerLuaFunction "loadMaterialYaml" (loadMaterialYamlFn env backendState)
  registerLuaFunction "loadVegetationYaml" (loadVegetationYamlFn env backendState)
  registerLuaFunction "loadFloraYaml" (loadFloraYamlFn env backendState)
  registerLuaFunction "loadUnitYaml" (loadUnitYamlFn env backendState)
  registerLuaFunction "loadBuildingYaml" (loadBuildingYamlFn env backendState)
  registerLuaFunction "loadItemYaml" (loadItemYamlFn env backendState)
  registerLuaFunction "loadEquipmentYaml" (loadEquipmentYamlFn env backendState)
  registerLuaFunction "loadSubstanceYaml" (loadSubstanceYamlFn env)
  registerLuaFunction "loadInfectionYaml" (loadInfectionYamlFn env)
  registerLuaFunction "loadRecipeYaml" (loadRecipeYamlFn env)
  registerLuaFunction "loadLocationYaml" (loadLocationYamlFn env backendState)
  registerLuaFunction "listLocationDefs" (locationListDefsFn env)
  registerLuaFunction "loadLootTableYaml" (loadLootTableYamlFn env)

  registerLuaFunction "isKeyDown"         (isKeyDownFn backendState)
  registerLuaFunction "isActionDown"      (isActionDownFn env backendState)
  registerLuaFunction "getMousePosition"  (getMousePositionFn backendState)
  registerLuaFunction "isMouseButtonDown" (isMouseButtonDownFn backendState)
  registerLuaFunction "getWindowSize"     (getWindowSizeFn env backendState)
  registerLuaFunction "getFramebufferSize" (getFramebufferSizeFn env backendState)
  registerLuaFunction "getWorldCoord"     (getWorldCoordFn env backendState)

  registerLuaFunction "getKeybinds"        (getKeybindsFn env)
  registerLuaFunction "setActionKeys"      (setActionKeysFn env)
  registerLuaFunction "addActionKey"       (addActionKeyFn env)
  registerLuaFunction "removeActionKey"    (removeActionKeyFn env)
  registerLuaFunction "removeActionKeysMatching" (removeActionKeysMatchingFn env)
  registerLuaFunction "saveKeybinds"       (saveKeybindsFn env)
  registerLuaFunction "loadDefaultKeybinds" (loadDefaultKeybindsFn env)
  registerLuaFunction "keyMatchesAction"   (keyMatchesActionFn env)
  registerLuaFunction "getCurrentKeyName"  (getCurrentKeyNameFn env)

  registerLuaFunction "loadFont"     (loadFontFn env backendState)
  registerLuaFunction "spawnText"    (spawnTextFn env backendState)
  registerLuaFunction "setText"      (setTextFn env)
  registerLuaFunction "getText"      (getTextFn env)
  registerLuaFunction "getTextWidth" (getTextWidthFn env)

  registerLuaFunction "registerFocusable" (registerFocusableFn env)
  registerLuaFunction "requestFocus"      (requestFocusFn env)
  registerLuaFunction "releaseFocus"      (releaseFocusFn env)
  registerLuaFunction "getFocusId"        (getFocusIdFn env)
  registerLuaFunction "shellExecute" shellExecuteFn

  registerLuaFunction "listSaves" (saveListFn env)
  registerLuaFunction "saveWorld" (saveWorldFn env)
  registerLuaFunction "loadSave"  (loadSaveFn env)

  registerLuaFunction "emitEvent"   (emitEventFn env)
  registerLuaFunction "emitEventAt" (emitEventAtFn env)
  registerLuaFunction "emitEventForUnit" (emitEventForUnitFn env)
  registerLuaFunction "getEventLog" (getEventLogFn env)
  registerLuaFunction "getNotificationCfg"      (getNotificationCfgFn env)
  registerLuaFunction "setNotificationOverrides"
    (setNotificationOverridesFn env)

  Lua.setglobal (Lua.Name "engine")
