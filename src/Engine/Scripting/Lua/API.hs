module Engine.Scripting.Lua.API
  ( registerLuaAPI
  , registerLuaFunction
  ) where

import UPrelude
import Engine.Scripting.Lua.Types (LuaBackendState)
import Engine.Scripting.Lua.API.Core (loadScriptFn, killScriptFn,
                                      setTickIntervalFn, pauseScriptFn,
                                      resumeScriptFn, quitFn, getFPSFn,
                                      listFilesFn, setPausedFn, isPausedFn,
                                      realTimeFn, gameTimeFn)
import Engine.Scripting.Lua.API.Camera
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
import Engine.Scripting.Lua.API.Graphics (loadTextureFn, spawnSpriteFn, setPosFn,
                                           setColorFn, setSizeFn, setVisibleFn
                                           , destroyFn, getUIScaleFn)
import Engine.Scripting.Lua.API.YamlTextures (loadMaterialYamlFn, loadVegetationYamlFn
                                             , getTextureHandleFn, loadFloraYamlFn)
import Engine.Scripting.Lua.API.Log (logInfoFn, logWarnFn, logDebugFn)
import Engine.Scripting.Lua.API.Input (isKeyDownFn, isActionDownFn,
                                       getMousePositionFn,
                                       isMouseButtonDownFn, getWindowSizeFn, 
                                       getFramebufferSizeFn, getWorldCoordFn)
import Engine.Scripting.Lua.API.Text (loadFontFn, spawnTextFn, setTextFn, 
                                       getTextFn, getTextWidthFn)
import Engine.Scripting.Lua.API.Focus (registerFocusableFn, requestFocusFn, 
                                        releaseFocusFn, getFocusIdFn)
import Engine.Scripting.Lua.API.Shell (shellExecuteFn)
import Engine.Scripting.Lua.API.Save (saveListFn, saveWorldFn, loadSaveFn)
import Engine.Scripting.Lua.API.PlayerEvent (emitEventFn, emitEventAtFn
                                            , getEventLogFn
                                            , getNotificationCfgFn
                                            , setNotificationOverridesFn)
import Engine.Scripting.Lua.API.World
import Engine.Scripting.Lua.API.WorldQuery
import Engine.Scripting.Lua.API.Units
import Engine.Scripting.Lua.API.Buildings
import Engine.Scripting.Lua.API.Items
import Engine.Scripting.Lua.API.Flora
import Engine.Scripting.Lua.API.UI
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua
import qualified Data.ByteString.Char8 as BS

registerLuaFunction ∷ BS.ByteString → Lua.LuaE Lua.Exception Lua.NumResults
                    → Lua.LuaE Lua.Exception ()
registerLuaFunction name action = do
    Lua.pushHaskellFunction action
    Lua.setfield (-2) (Lua.Name name)

registerLuaAPI ∷ Lua.State → EngineEnv → LuaBackendState → IO ()
registerLuaAPI lst env backendState = Lua.runWith lst $ do
  Lua.newtable
  
  registerLuaFunction "quit"              (quitFn env)
  registerLuaFunction "logInfo"           (logInfoFn env)
  registerLuaFunction "logWarn"           (logWarnFn env)
  registerLuaFunction "logDebug"          (logDebugFn env)
  registerLuaFunction "showDebug"         (showDebugFn backendState)
  registerLuaFunction "hideDebug"         (hideDebugFn backendState)
  registerLuaFunction "toggleDebug"       (toggleDebugFn backendState)
  registerLuaFunction "getFPS"            (getFPSFn env)
  registerLuaFunction "setPaused"         (setPausedFn env)
  registerLuaFunction "isPaused"          (isPausedFn env)
  registerLuaFunction "realTime"          realTimeFn
  registerLuaFunction "gameTime"          (gameTimeFn env)
  registerLuaFunction "loadScript"        (loadScriptFn env backendState lst)
  registerLuaFunction "killScript"        (killScriptFn env backendState lst)
  registerLuaFunction "pauseScript"       (pauseScriptFn backendState)
  registerLuaFunction "resumeScript"      (resumeScriptFn backendState)
  registerLuaFunction "setTickInterval"   (setTickIntervalFn env backendState)
  registerLuaFunction "listFiles"         (listFilesFn)

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
  
  registerLuaFunction "isKeyDown"         (isKeyDownFn backendState)
  registerLuaFunction "isActionDown"      (isActionDownFn env backendState)
  registerLuaFunction "getMousePosition"  (getMousePositionFn backendState)
  registerLuaFunction "isMouseButtonDown" (isMouseButtonDownFn backendState)
  registerLuaFunction "getWindowSize"     (getWindowSizeFn env backendState)
  registerLuaFunction "getFramebufferSize" (getFramebufferSizeFn env backendState)
  registerLuaFunction "getWorldCoord"     (getWorldCoordFn env backendState)
  
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
  registerLuaFunction "getEventLog" (getEventLogFn env)
  registerLuaFunction "getNotificationCfg"      (getNotificationCfgFn env)
  registerLuaFunction "setNotificationOverrides"
    (setNotificationOverridesFn env)

  Lua.setglobal (Lua.Name "engine")
  
  Lua.newtable
  registerLuaFunction "newPage"    (uiNewPageFn env)
  registerLuaFunction "deletePage" (uiDeletePageFn env)
  registerLuaFunction "showPage"   (uiShowPageFn env)
  registerLuaFunction "hidePage"   (uiHidePageFn env)
  
  registerLuaFunction "newElement" (uiNewElementFn env)
  registerLuaFunction "newBox"     (uiNewBoxFn env)
  registerLuaFunction "newText"    (uiNewTextFn env)
  registerLuaFunction "newSprite"  (uiNewSpriteFn env)
  
  registerLuaFunction "addToPage"      (uiAddToPageFn env)
  registerLuaFunction "addChild"       (uiAddChildFn env)
  registerLuaFunction "removeElement"  (uiRemoveElementFn env)
  registerLuaFunction "deleteElement"  (uiDeleteElementFn env)
  registerLuaFunction "findElementAt" (uiFindElementAtFn env)
  registerLuaFunction "getElementOnClick" (uiGetElementOnClickFn env)
  registerLuaFunction "findHoverTarget" (uiFindHoverTargetFn env)

  registerLuaFunction "enableTextInput" (uiEnableTextInputFn env)
  registerLuaFunction "getTextInput" (uiGetTextFn env)
  registerLuaFunction "setTextInput" (uiSetTextInputFn env)
  registerLuaFunction "getCursor" (uiGetCursorFn env)
  registerLuaFunction "setCursor" (uiSetCursorFn env)
  registerLuaFunction "insertChar" (uiInsertCharFn env)
  registerLuaFunction "deleteBackward" (uiDeleteBackwardFn env)
  registerLuaFunction "deleteForward" (uiDeleteForwardFn env)
  registerLuaFunction "cursorLeft" (uiCursorLeftFn env)
  registerLuaFunction "cursorRight" (uiCursorRightFn env)
  registerLuaFunction "cursorHome" (uiCursorHomeFn env)
  registerLuaFunction "cursorEnd" (uiCursorEndFn env)

  registerLuaFunction "setFocus"       (uiSetFocusFn env)
  registerLuaFunction "clearFocus"     (uiClearFocusFn env)
  registerLuaFunction "getFocus"       (uiGetFocusFn env)
  registerLuaFunction "hasFocus"       (uiHasFocusFn env)
  
  registerLuaFunction "setPosition"  (uiSetPositionFn env)
  registerLuaFunction "setSize"      (uiSetSizeFn env)
  registerLuaFunction "setVisible"   (uiSetVisibleFn env)
  registerLuaFunction "isPageVisible" (uiIsPageVisibleFn env)
  registerLuaFunction "setClickable" (uiSetClickableFn env)
  registerLuaFunction "setZIndex"    (uiSetZIndexFn env)
  registerLuaFunction "setColor"     (uiSetColorFn env)
  registerLuaFunction "setText"      (uiSetTextFn env)
  registerLuaFunction "setSpriteTexture" (uiSetSpriteTextureFn env)
  registerLuaFunction "setOnClick"   (uiSetOnClickFn env)
  registerLuaFunction "setOnRightClick" (uiSetOnRightClickFn env)
  registerLuaFunction "removeFromPage" (uiRemoveFromPageFn env)
  
  registerLuaFunction "setBoxTextures" (uiSetBoxTexturesFn env)
  registerLuaFunction "loadBoxTextures" (uiLoadBoxTexturesFn env)

  registerLuaFunction "setTooltip"        (uiSetTooltipFn env)
  registerLuaFunction "setTooltipRich"    (uiSetTooltipRichFn env)
  registerLuaFunction "clearTooltip"      (uiClearTooltipFn env)
  registerLuaFunction "setTooltipStyle"   (uiSetTooltipStyleFn env)
  registerLuaFunction "lockTooltip"       (uiLockTooltipFn env)
  registerLuaFunction "unlockTooltip"     (uiUnlockTooltipFn env)
  registerLuaFunction "toggleTooltipLock" (uiToggleTooltipLockFn env)
  registerLuaFunction "isTooltipLocked"   (uiIsTooltipLockedFn env)

  Lua.setglobal (Lua.Name "UI")

  Lua.newtable

  registerLuaFunction "spawn"       (unitSpawnFn env)
  registerLuaFunction "destroy"     (unitDestroyFn env)
  registerLuaFunction "setPos"      (unitSetPosFn env)
  registerLuaFunction "getPos"      (unitGetPosFn env)
  registerLuaFunction "getInfo"     (unitGetInfoFn env)
  registerLuaFunction "list"        (unitListFn env)
  registerLuaFunction "listDefs"    (unitListDefsFn env)
  registerLuaFunction "moveTo"      (unitMoveToFn env)
  registerLuaFunction "stop"        (unitStopFn env)
  registerLuaFunction "select"      (unitSelectFn env)
  registerLuaFunction "deselectAll" (unitDeselectAllFn env)
  registerLuaFunction "getSelected" (unitGetSelectedFn env)
  registerLuaFunction "isSelected"  (unitIsSelectedFn env)
  registerLuaFunction "hitTestAt"   (unitHitTestAtFn env)
  registerLuaFunction "hitTestInRect" (unitHitTestInRectFn env)
  registerLuaFunction "setSelection" (unitSetSelectionFn env)
  registerLuaFunction "setAnim"     (unitSetAnimFn env)
  registerLuaFunction "collapse"    (unitCollapseFn env)
  registerLuaFunction "revive"      (unitReviveFn env)
  registerLuaFunction "kill"        (unitKillFn env)
  registerLuaFunction "recomputeBody" (unitRecomputeBodyFn env)
  registerLuaFunction "getStat"     (unitGetStatFn env)
  registerLuaFunction "getStatBase" (unitGetStatBaseFn env)
  registerLuaFunction "setStat"     (unitSetStatFn env)
  registerLuaFunction "getAllStats" (unitGetAllStatsFn env)
  registerLuaFunction "getInventory" (unitGetInventoryFn env)
  registerLuaFunction "drink"        (unitDrinkFn env)
  registerLuaFunction "eat"          (unitEatFn env)
  registerLuaFunction "pickup"       (unitPickupFn env)
  registerLuaFunction "removeItem"   (unitRemoveItemFn env)
  registerLuaFunction "transitionTo" (unitTransitionToFn env)
  registerLuaFunction "getPose"      (unitGetPoseFn env)
  registerLuaFunction "modifyItemFill" (unitModifyItemFillFn env)
  registerLuaFunction "addItem"        (unitAddItemFn env)
  registerLuaFunction "getVisibleTiles" (unitGetVisibleTilesFn env)
  registerLuaFunction "getFrameTexture" (unitGetFrameTextureFn env)
  registerLuaFunction "addModifier"    (unitAddModifierFn env)
  registerLuaFunction "removeModifier" (unitRemoveModifierFn env)
  registerLuaFunction "getModifiers"   (unitGetModifiersFn env)
  registerLuaFunction "clearModifiers" (unitClearModifiersFn env)
  registerLuaFunction "getAllIds"   (unitGetAllIdsFn env)
  registerLuaFunction "getActivity" (unitGetActivityFn env)
  registerLuaFunction "getSkill"     (unitGetSkillFn env)
  -- NB: skill functions follow; the building global is set up after
  -- this `unit` table is finalized below.
  registerLuaFunction "setSkill"     (unitSetSkillFn env)
  registerLuaFunction "addXP"        (unitAddXPFn env)
  registerLuaFunction "getAllSkills" (unitGetAllSkillsFn env)

  Lua.setglobal (Lua.Name "unit")

  -- Building global. Mirrors `unit` in shape.
  Lua.newtable
  registerLuaFunction "spawn"               (buildingSpawnFn env)
  registerLuaFunction "destroy"             (buildingDestroyFn env)
  registerLuaFunction "canPlaceAt"          (buildingCanPlaceAtFn env)
  registerLuaFunction "setGhost"            (buildingSetGhostFn env)
  registerLuaFunction "clearGhost"          (buildingClearGhostFn env)
  registerLuaFunction "getStartingBuildings" (buildingGetStartingBuildingsFn env)
  registerLuaFunction "getInfo"             (buildingGetInfoFn env)
  registerLuaFunction "getActivity"         (buildingGetActivityFn env)
  registerLuaFunction "list"                (buildingListFn env)
  registerLuaFunction "listDefs"            (buildingListDefsFn env)
  registerLuaFunction "hitTestAt"           (buildingHitTestAtFn env)
  registerLuaFunction "select"              (buildingSelectFn env)
  registerLuaFunction "deselect"            (buildingDeselectFn env)
  registerLuaFunction "getSelected"         (buildingGetSelectedFn env)
  registerLuaFunction "setSpawnRemaining"   (buildingSetSpawnRemainingFn env)
  registerLuaFunction "getSpawnRemaining"   (buildingGetSpawnRemainingFn env)
  registerLuaFunction "consumeSpawn"        (buildingConsumeSpawnFn env)
  Lua.setglobal (Lua.Name "building")

  Lua.newtable
  registerLuaFunction "getGenDefaults" (worldGetGenDefaultsFn env)
  registerLuaFunction "setGenConfig" (worldSetGenConfigFn env)
  registerLuaFunction "init" (worldInitFn env)
  registerLuaFunction "initArena" (worldInitArenaFn env)
  registerLuaFunction "initArenaDone" (worldInitArenaDoneFn env)
  registerLuaFunction "openArena" (worldOpenArenaFn env)
  registerLuaFunction "show" (worldShowFn env)
  registerLuaFunction "hide" (worldHideFn env)
  registerLuaFunction "setTexture" (worldSetTextureFn env)
  registerLuaFunction "setCamera" (worldSetCameraFn env)
  registerLuaFunction "setSunAngle" (worldSetSunAngleFn env)
  registerLuaFunction "setTime" (worldSetTimeFn env)
  registerLuaFunction "setDate" (worldSetDateFn env)
  registerLuaFunction "setTimeScale" (worldSetTimeScaleFn env)
  registerLuaFunction "getTimeScale" (worldGetTimeScaleFn env)
  registerLuaFunction "getActiveWorldId" (worldGetActiveWorldIdFn env)
  registerLuaFunction "setMapMode" (worldSetMapModeFn env)
  registerLuaFunction "setZoomCursorHover" (worldSetZoomCursorHoverFn env)
  registerLuaFunction "setZoomCursorSelect" (worldSetZoomCursorSelectFn env)
  registerLuaFunction "clearZoomCursorSelect" (worldClearZoomCursorSelectFn env)
  registerLuaFunction "setZoomCursorSelectTexture"
    (worldSetZoomCursorSelectTextureFn env)
  registerLuaFunction "setZoomCursorHoverTexture"
    (worldSetZoomCursorHoverTextureFn env)
  registerLuaFunction "setWorldCursorSelectTexture"
    (worldSetWorldCursorSelectTextureFn env)
  registerLuaFunction "setWorldCursorHoverTexture"
    (worldSetWorldCursorHoverTextureFn env)
  registerLuaFunction "setWorldCursorSelectBgTexture"
    (worldSetWorldCursorSelectBgTextureFn env)
  registerLuaFunction "setWorldCursorHoverBgTexture"
    (worldSetWorldCursorHoverBgTextureFn env)
  registerLuaFunction "setWorldCursorHover" (worldSetWorldCursorHoverFn env)
  registerLuaFunction "setWorldCursorSelect" (worldSetWorldCursorSelectFn env)
  registerLuaFunction "clearWorldCursorSelect" (worldClearWorldCursorSelectFn env)
  registerLuaFunction "setToolMode" (worldSetToolModeFn env)
  registerLuaFunction "getToolMode" (worldGetToolModeFn env)
  registerLuaFunction "getInitProgress" (worldGetInitProgressFn env)
  registerLuaFunction "waitForInit" (worldWaitForInitFn env)
  registerLuaFunction "destroy" (worldDestroyFn env)
  registerLuaFunction "deleteTile" (worldDeleteTileFn env)
  registerLuaFunction "setFluidTile" (worldSetFluidTileFn env)

  registerLuaFunction "getTerrainAt" (worldGetTerrainAtFn env)
  registerLuaFunction "getFluidAt" (worldGetFluidAtFn env)
  registerLuaFunction "getSurfaceAt" (worldGetSurfaceAtFn env)
  registerLuaFunction "getChunkInfo" (worldGetChunkInfoFn env)
  registerLuaFunction "getAreaFluid" (worldGetAreaFluidFn env)
  registerLuaFunction "getRivers" (worldGetRiversFn env)
  registerLuaFunction "loadChunksInRegion" (worldLoadChunksInRegionFn env)
  registerLuaFunction "waitForChunks" (worldWaitForChunksFn env)
  registerLuaFunction "getHoverTile" (worldGetHoverTileFn env)

  Lua.setglobal (Lua.Name "world")

  Lua.newtable
  registerLuaFunction "register" (floraRegisterFn env)
  registerLuaFunction "setLifecycle" (floraSetLifecycleFn env)
  registerLuaFunction "addCycleStage" (floraAddCycleStageFn env)
  registerLuaFunction "addCycleOverride" (floraAddCycleOverrideFn env)
  registerLuaFunction "addPhase" (floraAddPhaseFn env)
  registerLuaFunction "registerForWorldGen" (floraRegisterForWorldGenFn env)

  Lua.setglobal (Lua.Name "flora")

  Lua.newtable

  registerLuaFunction "goToTile" (cameraGotoTileFn env)
  registerLuaFunction "move" (cameraMoveFn env)
  registerLuaFunction "setPosition" (cameraSetPositionFn env)
  registerLuaFunction "getPosition" (cameraGetPositionFn env)
  registerLuaFunction "setZoom" (cameraSetZoomFn env)
  registerLuaFunction "getZoom" (cameraGetZoomFn env)
  registerLuaFunction "getZoomFadeStart" cameraGetZoomFadeStartFn
  registerLuaFunction "getZoomFadeEnd" cameraGetZoomFadeEndFn
  registerLuaFunction "setZoomVelocity" (cameraSetZoomVelocityFn env)
  registerLuaFunction "getZoomVelocity" (cameraGetZoomVelocityFn env)
  registerLuaFunction "setZSlice" (cameraSetZSliceFn env)
  registerLuaFunction "getZSlice" (cameraGetZSliceFn env)
  registerLuaFunction "rotateCW" (cameraRotateCWFn env)
  registerLuaFunction "rotateCCW" (cameraRotateCCWFn env)
  registerLuaFunction "getFacing" (cameraGetFacingFn env)
  registerLuaFunction "getZTracking" (cameraGetZTrackingFn env)
  registerLuaFunction "setZTracking" (cameraSetZTrackingFn env)
  Lua.setglobal (Lua.Name "camera")
