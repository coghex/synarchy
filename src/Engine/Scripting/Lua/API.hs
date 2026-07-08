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
                                      getBootProfileFn, realTimeFn, gameTimeFn)
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
import Engine.Scripting.Lua.API.Save (saveListFn, saveWorldFn, loadSaveFn)
import Engine.Scripting.Lua.API.PlayerEvent (emitEventFn, emitEventAtFn
                                            , emitEventForUnitFn
                                            , getEventLogFn
                                            , getNotificationCfgFn
                                            , setNotificationOverridesFn)
import Engine.Scripting.Lua.API.World
import Engine.Scripting.Lua.API.WorldQuery
import Engine.Scripting.Lua.API.Forage
import Engine.Scripting.Lua.API.Units
import Engine.Scripting.Lua.API.Buildings
import Engine.Scripting.Lua.API.Structure
import Engine.Scripting.Lua.API.Construct
import Engine.Scripting.Lua.API.Chop
import Engine.Scripting.Lua.API.Till
import Engine.Scripting.Lua.API.Plant
import Engine.Scripting.Lua.API.Combat
import Engine.Scripting.Lua.API.Items
import Engine.Scripting.Lua.API.Yaml (loadYamlFn)
import Engine.Scripting.Lua.API.Equipment
import Engine.Scripting.Lua.API.Substance
import Engine.Scripting.Lua.API.Infection
import Engine.Scripting.Lua.API.Craft
import Engine.Scripting.Lua.API.Power
import Engine.Scripting.Lua.API.Repair
import Engine.Scripting.Lua.API.Blood
import Engine.Scripting.Lua.API.Locations
import Engine.Scripting.Lua.API.LootTables
import Engine.Scripting.Lua.API.Flora
import Engine.Scripting.Lua.API.UI
import Engine.Core.State (EngineEnv)
import qualified Control.Monad.Catch as Catch
import Control.Exception (SomeException, SomeAsyncException
                         , fromException, displayException)
import qualified HsLua as Lua
import qualified Data.ByteString.Char8 as BS

-- | Register a Haskell function in the table on top of the stack.
--
--   The action is guarded so that any non-Lua Haskell exception
--   thrown inside an API function (partial @VU.!@/@V.!@, 'error',
--   arithmetic) is converted into a regular Lua error instead of
--   escaping. hslua's own wrapper ('pushHaskellFunction' →
--   @exceptionToError@) only converts exceptions of the LuaError
--   type — anything else unwinds through @lua_pcall@'s C frames as
--   a Haskell exception, reaches the Lua thread's top-level handler,
--   and tears the whole engine down. With the guard, the error is
--   raised as a Lua error, caught by the 'Lua.pcall' isolation in
--   "Engine.Scripting.Lua.Script", logged, and the callback skipped.
--
--   'Lua.Exception's are re-thrown for hslua's own (richer)
--   conversion; async exceptions are re-thrown so shutdown's
--   killThread still works.
registerLuaFunction ∷ BS.ByteString → Lua.LuaE Lua.Exception Lua.NumResults
                    → Lua.LuaE Lua.Exception ()
registerLuaFunction name action = do
    Lua.pushHaskellFunction (action `Catch.catch` handler)
    Lua.setfield (-2) (Lua.Name name)
  where
    handler ∷ SomeException → Lua.LuaE Lua.Exception Lua.NumResults
    handler e
        | Just (le ∷ Lua.Exception) ← fromException e = Catch.throwM le
        | Just (ae ∷ SomeAsyncException) ← fromException e = Catch.throwM ae
        | otherwise = do
            Lua.pushstring $ "Haskell exception in " <> name <> ": "
                <> BS.pack (displayException e)
            Lua.error

-- | engine.debugThrow() — deliberately throws a non-Lua Haskell
--   exception. Exists to regression-test the registerLuaFunction
--   guard above: pre-guard, this exact call killed the engine.
debugThrowFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
debugThrowFn = error "debugThrow: deliberate test exception"

registerLuaAPI ∷ Lua.State → EngineEnv → LuaBackendState → IO ()
registerLuaAPI lst env backendState = Lua.runWith lst $ do
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
  registerLuaFunction "loadLocationYaml" (loadLocationYamlFn env)
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
  registerLuaFunction "listAnimations" (unitListAnimationsFn env)
  registerLuaFunction "moveTo"      (unitMoveToFn env)
  registerLuaFunction "jump"        (unitJumpFn env)
  registerLuaFunction "stop"        (unitStopFn env)
  registerLuaFunction "select"      (unitSelectFn env)
  registerLuaFunction "deselectAll" (unitDeselectAllFn env)
  registerLuaFunction "getSelected" (unitGetSelectedFn env)
  registerLuaFunction "isSelected"  (unitIsSelectedFn env)
  registerLuaFunction "hitTestAt"   (unitHitTestAtFn env)
  registerLuaFunction "hitTestInRect" (unitHitTestInRectFn env)
  registerLuaFunction "setSelection" (unitSetSelectionFn env)
  registerLuaFunction "setAnim"     (unitSetAnimFn env)
  registerLuaFunction "setAnimOverride"   (unitSetAnimOverrideFn env)
  registerLuaFunction "clearAnimOverride" (unitClearAnimOverrideFn env)
  registerLuaFunction "setFacing"   (unitSetFacingFn env)
  registerLuaFunction "setFrozen"   (unitSetFrozenFn env)
  registerLuaFunction "setForceLoop" (unitSetForceLoopFn env)
  registerLuaFunction "collapse"    (unitCollapseFn env)
  registerLuaFunction "crawl"       (unitCrawlFn env)
  registerLuaFunction "revive"      (unitReviveFn env)
  registerLuaFunction "kill"        (unitKillFn env)
  registerLuaFunction "recomputeBody" (unitRecomputeBodyFn env)
  registerLuaFunction "getStat"     (unitGetStatFn env)
  registerLuaFunction "getStatBase" (unitGetStatBaseFn env)
  registerLuaFunction "setStat"     (unitSetStatFn env)
  registerLuaFunction "getAllStats" (unitGetAllStatsFn env)
  registerLuaFunction "getInventory" (unitGetInventoryFn env)
  registerLuaFunction "getItemContents" (unitGetItemContentsFn env)
  registerLuaFunction "treatBleeding" (unitTreatBleedingFn env)
  registerLuaFunction "treatInfection" (unitTreatInfectionFn env)
  registerLuaFunction "frostbite"    (unitFrostbiteFn env)
  registerLuaFunction "injure"       (unitInjureFn env)
  registerLuaFunction "drink"        (unitDrinkFn env)
  registerLuaFunction "eat"          (unitEatFn env)
  registerLuaFunction "feed"         (unitFeedFn env)
  registerLuaFunction "getCalories"  (unitGetCaloriesFn env)
  registerLuaFunction "pickup"       (unitPickupFn env)
  registerLuaFunction "removeItem"   (unitRemoveItemFn env)
  registerLuaFunction "transferItemToBuilding" (unitTransferItemToBuildingFn env)
  registerLuaFunction "transferItemToUnit"     (unitTransferItemToUnitFn env)
  registerLuaFunction "depositToCargo"     (unitDepositToCargoFn env)
  registerLuaFunction "withdrawFromCargo"  (unitWithdrawFromCargoFn env)
  registerLuaFunction "getCarryingWeight"  (unitGetCarryingWeightFn env)
  registerLuaFunction "transitionTo" (unitTransitionToFn env)
  registerLuaFunction "getPose"      (unitGetPoseFn env)
  registerLuaFunction "getFaction"   (unitGetFactionFn env)
  registerLuaFunction "exists"       (unitExistsFn env)
  registerLuaFunction "getAttackRange" (unitGetAttackRangeFn env)
  registerLuaFunction "getAttackCooldown" (unitGetAttackCooldownFn env)
  registerLuaFunction "getAnimDuration" (unitGetAnimDurationFn env)
  registerLuaFunction "getMaxSpeed"  (unitGetMaxSpeedFn env)
  registerLuaFunction "getEquippedWeaponWeight"
                                          (unitGetEquippedWeaponWeightFn env)
  registerLuaFunction "getWeaponWieldedFrom"
                                          (unitGetWeaponWieldedFromFn env)
  registerLuaFunction "getWoundSeverityOn"
                                          (unitGetWoundSeverityOnFn env)
  registerLuaFunction "getWounds"    (unitGetWoundsFn env)
  registerLuaFunction "getScars"     (unitGetScarsFn env)
  registerLuaFunction "getImmunities" (unitGetImmunitiesFn env)
  registerLuaFunction "getInsulation" (unitGetInsulationFn env)
  registerLuaFunction "dropEquipmentToGround" (unitDropEquipmentToGroundFn env)
  registerLuaFunction "dropItemToGround" (unitDropItemToGroundFn env)
  registerLuaFunction "dropItemById"     (unitDropItemByIdFn env)
  registerLuaFunction "getBlood"     (unitGetBloodFn env)
  registerLuaFunction "getPain"      (unitGetPainFn env)
  registerLuaFunction "getLastAttacker" (unitGetLastAttackerFn env)
  registerLuaFunction "getWeaponClass"  (unitGetWeaponClassFn env)
  registerLuaFunction "modifyItemFill" (unitModifyItemFillFn env)
  registerLuaFunction "modifyItemFillById" (unitModifyItemFillByIdFn env)
  registerLuaFunction "repairItem"     (unitRepairItemFn env)
  registerLuaFunction "addItem"        (unitAddItemFn env)
  registerLuaFunction "getItemTemp"    (unitGetItemTempFn env)
  registerLuaFunction "setItemTemp"    (unitSetItemTempFn env)
  registerLuaFunction "getVisibleTiles" (unitGetVisibleTilesFn env)
  registerLuaFunction "getFrameTexture" (unitGetFrameTextureFn env)
  registerLuaFunction "getPortraitTexture" (unitGetPortraitTextureFn env)
  registerLuaFunction "addModifier"    (unitAddModifierFn env)
  registerLuaFunction "removeModifier" (unitRemoveModifierFn env)
  registerLuaFunction "getModifiers"   (unitGetModifiersFn env)
  registerLuaFunction "clearModifiers" (unitClearModifiersFn env)
  registerLuaFunction "getAllIds"   (unitGetAllIdsFn env)
  registerLuaFunction "getActivity" (unitGetActivityFn env)
  registerLuaFunction "getCurrentAnim" (unitGetCurrentAnimFn env)
  registerLuaFunction "getJumpReach" (unitGetJumpReachFn env)
  registerLuaFunction "lungeImpactSpeed" (unitLungeImpactSpeedFn env)
  registerLuaFunction "getSkill"     (unitGetSkillFn env)
  -- NB: skill functions follow; the building global is set up after
  -- this `unit` table is finalized below.
  registerLuaFunction "setSkill"     (unitSetSkillFn env)
  registerLuaFunction "getKnowledge"     (unitGetKnowledgeFn env)
  registerLuaFunction "setKnowledge"     (unitSetKnowledgeFn env)
  registerLuaFunction "getKnowledgeList" (unitGetKnowledgeListFn env)
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
  registerLuaFunction "getActiveIds"        (buildingGetActiveIdsFn env)
  registerLuaFunction "listDefs"            (buildingListDefsFn env)
  registerLuaFunction "hitTestAt"           (buildingHitTestAtFn env)
  registerLuaFunction "select"              (buildingSelectFn env)
  registerLuaFunction "deselect"            (buildingDeselectFn env)
  registerLuaFunction "getSelected"         (buildingGetSelectedFn env)
  registerLuaFunction "setSpawnRemaining"   (buildingSetSpawnRemainingFn env)
  registerLuaFunction "getSpawnRemaining"   (buildingGetSpawnRemainingFn env)
  registerLuaFunction "consumeSpawn"        (buildingConsumeSpawnFn env)
  registerLuaFunction "getBuildProgress"    (buildingGetBuildProgressFn env)
  registerLuaFunction "getBuildRequired"    (buildingGetBuildRequiredFn env)
  registerLuaFunction "addBuildProgress"    (buildingAddBuildProgressFn env)
  registerLuaFunction "getMaterialNeed"     (buildingGetMaterialNeedFn env)
  registerLuaFunction "getMaterialDelivered" (buildingGetMaterialDeliveredFn env)
  registerLuaFunction "areMaterialsSatisfied" (buildingAreMaterialsSatisfiedFn env)
  registerLuaFunction "getStorage"          (buildingGetStorageFn env)
  registerLuaFunction "getStorageCapacity"  (buildingGetStorageCapacityFn env)
  registerLuaFunction "getStorageWeight"    (buildingGetStorageWeightFn env)
  registerLuaFunction "getOperations"       (buildingGetOperationsFn env)
  registerLuaFunction "findStation"         (buildingFindStationFn env)
  Lua.setglobal (Lua.Name "building")

  -- Structure global — debug builder for walls / floors / ceilings.
  -- structure.place(gx,gy,slot,texHandle,faceHandle[,z]) / clear / clearAll / count.
  Lua.newtable
  registerLuaFunction "place"    (structurePlaceFn env)
  registerLuaFunction "clear"    (structureClearFn env)
  registerLuaFunction "clearAll" (structureClearAllFn env)
  registerLuaFunction "count"    (structureCountFn env)
  registerLuaFunction "loadedCount" (structureLoadedCountFn env)
  registerLuaFunction "unresolvedPaletteIds" (structureUnresolvedPaletteIdsFn env)
  registerLuaFunction "setPaletteHandle" (structureSetPaletteHandleFn env)
  registerLuaFunction "paletteCount" (structurePaletteCountFn env)
  registerLuaFunction "floorZAt" (structureFloorZAtFn env)
  registerLuaFunction "hasAt"    (structureHasAtFn env)
  registerLuaFunction "getAt"    (structureGetAtFn env)
  Lua.setglobal (Lua.Name "structure")

  -- Construction designation tool (#95). Mirrors the mine-designation
  -- API: the tool drives setAnchor/clearAnchor/designate, the build AI
  -- (#96) drives getPendingJobs/nearestDesignation/setJobStatus.
  Lua.newtable
  registerLuaFunction "setAnchor"          (constructSetAnchorFn env)
  registerLuaFunction "clearAnchor"        (constructClearAnchorFn env)
  registerLuaFunction "designate"          (constructDesignateFn env)
  registerLuaFunction "cancelDesignation"  (constructCancelDesignationFn env)
  registerLuaFunction "getPendingJobs"     (constructGetPendingJobsFn env)
  registerLuaFunction "getDesignationAt"   (constructGetDesignationAtFn env)
  registerLuaFunction "getDesignationCount" (constructGetDesignationCountFn env)
  registerLuaFunction "nearestDesignation" (constructNearestDesignationFn env)
  registerLuaFunction "setJobStatus"       (constructSetJobStatusFn env)
  registerLuaFunction "addJobProgress"     (constructAddJobProgressFn env)
  registerLuaFunction "setDesignateTexture" (constructSetDesignateTextureFn env)
  registerLuaFunction "setLineMode"        (constructSetLineModeFn env)
  Lua.setglobal (Lua.Name "construction")

  -- Chop designation tool (#97). Mirrors the construction-designation
  -- API: the tool drives setAnchor/clearAnchor/designate, the chop AI
  -- drives nearestDesignation/getDesignationAt/cancelDesignation
  -- (claims are Lua-side, like dig jobs — no engine job status).
  Lua.newtable
  registerLuaFunction "setAnchor"           (chopSetAnchorFn env)
  registerLuaFunction "clearAnchor"         (chopClearAnchorFn env)
  registerLuaFunction "designate"           (chopDesignateFn env)
  registerLuaFunction "cancelDesignation"   (chopCancelDesignationFn env)
  registerLuaFunction "getDesignationAt"    (chopGetDesignationAtFn env)
  registerLuaFunction "getDesignationCount" (chopGetDesignationCountFn env)
  registerLuaFunction "nearestDesignation"  (chopNearestDesignationFn env)
  registerLuaFunction "setDesignateTexture" (chopSetDesignateTextureFn env)
  Lua.setglobal (Lua.Name "chop")

  -- Till designation tool (#333). Mirrors the chop-designation API:
  -- the tool drives setAnchor/clearAnchor/designate, the till AI drives
  -- nearestDesignation/getDesignationAt/cancelDesignation (claims are
  -- Lua-side, like dig/chop jobs — no engine job status).
  Lua.newtable
  registerLuaFunction "setAnchor"           (tillSetAnchorFn env)
  registerLuaFunction "clearAnchor"         (tillClearAnchorFn env)
  registerLuaFunction "designate"           (tillDesignateFn env)
  registerLuaFunction "cancelDesignation"   (tillCancelDesignationFn env)
  registerLuaFunction "getDesignationAt"    (tillGetDesignationAtFn env)
  registerLuaFunction "getDesignationCount" (tillGetDesignationCountFn env)
  registerLuaFunction "nearestDesignation"  (tillNearestDesignationFn env)
  registerLuaFunction "setDesignateTexture" (tillSetDesignateTextureFn env)
  Lua.setglobal (Lua.Name "till")

  -- Plant designation tool (#335). Single-tile, no anchor: the tool
  -- drives designate, the farm AI (#336) drives nearestDesignation/
  -- getDesignationAt/cancelDesignation (claims are Lua-side, like dig/
  -- chop/till jobs — no engine job status).
  Lua.newtable
  registerLuaFunction "designate"           (plantDesignateFn env)
  registerLuaFunction "cancelDesignation"   (plantCancelDesignationFn env)
  registerLuaFunction "getDesignationAt"    (plantGetDesignationAtFn env)
  registerLuaFunction "getDesignationCount" (plantGetDesignationCountFn env)
  registerLuaFunction "nearestDesignation"  (plantNearestDesignationFn env)
  registerLuaFunction "setDesignateTexture" (plantSetDesignateTextureFn env)
  Lua.setglobal (Lua.Name "plant")

  -- Equipment global.
  -- Read: getClass / getClassNames / getLoadout.
  -- Write: equip / unequip (with kind validation against slot's accepted kind).
  Lua.newtable
  registerLuaFunction "getClass"      (equipmentGetClassFn env)
  registerLuaFunction "getClassNames" (equipmentGetClassNamesFn env)
  registerLuaFunction "getLoadout"    (equipmentGetLoadoutFn env)
  registerLuaFunction "equip"           (equipmentEquipFn env)
  registerLuaFunction "unequip"         (equipmentUnequipFn env)
  registerLuaFunction "equipAccessory"  (equipmentEquipAccessoryFn env)
  registerLuaFunction "unequipAccessory"(equipmentUnequipAccessoryFn env)
  registerLuaFunction "getAccessories"  (equipmentGetAccessoriesFn env)
  Lua.setglobal (Lua.Name "equipment")

  -- Substance global — read-only access to material physical
  -- properties (density, tensile, fracture toughness, …). Loaded
  -- from data/substances/*.yaml via engine.loadSubstanceYaml.
  Lua.newtable
  registerLuaFunction "get"      (substanceGetFn env)
  registerLuaFunction "getNames" (substanceGetNamesFn env)
  Lua.setglobal (Lua.Name "substance")

  -- Infection global — read-only access to the infection catalogue
  -- (staph, gas gangrene, …) loaded from data/infections/*.yaml via
  -- engine.loadInfectionYaml.
  Lua.newtable
  registerLuaFunction "get"      (infectionGetFn env)
  registerLuaFunction "getNames" (infectionGetNamesFn env)
  Lua.setglobal (Lua.Name "infection")

  -- Craft global — the crafting recipe catalogue (#325), loaded from
  -- data/recipes/*.yaml via engine.loadRecipeYaml. get/getNames are
  -- read-only queries; execute runs one craft against a unit's
  -- inventory (verify + consume inputs/fuel, produce outputs);
  -- executeAt (#326) additionally requires a Built work station
  -- offering the recipe's station kind with the unit adjacent.
  -- The bill verbs (#329) manage the active world's per-station
  -- standing-order queue (Craft.Bills): addBill / cancelBill /
  -- getBill / getBills are the queue surface (UI #330);
  -- claimBill / releaseBill / addBillProgress / completeBillCycle
  -- are the craft AI's job lifecycle. setBillPaused / reorderBill are
  -- the #330 station panel's pause + manual-reorder controls.
  Lua.newtable
  registerLuaFunction "get"      (craftGetFn env)
  registerLuaFunction "getNames" (craftGetNamesFn env)
  registerLuaFunction "execute"  (craftExecuteFn env)
  registerLuaFunction "executeAt" (craftExecuteAtFn env)
  registerLuaFunction "addBill"           (craftAddBillFn env)
  registerLuaFunction "cancelBill"        (craftCancelBillFn env)
  registerLuaFunction "getBill"           (craftGetBillFn env)
  registerLuaFunction "getBills"          (craftGetBillsFn env)
  registerLuaFunction "claimBill"         (craftClaimBillFn env)
  registerLuaFunction "releaseBill"       (craftReleaseBillFn env)
  registerLuaFunction "addBillProgress"   (craftAddBillProgressFn env)
  registerLuaFunction "completeBillCycle" (craftCompleteBillCycleFn env)
  registerLuaFunction "setBillPaused"     (craftSetBillPausedFn env)
  registerLuaFunction "setBillWorking"    (craftSetBillWorkingFn env)
  registerLuaFunction "reorderBill"       (craftReorderBillFn env)
  Lua.setglobal (Lua.Name "craft")

  -- Power global (#358) — the placeable power-node registry. placeNode
  -- pops a solar_panel/high_voltage_battery item out of a unit's
  -- inventory and turns it into a persistent power node; getNode /
  -- getNodeForBuilding / listNodes are read-only queries reporting each
  -- node's role + parameters. listNetworks / getNetworkForNode (#360)
  -- report the live wire-connectivity view: which nodes share a network
  -- and its current generation/drain/stored/capacity/powered status.
  Lua.newtable
  registerLuaFunction "isPlaceable"       (powerIsPlaceableFn env)
  registerLuaFunction "placeNode"         (powerPlaceNodeFn env)
  registerLuaFunction "getNode"           (powerGetNodeFn env)
  registerLuaFunction "getNodeForBuilding" (powerGetNodeForBuildingFn env)
  registerLuaFunction "listNodes"         (powerListNodesFn env)
  registerLuaFunction "listNetworks"       (powerListNetworksFn env)
  registerLuaFunction "getNetworkForNode"  (powerGetNetworkForNodeFn env)
  registerLuaFunction "isBuildingPowered"  (powerIsBuildingPoweredFn env)
  registerLuaFunction "isStationPoweredForRecipe"
                                            (powerIsStationPoweredForRecipeFn env)
  Lua.setglobal (Lua.Name "power")

  -- Repair global (#301) — the policy layer on top of unit.repairItem
  -- (#300): repair flows are recipe entries tagged with a repair axis
  -- (data/recipes/repair.yaml), gated on the same Built/adjacent
  -- station rules as craft.executeAt. get/getNames are read-only,
  -- restricted to repair-tagged recipes; repairAt runs one repair
  -- against a targeted item instance.
  Lua.newtable
  registerLuaFunction "get"      (repairGetFn env)
  registerLuaFunction "getNames" (repairGetNamesFn env)
  registerLuaFunction "repairAt" (repairAtFn env)
  Lua.setglobal (Lua.Name "repair")

  -- Blood global (#604) — the world-scoped blood decal model + debug
  -- surface: spawn a decal (reusing a near-matching generated-texture
  -- descriptor or minting + FIFO-evicting a new one), list current
  -- decals / texture descriptors, and clear both. No rendering, no
  -- combat hook, no real GPU texture generation yet (see Blood.Types +
  -- docs/blood_decals.md).
  Lua.newtable
  registerLuaFunction "spawn"          (bloodSpawnFn env)
  registerLuaFunction "getDecal"       (bloodGetDecalFn env)
  registerLuaFunction "listDecals"     (bloodListDecalsFn env)
  registerLuaFunction "getTexture"     (bloodGetTextureFn env)
  registerLuaFunction "listTextures"   (bloodListTexturesFn env)
  registerLuaFunction "getTextureCap"  (bloodGetTextureCapFn env)
  registerLuaFunction "clear"          (bloodClearFn env)
  Lua.setglobal (Lua.Name "blood")

  -- Loot table global — weighted rolls against data/loot_tables/*.yaml
  -- (loaded via engine.loadLootTableYaml). Consumed by a `loot_table`
  -- location content entry (#90).
  Lua.newtable
  registerLuaFunction "roll" (lootRollFn env)
  Lua.setglobal (Lua.Name "loot")

  Lua.newtable
  registerLuaFunction "listDefs"     (itemListDefsFn env)
  registerLuaFunction "spawnGround"  (itemSpawnGroundFn env)
  registerLuaFunction "listGround"   (itemListGroundFn env)
  registerLuaFunction "removeGround" (itemRemoveGroundFn env)
  registerLuaFunction "groundCount"  (itemGroundCountFn env)
  registerLuaFunction "getGroundTemp" (itemGetGroundTempFn env)
  registerLuaFunction "setGroundTemp" (itemSetGroundTempFn env)
  registerLuaFunction "hitTestAt"    (itemHitTestAtFn env)
  registerLuaFunction "select"       (itemSelectFn env)
  registerLuaFunction "deselect"     (itemDeselectFn env)
  registerLuaFunction "getSelected"  (itemGetSelectedFn env)
  registerLuaFunction "pickupGround" (itemPickupGroundFn env)
  registerLuaFunction "getFood"      (itemGetFoodFn env)
  registerLuaFunction "debugQuads"   (itemDebugQuadsFn env)
  Lua.setglobal (Lua.Name "item")

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
  registerLuaFunction "getDate" (worldGetDateFn env)
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
  registerLuaFunction "selectTile" (worldSelectTileFn env)
  registerLuaFunction "clearWorldCursorSelect" (worldClearWorldCursorSelectFn env)
  registerLuaFunction "setToolMode" (worldSetToolModeFn env)
  registerLuaFunction "getToolMode" (worldGetToolModeFn env)
  registerLuaFunction "setMineAnchor" (worldSetMineAnchorFn env)
  registerLuaFunction "clearMineAnchor" (worldClearMineAnchorFn env)
  registerLuaFunction "designateMine" (worldDesignateMineFn env)
  registerLuaFunction "setMineDesignateTexture"
    (worldSetMineDesignateTextureFn env)
  registerLuaFunction "getMineDesignationCount"
    (worldGetMineDesignationCountFn env)
  registerLuaFunction "nearestMineDesignation"
    (worldNearestMineDesignationFn env)
  registerLuaFunction "getDigInfoAt" (worldGetDigInfoAtFn env)
  registerLuaFunction "getSpoilInfo" (worldGetSpoilInfoFn env)
  registerLuaFunction "getGemInfoAt" (worldGetGemInfoAtFn env)
  registerLuaFunction "debugTileQuads" (worldDebugTileQuadsFn env)
  registerLuaFunction "addTile"       (worldAddTileFn env)
  registerLuaFunction "listMaterials" (worldListMaterialsFn env)
  registerLuaFunction "digTile" (worldDigTileFn env)
  registerLuaFunction "getMineDesignationAt"
    (worldGetMineDesignationAtFn env)
  registerLuaFunction "getInitProgress" (worldGetInitProgressFn env)
  registerLuaFunction "waitForInit" (worldWaitForInitFn env)
  registerLuaFunction "destroy" (worldDestroyFn env)
  registerLuaFunction "destroyAll" (worldDestroyAllFn env)
  registerLuaFunction "deleteTile" (worldDeleteTileFn env)
  registerLuaFunction "setFluidTile" (worldSetFluidTileFn env)
  registerLuaFunction "setSlope" (worldSetSlopeFn env)
  registerLuaFunction "setVegAt" (worldSetVegFn env)
  registerLuaFunction "setCell" (worldSetCellFn env)

  registerLuaFunction "getTerrainAt" (worldGetTerrainAtFn env)
  registerLuaFunction "getSlopeAt"   (worldGetSlopeAtFn env)
  registerLuaFunction "getVegAt"     (worldGetVegAtFn env)
  registerLuaFunction "isPlantable"  (worldIsPlantableFn env)
  registerLuaFunction "getFluidAt" (worldGetFluidAtFn env)
  registerLuaFunction "getSurfaceAt" (worldGetSurfaceAtFn env)
  registerLuaFunction "getChunkInfo" (worldGetChunkInfoFn env)
  registerLuaFunction "getAreaFluid" (worldGetAreaFluidFn env)
  registerLuaFunction "getRivers" (worldGetRiversFn env)
  registerLuaFunction "loadChunksInRegion" (worldLoadChunksInRegionFn env)
  registerLuaFunction "waitForChunks" (worldWaitForChunksFn env)
  registerLuaFunction "getHoverTile" (worldGetHoverTileFn env)
  registerLuaFunction "getHoverPos"  (worldGetHoverPosFn env)
  registerLuaFunction "pickTile"     (worldPickTileFn env)
  registerLuaFunction "pickPos"      (worldPickPosFn env)
  registerLuaFunction "getClimateAt" (worldGetClimateAtFn env)
  registerLuaFunction "getAmbientAt" (worldGetAmbientAtFn env)
  registerLuaFunction "getSunAngleAt" (worldGetSunAngleAtFn env)
  registerLuaFunction "listPlacedLocations" (worldListPlacedLocationsFn env)
  registerLuaFunction "hasSpawnedLocationContents"
    (worldHasSpawnedLocationContentsFn env)
  registerLuaFunction "markLocationContentsSpawned"
    (worldMarkLocationContentsSpawnedFn env)
  registerLuaFunction "hasStampedLocation"
    (worldHasStampedLocationFn env)
  registerLuaFunction "markLocationStamped"
    (worldMarkLocationStampedFn env)
  registerLuaFunction "getFloraAt" (worldGetFloraAtFn env)
  registerLuaFunction "getFloraGrowthAt" (worldGetFloraGrowthAtFn env)
  registerLuaFunction "harvestFlora" (worldHarvestFloraFn env)
  registerLuaFunction "findHarvestableFlora"
    (worldFindHarvestableFloraFn env)
  registerLuaFunction "plantCropAt" (worldPlantCropAtFn env)
  registerLuaFunction "getCropPlotAt" (worldGetCropPlotAtFn env)
  registerLuaFunction "getPlantSuitability" (worldGetPlantSuitabilityFn env)
  registerLuaFunction "plantRowCropAt" (worldPlantRowCropAtFn env)

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
  registerLuaFunction "applyScrollZoom" (cameraApplyScrollZoomFn env)
  registerLuaFunction "setZSlice" (cameraSetZSliceFn env)
  registerLuaFunction "getZSlice" (cameraGetZSliceFn env)
  registerLuaFunction "rotateCW" (cameraRotateCWFn env)
  registerLuaFunction "rotateCCW" (cameraRotateCCWFn env)
  registerLuaFunction "getFacing" (cameraGetFacingFn env)
  registerLuaFunction "getZTracking" (cameraGetZTrackingFn env)
  registerLuaFunction "setZTracking" (cameraSetZTrackingFn env)
  Lua.setglobal (Lua.Name "camera")

  -- Combat: queue attack commands + drain combat-thread events for
  -- the combat-log UI. Skeleton phase — `combat.attack` enqueues but
  -- the thread doesn't resolve yet; `combat.drainEvents` always
  -- returns empty.
  Lua.newtable
  registerLuaFunction "attack"      (combatAttackFn env)
  registerLuaFunction "drainEvents" (combatDrainEventsFn env)
  registerLuaFunction "emitDeath"   (combatEmitDeathFn env)
  Lua.setglobal (Lua.Name "combat")

  -- Injury: NON-combat wound stream (falls / hazards / wound-caused
  -- deaths) for the injury-log UI. Mirrors the combat event stream.
  Lua.newtable
  registerLuaFunction "emit"        (injuryEmitFn env)
  registerLuaFunction "drainEvents" (injuryDrainEventsFn env)
  Lua.setglobal (Lua.Name "injury")

  -- Thought: per-unit thought stream (#351) for the unit-log UI's
  -- Thought tab. Purely Lua-produced (scripts/thoughts.lua) — mirrors
  -- the injury event stream's shape and drain pattern.
  Lua.newtable
  registerLuaFunction "emit"        (thoughtEmitFn env)
  registerLuaFunction "drainEvents" (thoughtDrainEventsFn env)
  Lua.setglobal (Lua.Name "thought")
