module Engine.Scripting.Lua.API.Register.Unit
  ( registerUnitAPI
  ) where

import Engine.Scripting.Lua.CallStats (LuaCallStats)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Units
-- The #1000 transfer contract is imported directly rather than through
-- the Units facade: that facade's export list is deliberately frozen at
-- the pre-split surface (see its header).
import Engine.Scripting.Lua.API.Units.Transfer
-- Durable transfer ORDERS (#1247) — the walk-then-commit executor's
-- own verbs, kept in their own module beside the contract they drive.
import Engine.Scripting.Lua.API.Units.TransferOrder
-- #2297's two read-only medical queries, imported directly for the
-- same reason as the transfer verbs above: the Units facade's export
-- list is frozen at the pre-split surface.
import Engine.Scripting.Lua.API.Units.Medical
    (unitCanTreatFn, unitTreatmentRangeFn)
-- #2468's atomic relative stance credit, imported directly for the same
-- reason: the Units facade's export list is frozen at the pre-split
-- surface. Its own module because it is the one verb that must commit
-- against the SAME reference combat debits, not another stat accessor.
import Engine.Scripting.Lua.API.Units.Stance (unitRecoverStanceFn)
-- #2470's atomic stamina commit, imported directly for the same reason.
-- Its own module beside the stance verb: it likewise commits against the
-- SAME reference combat debits, and additionally REPORTS what it
-- committed so the physiology tick's exhaustion rules read storage
-- rather than a script-side estimate.
import Engine.Scripting.Lua.API.Units.Stamina (unitCommitStaminaFn)
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | Populate and install the @unit@ global table.
registerUnitAPI ∷ LuaCallStats → EngineEnv → Lua.LuaE Lua.Exception ()
registerUnitAPI callStats env = do
  Lua.newtable

  registerLuaFunction callStats "unit" "spawn"       (unitSpawnFn env)
  registerLuaFunction callStats "unit" "destroy"     (unitDestroyFn env)
  registerLuaFunction callStats "unit" "setPos"      (unitSetPosFn env)
  registerLuaFunction callStats "unit" "getPos"      (unitGetPosFn env)
  registerLuaFunction callStats "unit" "getInfo"     (unitGetInfoFn env)
  registerLuaFunction callStats "unit" "list"        (unitListFn env)
  registerLuaFunction callStats "unit" "listDefs"    (unitListDefsFn env)
  registerLuaFunction callStats "unit" "listAnimations" (unitListAnimationsFn env)
  registerLuaFunction callStats "unit" "moveTo"      (unitMoveToFn env)
  registerLuaFunction callStats "unit" "setMoveSpeed" (unitSetMoveSpeedFn env)
  registerLuaFunction callStats "unit" "jump"        (unitJumpFn env)
  registerLuaFunction callStats "unit" "stop"        (unitStopFn env)
  registerLuaFunction callStats "unit" "select"      (unitSelectFn env)
  registerLuaFunction callStats "unit" "deselectAll" (unitDeselectAllFn env)
  registerLuaFunction callStats "unit" "getSelected" (unitGetSelectedFn env)
  registerLuaFunction callStats "unit" "isSelected"  (unitIsSelectedFn env)
  registerLuaFunction callStats "unit" "hitTestAt"   (unitHitTestAtFn env)
  registerLuaFunction callStats "unit" "hitTestInRect" (unitHitTestInRectFn env)
  registerLuaFunction callStats "unit" "setSelection" (unitSetSelectionFn env)
  registerLuaFunction callStats "unit" "setAnim"     (unitSetAnimFn env)
  registerLuaFunction callStats "unit" "setAnimOverride"   (unitSetAnimOverrideFn env)
  registerLuaFunction callStats "unit" "clearAnimOverride" (unitClearAnimOverrideFn env)
  registerLuaFunction callStats "unit" "setFacing"   (unitSetFacingFn env)
  registerLuaFunction callStats "unit" "setFrozen"   (unitSetFrozenFn env)
  registerLuaFunction callStats "unit" "setForceLoop" (unitSetForceLoopFn env)
  registerLuaFunction callStats "unit" "collapse"    (unitCollapseFn env)
  registerLuaFunction callStats "unit" "crawl"       (unitCrawlFn env)
  registerLuaFunction callStats "unit" "revive"      (unitReviveFn env)
  registerLuaFunction callStats "unit" "kill"        (unitKillFn env)
  registerLuaFunction callStats "unit" "recomputeBody" (unitRecomputeBodyFn env)
  registerLuaFunction callStats "unit" "getStat"     (unitGetStatFn env)
  registerLuaFunction callStats "unit" "getStatBase" (unitGetStatBaseFn env)
  registerLuaFunction callStats "unit" "setStat"     (unitSetStatFn env)
  registerLuaFunction callStats "unit" "getAllStats" (unitGetAllStatsFn env)
  registerLuaFunction callStats "unit" "recoverStance" (unitRecoverStanceFn env)
  registerLuaFunction callStats "unit" "commitStamina" (unitCommitStaminaFn env)
  registerLuaFunction callStats "unit" "getInventory" (unitGetInventoryFn env)
  registerLuaFunction callStats "unit" "getItemContents" (unitGetItemContentsFn env)
  registerLuaFunction callStats "unit" "treatBleeding" (unitTreatBleedingFn env)
  registerLuaFunction callStats "unit" "treatInfection" (unitTreatInfectionFn env)
  registerLuaFunction callStats "unit" "treatmentRange" (unitTreatmentRangeFn env)
  registerLuaFunction callStats "unit" "canTreat" (unitCanTreatFn env)
  registerLuaFunction callStats "unit" "frostbite"    (unitFrostbiteFn env)
  registerLuaFunction callStats "unit" "injure"       (unitInjureFn env)
  registerLuaFunction callStats "unit" "drink"        (unitDrinkFn env)
  registerLuaFunction callStats "unit" "eat"          (unitEatFn env)
  registerLuaFunction callStats "unit" "feed"         (unitFeedFn env)
  registerLuaFunction callStats "unit" "getCalories"  (unitGetCaloriesFn env)
  registerLuaFunction callStats "unit" "pickup"       (unitPickupFn env)
  registerLuaFunction callStats "unit" "removeItem"   (unitRemoveItemFn env)
  registerLuaFunction callStats "unit" "transferItemToBuilding" (unitTransferItemToBuildingFn env)
  registerLuaFunction callStats "unit" "transferItemToUnit"     (unitTransferItemToUnitFn env)
  registerLuaFunction callStats "unit" "depositToCargo"     (unitDepositToCargoFn env)
  registerLuaFunction callStats "unit" "withdrawFromCargo"  (unitWithdrawFromCargoFn env)
  registerLuaFunction callStats "unit" "getCarryingWeight"  (unitGetCarryingWeightFn env)
  registerLuaFunction callStats "unit" "checkTransfer"      (unitCheckTransferFn env)
  registerLuaFunction callStats "unit" "commitTransfer"     (unitCommitTransferFn env)
  registerLuaFunction callStats "unit" "transferContract"   (unitTransferContractFn env)
  registerLuaFunction callStats "unit" "transferEndpointInfo" (unitTransferEndpointInfoFn env)
  registerLuaFunction callStats "unit" "createTransferOrder"  (unitCreateTransferOrderFn env)
  registerLuaFunction callStats "unit" "getTransferOrders"    (unitGetTransferOrdersFn env)
  registerLuaFunction callStats "unit" "advanceTransferOrder" (unitAdvanceTransferOrderFn env)
  registerLuaFunction callStats "unit" "commitTransferOrder"  (unitCommitTransferOrderFn env)
  registerLuaFunction callStats "unit" "failTransferOrder"    (unitFailTransferOrderFn env)
  registerLuaFunction callStats "unit" "cancelTransferOrder"  (unitCancelTransferOrderFn env)
  registerLuaFunction callStats "unit" "pruneTransferOrder"   (unitPruneTransferOrderFn env)
  registerLuaFunction callStats "unit" "transitionTo" (unitTransitionToFn env)
  registerLuaFunction callStats "unit" "getPose"      (unitGetPoseFn env)
  registerLuaFunction callStats "unit" "getFaction"   (unitGetFactionFn env)
  registerLuaFunction callStats "unit" "exists"       (unitExistsFn env)
  registerLuaFunction callStats "unit" "getAttackRange" (unitGetAttackRangeFn env)
  registerLuaFunction callStats "unit" "getAttackCooldown" (unitGetAttackCooldownFn env)
  registerLuaFunction callStats "unit" "getAnimDuration" (unitGetAnimDurationFn env)
  registerLuaFunction callStats "unit" "getMaxSpeed"  (unitGetMaxSpeedFn env)
  registerLuaFunction callStats "unit" "getEquippedWeaponWeight"
                                          (unitGetEquippedWeaponWeightFn env)
  registerLuaFunction callStats "unit" "getWeaponWieldedFrom"
                                          (unitGetWeaponWieldedFromFn env)
  registerLuaFunction callStats "unit" "getWoundSeverityOn"
                                          (unitGetWoundSeverityOnFn env)
  registerLuaFunction callStats "unit" "getWounds"    (unitGetWoundsFn env)
  registerLuaFunction callStats "unit" "getScars"     (unitGetScarsFn env)
  registerLuaFunction callStats "unit" "getImmunities" (unitGetImmunitiesFn env)
  registerLuaFunction callStats "unit" "getInsulation" (unitGetInsulationFn env)
  registerLuaFunction callStats "unit" "dropEquipmentToGround" (unitDropEquipmentToGroundFn env)
  registerLuaFunction callStats "unit" "dropItemToGround" (unitDropItemToGroundFn env)
  registerLuaFunction callStats "unit" "dropItemById"     (unitDropItemByIdFn env)
  registerLuaFunction callStats "unit" "getBlood"     (unitGetBloodFn env)
  registerLuaFunction callStats "unit" "getPain"      (unitGetPainFn env)
  registerLuaFunction callStats "unit" "getMentalEffectiveness" (unitGetMentalEffectivenessFn env)
  registerLuaFunction callStats "unit" "getLastAttacker" (unitGetLastAttackerFn env)
  registerLuaFunction callStats "unit" "getWeaponClass"  (unitGetWeaponClassFn env)
  registerLuaFunction callStats "unit" "modifyItemFill" (unitModifyItemFillFn env)
  registerLuaFunction callStats "unit" "modifyItemFillById" (unitModifyItemFillByIdFn env)
  registerLuaFunction callStats "unit" "repairItem"     (unitRepairItemFn env)
  registerLuaFunction callStats "unit" "addItem"        (unitAddItemFn env)
  registerLuaFunction callStats "unit" "getItemTemp"    (unitGetItemTempFn env)
  registerLuaFunction callStats "unit" "setItemTemp"    (unitSetItemTempFn env)
  registerLuaFunction callStats "unit" "getVisibleTiles" (unitGetVisibleTilesFn env)
  registerLuaFunction callStats "unit" "getFrameTexture" (unitGetFrameTextureFn env)
  registerLuaFunction callStats "unit" "getFrameSample" (unitGetFrameSampleFn env)
  registerLuaFunction callStats "unit" "getPortraitTexture" (unitGetPortraitTextureFn env)
  registerLuaFunction callStats "unit" "addModifier"    (unitAddModifierFn env)
  registerLuaFunction callStats "unit" "removeModifier" (unitRemoveModifierFn env)
  registerLuaFunction callStats "unit" "getModifiers"   (unitGetModifiersFn env)
  registerLuaFunction callStats "unit" "clearModifiers" (unitClearModifiersFn env)
  registerLuaFunction callStats "unit" "getAllIds"   (unitGetAllIdsFn env)
  registerLuaFunction callStats "unit" "getActivity" (unitGetActivityFn env)
  registerLuaFunction callStats "unit" "getCurrentAnim" (unitGetCurrentAnimFn env)
  registerLuaFunction callStats "unit" "getJumpReach" (unitGetJumpReachFn env)
  registerLuaFunction callStats "unit" "lungeImpactSpeed" (unitLungeImpactSpeedFn env)
  registerLuaFunction callStats "unit" "getSkill"     (unitGetSkillFn env)
  registerLuaFunction callStats "unit" "setSkill"     (unitSetSkillFn env)
  registerLuaFunction callStats "unit" "getKnowledge"     (unitGetKnowledgeFn env)
  registerLuaFunction callStats "unit" "setKnowledge"     (unitSetKnowledgeFn env)
  registerLuaFunction callStats "unit" "getKnowledgeList" (unitGetKnowledgeListFn env)
  registerLuaFunction callStats "unit" "addXP"        (unitAddXPFn env)
  registerLuaFunction callStats "unit" "getAllSkills" (unitGetAllSkillsFn env)

  Lua.setglobal (Lua.Name "unit")
