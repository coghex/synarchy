{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Top-level unit API module. Re-exports the original public surface
--   from the sub-modules so existing call-sites keep working: Yaml is
--   unit-def loading, Spawn is lifecycle/position/pose, Survival is
--   drink/eat/feed, Query is combat-adjacent numeric getters, List is
--   listing/info/texture lookups, Combat is wound/blood/injury, Equipment
--   is item-fill/repair, Inventory is item-level inventory ops, Cargo is
--   building/unit/technomule transfers, Stats is skills/knowledge/stats/
--   modifiers, Selection is cursor selection + anim/facing state, and
--   Medical is bleed/infection/frostbite treatment (#546). The export
--   list here is the ORIGINAL pre-split export list verbatim — each
--   submodule exports a few extra internal-use helpers (needed for
--   cross-submodule imports) that intentionally stay unlisted here so
--   this module's public surface doesn't widen.
module Engine.Scripting.Lua.API.Units
    ( loadUnitYamlFn
    , unitSpawnFn
    , unitDestroyFn
    , unitSetPosFn
    , unitMoveToFn
    , unitJumpFn
    , unitStopFn
    , unitGetPosFn
    , unitGetInfoFn
    , unitListFn
    , unitListDefsFn
    , unitListAnimationsFn
    , unitSelectFn
    , unitDeselectAllFn
    , unitGetSelectedFn
    , unitIsSelectedFn
    , unitHitTestAtFn
    , unitHitTestInRectFn
    , unitSetSelectionFn
    , unitSetAnimFn
    , unitSetAnimOverrideFn
    , unitClearAnimOverrideFn
    , unitSetFacingFn
    , unitSetFrozenFn
    , unitSetForceLoopFn
    , unitCollapseFn
    , unitCrawlFn
    , unitReviveFn
    , unitKillFn
    , unitRecomputeBodyFn
    , unitGetStatFn
    , unitGetStatBaseFn
    , unitSetStatFn
    , unitGetAllStatsFn
    , unitAddModifierFn
    , unitRemoveModifierFn
    , unitGetModifiersFn
    , unitClearModifiersFn
    , unitGetAllIdsFn
    , unitGetActivityFn
    , unitGetCurrentAnimFn
    , unitGetJumpReachFn
    , unitLungeImpactSpeedFn
    , unitGetSkillFn
    , unitSetSkillFn
    , unitGetKnowledgeFn
    , unitSetKnowledgeFn
    , unitGetKnowledgeListFn
    , unitAddXPFn
    , unitGetAllSkillsFn
    , unitGetInventoryFn
    , unitGetItemContentsFn
    , unitTreatBleedingFn
    , unitTreatInfectionFn
    , unitFrostbiteFn
    , unitInjureFn
    , unitDrinkFn
    , unitEatFn
    , unitFeedFn
    , unitGetCaloriesFn
    , unitPickupFn
    , unitRemoveItemFn
    , unitTransferItemToBuildingFn
    , unitTransferItemToUnitFn
    , unitDepositToCargoFn
    , unitWithdrawFromCargoFn
    , unitGetCarryingWeightFn
    , unitTransitionToFn
    , unitGetPoseFn
    , unitGetFactionFn
    , unitExistsFn
    , unitGetAttackRangeFn
    , unitGetAttackCooldownFn
    , unitGetAnimDurationFn
    , unitGetMaxSpeedFn
    , unitGetEquippedWeaponWeightFn
    , unitGetWeaponWieldedFromFn
    , unitGetWoundSeverityOnFn
    , unitGetWoundsFn
    , unitGetScarsFn
    , unitGetImmunitiesFn
    , unitGetInsulationFn
    , unitDropEquipmentToGroundFn
    , unitDropItemToGroundFn
    , unitDropItemByIdFn
    , unitGetBloodFn
    , unitGetPainFn
    , unitGetMentalEffectivenessFn
    , unitGetLastAttackerFn
    , unitGetWeaponClassFn
    , unitModifyItemFillFn
    , unitModifyItemFillByIdFn
    , unitRepairItemFn
    , applyRepairToUnit
    , findHeldItemById
    , unitAddItemFn
    , unitGetItemTempFn
    , unitSetItemTempFn
    , unitGetVisibleTilesFn
    , unitGetFrameTextureFn
    , unitGetPortraitTextureFn
    , unknownUnitTexture
    , unknownUnitAnimFrame
    ) where

import Engine.Scripting.Lua.API.Units.Yaml
import Engine.Scripting.Lua.API.Units.Spawn
import Engine.Scripting.Lua.API.Units.Survival
import Engine.Scripting.Lua.API.Units.Query
import Engine.Scripting.Lua.API.Units.List
import Engine.Scripting.Lua.API.Units.Combat
import Engine.Scripting.Lua.API.Units.Equipment
import Engine.Scripting.Lua.API.Units.Inventory
import Engine.Scripting.Lua.API.Units.Cargo
import Engine.Scripting.Lua.API.Units.Stats
import Engine.Scripting.Lua.API.Units.Selection
import Engine.Scripting.Lua.API.Units.Medical
