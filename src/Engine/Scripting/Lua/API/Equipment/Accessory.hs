{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Accessory equip/unequip. Unlike slot equipment (Equipment.Slot),
--   accessories are an ordered list (uiAccessories) rather than a
--   named-slot map, and their buffs are folded into/out of the unit's
--   stat modifiers (uiModifiers) on equip/unequip.
module Engine.Scripting.Lua.API.Equipment.Accessory
    ( removeModifiersBySource
    , equipmentEquipAccessoryFn
    , equipmentUnequipAccessoryFn
    ) where

import UPrelude
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.API.Equipment.Slot (removeFirstFromInventoryWhere)
import Item.Types (ItemInstance(..), lookupItemDef, itemMatches, idKind,
                   idDisplayName, idUnequippable, idBuffs)
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..), StatModifier(..))
import Unit.Stats (applyItemBuffs)

-- | Remove every modifier with @source@ across all stats. Mirrors
--   `unit.removeModifier` exactly so equip/unequip stay symmetric.
removeModifiersBySource ∷ Text
                        → HM.HashMap Text [StatModifier]
                        → HM.HashMap Text [StatModifier]
removeModifiersBySource src =
    HM.map (filter (\m → smSource m ≢ src))

-- | equipment.equipAccessory(uid, itemDefName[, instanceId]) → bool.
--   Moves the matching inventory item (the exact instance when an id is
--   given, else the first defName match) to the end of the unit's
--   accessory list. Returns false if the unit or item doesn't exist, or
--   if the targeted item is not `kind: accessory` — only accessories
--   belong on uiAccessories, so a non-accessory is rejected with no
--   mutation (the accessory analogue of equip's slot-kind gate).
equipmentEquipAccessoryFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentEquipAccessoryFn env = do
    uidArg  ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    -- Optional 3rd arg: equip the EXACT inventory instance (#67).
    -- Absent/0 → first defName match.
    instArg ← Lua.tointeger 3
    case (uidArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid    = UnitId (fromIntegral n)
                defName = TE.decodeUtf8 nameBS
                wantId  = maybe 0 fromIntegral instArg
            ok ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            case removeFirstFromInventoryWhere
                                     (itemMatches wantId defName)
                                     (uiInventory inst) of
                                (_, Nothing) → (um, False)
                                (newInv, Just newI) →
                                    -- Validate the ACTUAL popped instance is an
                                    -- accessory before it joins uiAccessories —
                                    -- the kind gate slot-equip has, but for the
                                    -- accessory list (only `kind: accessory`
                                    -- items belong there). When targeting by id
                                    -- the defName arg is advisory, so a
                                    -- mismatched (defName, id) pair must not
                                    -- smuggle a canteen/weapon in. The pop is a
                                    -- pure computation; `um` is mutated only in
                                    -- the success branch, so a reject leaves it
                                    -- untouched.
                                    case lookupItemDef (iiDefName newI) itemMgr of
                                      Just d | idKind d ≡ "accessory" →
                                        -- Apply the accessory's buffs to the
                                        -- unit's modifier list so combat /
                                        -- stat display sees them immediately.
                                        let mods' = applyItemBuffs
                                                       (idDisplayName d)
                                                       (iiCondition newI)
                                                       (idBuffs d)
                                                       (uiModifiers inst)
                                            inst' = inst
                                              { uiInventory   = newInv
                                              , uiAccessories =
                                                  uiAccessories inst ++ [newI]
                                              , uiModifiers   = mods'
                                              }
                                        in (um { umInstances = HM.insert uid inst'
                                                                 (umInstances um) },
                                            True)
                                      _ → (um, False)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | equipment.unequipAccessory(uid, index) → bool. Pops the accessory
--   at the given 1-based index and appends it to the unit's
--   inventory. Returns false on missing unit, out-of-range index, or
--   if the accessory's def is flagged `unequippable`.
equipmentUnequipAccessoryFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentUnequipAccessoryFn env = do
    uidArg ← Lua.tointeger 1
    idxArg ← Lua.tointeger 2
    case (uidArg, idxArg) of
        (Just n, Just i) → do
            let uid    = UnitId (fromIntegral n)
                idx0   = fromIntegral i - 1
            ok ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let xs = uiAccessories inst
                            in if idx0 < 0 ∨ idx0 >= length xs
                                 then (um, False)
                                 else
                                   let target = xs !! idx0
                                       mDef = lookupItemDef
                                                (iiDefName target) itemMgr
                                       defLocked = case mDef of
                                           Just d  → idUnequippable d
                                           Nothing → False
                                   in if defLocked
                                       then (um, False)
                                       else
                                         let xs' = take idx0 xs
                                                 ++ drop (idx0 + 1) xs
                                             -- Remove the accessory's
                                             -- buffs from the unit's
                                             -- modifier list (by source =
                                             -- item display_name).
                                             mods' = case mDef of
                                                Just d  → removeModifiersBySource
                                                            (idDisplayName d)
                                                            (uiModifiers inst)
                                                Nothing → uiModifiers inst
                                             inst' = inst
                                               { uiAccessories = xs'
                                               , uiInventory   =
                                                   uiInventory inst ++ [target]
                                               , uiModifiers   = mods'
                                               }
                                         in (um { umInstances =
                                                    HM.insert uid inst'
                                                      (umInstances um) },
                                             True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1
