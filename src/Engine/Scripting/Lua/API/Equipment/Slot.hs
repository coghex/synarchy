{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Slot-based equip/unequip (#67 exact-instance targeting). Moves an
--   inventory item into/out of a unit's equipment-class slots,
--   validating the slot's accepted kind. Accessory equip/unequip
--   (which manage uiAccessories + uiModifiers instead) live in the
--   sibling Equipment.Accessory module.
module Engine.Scripting.Lua.API.Equipment.Slot
    ( removeFirstFromInventoryWhere
    , equipmentEquipFn
    , equipmentUnequipFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Equipment.Types
import Item.Types (ItemInstance(..), lookupItemDef, itemMatches, idKind)
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..), UnitDef(..))

-- | Drop the first inventory entry matching a predicate, so equip can
--   target a specific 'iiInstanceId' (#67) — the clicked dagger, not the
--   first one matching its defName. Order of the surviving items is
--   preserved (the equipped slot is the only thing that moves).
removeFirstFromInventoryWhere ∷ (ItemInstance → Bool) → [ItemInstance]
                              → ([ItemInstance], Maybe ItemInstance)
removeFirstFromInventoryWhere p = go []
  where
    go acc [] = (reverse acc, Nothing)
    go acc (x:xs)
        | p x       = (reverse acc ++ xs, Just x)
        | otherwise = go (x : acc) xs

-- | equipment.equip(uid, slotId, itemDefName) → bool. Moves the first
--   inventory item matching @itemDefName@ into the named slot,
--   validating that its kind matches the slot's accepted kind. If the
--   slot already has something equipped, that item goes back to the
--   inventory. Returns false on: unknown unit, unknown slot for the
--   unit's class, kind mismatch, or no matching item in inventory.
equipmentEquipFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentEquipFn env = do
    uidArg   ← Lua.tointeger 1
    slotArg  ← Lua.tostring 2
    itemArg  ← Lua.tostring 3
    -- Optional 4th arg: equip the EXACT inventory instance (#67) — the
    -- dagger the player clicked, not the first defName match (which may
    -- be sharper/duller). Absent/0 → first match.
    instArg  ← Lua.tointeger 4
    case (uidArg, slotArg, itemArg) of
        (Just n, Just slotBS, Just itemBS) → do
            let uid    = UnitId (fromIntegral n)
                slotId = TE.decodeUtf8Lenient slotBS
                itemNm = TE.decodeUtf8Lenient itemBS
                wantId = maybe 0 fromIntegral instArg
            ok ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                ecMgr   ← readIORef (equipmentClassManagerRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            case HM.lookup (uiDefName inst) (umDefs um) of
                                Nothing → (um, False)
                                Just def → case udEquipmentClass def of
                                    Nothing → (um, False)
                                    Just clsNm →
                                        case lookupEquipmentClass clsNm ecMgr of
                                            Nothing → (um, False)
                                            Just cls →
                                                tryEquip um inst itemMgr cls
                                                  slotId itemNm wantId uid
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1
  where
    tryEquip um inst itemMgr cls slotId itemNm wantId uid =
        case [s | s ← ecSlots cls, esId s ≡ slotId] of
            []       → (um, False)
            (slot:_) →
                -- Pop the targeted instance FIRST, then validate the kind of
                -- the item we actually popped — NOT the caller-supplied
                -- defName. When targeting by instanceId, itemMatches ignores
                -- defName, so a mismatched (defName, id) pair (e.g.
                -- equip(slot="right_hand", "steel_dagger", canteenId)) would
                -- otherwise pass the kind gate on the dagger def yet slot the
                -- canteen. The pop is a pure computation; `um` is mutated only
                -- in the success branch below, so every failure here returns
                -- the ORIGINAL `um` untouched.
                case removeFirstFromInventoryWhere
                         (itemMatches wantId itemNm)
                         (uiInventory inst) of
                    (_, Nothing)        → (um, False)
                    (newInv, Just newI) → case lookupItemDef (iiDefName newI) itemMgr of
                        Nothing → (um, False)
                        Just iDef
                            | idKind iDef ≢ esKind slot → (um, False)
                            | otherwise →
                                let -- if something is already in the slot,
                                    -- bump it back to the inventory tail
                                    -- so the swap is atomic from Lua's
                                    -- perspective.
                                    existing = HM.lookup slotId
                                                 (uiEquipment inst)
                                    invAfter = case existing of
                                        Nothing → newInv
                                        Just e  → newInv ++ [e]
                                    eqAfter  = HM.insert slotId newI
                                                 (uiEquipment inst)
                                    inst'    = inst
                                        { uiInventory = invAfter
                                        , uiEquipment = eqAfter
                                        }
                                in (um { umInstances = HM.insert uid inst'
                                                         (umInstances um) },
                                    True)

-- | equipment.unequip(uid, slotId) → bool. Pops the item out of the
--   named slot and appends it to the unit's inventory. Returns false
--   if the unit doesn't exist or the slot was already empty.
equipmentUnequipFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentUnequipFn env = do
    uidArg  ← Lua.tointeger 1
    slotArg ← Lua.tostring 2
    case (uidArg, slotArg) of
        (Just n, Just slotBS) → do
            let uid    = UnitId (fromIntegral n)
                slotId = TE.decodeUtf8Lenient slotBS
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst → case HM.lookup slotId (uiEquipment inst) of
                        Nothing → (um, False)
                        Just it →
                            let inst' = inst
                                  { uiInventory = uiInventory inst ++ [it]
                                  , uiEquipment = HM.delete slotId
                                                    (uiEquipment inst)
                                  }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1
