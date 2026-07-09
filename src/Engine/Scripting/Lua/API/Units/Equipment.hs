{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units.Equipment
  ( unitModifyItemFillFn
  , unitModifyItemFillByIdFn
  , unitRepairItemFn
  , applyRepairToUnit
  )
    where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Unit.Types
import Unit.Stats (applyItemBuffs)
import Item.Types (ItemInstance(..), ItemDef(..), ItemContainer(..), ItemManager(..), lookupItemDef)


-- | unit.modifyItemFill(uid, defName, delta) → actual applied delta
--   (after clamp to [0, capacity]). Adjusts the fill of the FIRST item
--   matching defName. Returns 0 if no such item / no container / unit
--   missing.
unitModifyItemFillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitModifyItemFillFn env = do
    idArg    ← Lua.tointeger 1
    nameArg  ← Lua.tostring 2
    deltaArg ← Lua.tonumber 3
    case (idArg, nameArg, deltaArg) of
        (Just n, Just nameBS, Just (Lua.Number d)) → do
            let uid     = UnitId (fromIntegral n)
                defName = TE.decodeUtf8Lenient nameBS
                delta   = realToFrac d ∷ Float
            applied ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, 0 ∷ Float)
                        Just inst →
                            let (newInv, app) =
                                    adjustFirstFill itemMgr defName delta
                                        (uiInventory inst)
                                inst' = inst { uiInventory = newInv }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }, app)
            Lua.pushnumber (Lua.Number (realToFrac applied))
            return 1
        _ → do
            Lua.pushnumber 0
            return 1

-- | unit.modifyItemFillById(uid, instanceId, delta) → actual applied
--   delta (after clamp to [0, capacity]), or nil if the unit doesn't
--   exist or holds no inventory instance with that id. Same clamp
--   semantics as unit.modifyItemFill, but targets a SPECIFIC instance
--   rather than the first item matching a defName — needed once a unit
--   can hold more than one instance of the same container def (e.g. an
--   already-empty and a freshly-brewed coffee_pot at once, #347):
--   modifyItemFill's first-match semantics would silently drain the
--   WRONG instance in that case.
unitModifyItemFillByIdFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitModifyItemFillByIdFn env = do
    idArg    ← Lua.tointeger 1
    instArg  ← Lua.tointeger 2
    deltaArg ← Lua.tonumber 3
    case (idArg, instArg, deltaArg) of
        (Just n, Just iidI, Just (Lua.Number d)) → do
            let uid   = UnitId (fromIntegral n)
                iid   = fromIntegral iidI ∷ Word64
                delta = realToFrac d ∷ Float
            mApplied ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, Nothing)
                        Just inst →
                            case adjustFillById itemMgr iid delta
                                     (uiInventory inst) of
                                Nothing → (um, Nothing)
                                Just (inv', applied) →
                                    let inst' = inst { uiInventory = inv' }
                                    in ( um { umInstances = HM.insert uid inst'
                                                            (umInstances um) }
                                       , Just applied )
            case mApplied of
                Just applied → do
                    Lua.pushnumber (Lua.Number (realToFrac applied))
                    return 1
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | unit.repairItem(uid, instanceId, conditionDelta[, sharpnessDelta])
--   → table | nil. The low-level repair primitive (#300): the single
--   verb that ADJUSTS an item instance's two wear axes —
--   `iiCondition` (structural wear, gates breakage) and `iiSharpness`
--   (edge keenness, gates penetration) — in place, PRESERVING the
--   exact `iiInstanceId` (#67) so the physical item keeps its identity.
--
--   The item is found by `instanceId` across the unit's inventory,
--   equipment, AND worn accessories (so a degraded weapon can be
--   restored whether it's stowed or wielded, and a worn item like the
--   technogoggles is reachable too). Both deltas are applied additively
--   and the RESULT is clamped to 0..100. Positive restores (repair);
--   negative wears (the inverse — used by hazards / tests).
--   `sharpnessDelta` defaults to 0, so a furnace can pass condition-only
--   and a whetstone sharpness-only.
--
--   Repairing a WORN accessory also refreshes the unit's `uiModifiers`,
--   because accessory buffs can be condition-scaled (technogoggles'
--   perception buff) and are baked into the modifier map at equip time —
--   without the refresh a repaired accessory would keep its degraded
--   buff. (Equipped weapons/armour don't bake buffs into `uiModifiers`,
--   so the inventory/equipment branches need no such refresh.)
--
--   This is deliberately policy-free: restore *rates*, station↔axis
--   mapping, broken-item rules, and resource cost all live above it
--   (#301). It just moves the numbers and reports what actually changed.
--
--   Returns a table { defName, condition, sharpness, conditionApplied,
--   sharpnessApplied } with the post-clamp axis values and the actual
--   applied deltas (0 when an axis is already at a bound), or nil if the
--   unit doesn't exist or holds no instance with that id.
unitRepairItemFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitRepairItemFn env = do
    idArg    ← Lua.tointeger 1
    instArg  ← Lua.tointeger 2
    condArg  ← Lua.tonumber 3
    sharpArg ← Lua.tonumber 4
    case (idArg, instArg) of
        (Just n, Just iidI) → do
            let uid    = UnitId (fromIntegral n)
                iid    = fromIntegral iidI ∷ Word64
                condD  = case condArg of
                    Just (Lua.Number d) → realToFrac d ∷ Float
                    _                   → 0
                sharpD = case sharpArg of
                    Just (Lua.Number d) → realToFrac d ∷ Float
                    _                   → 0
            -- For the accessory branch we may need the item def to refresh
            -- condition-scaled buffs; read the manager once up front.
            itemMgr ← Lua.liftIO $ readIORef (itemManagerRef env)
            mRes ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing   → (um, Nothing)
                    Just inst → case applyRepairToUnit iid condD sharpD itemMgr inst of
                        Nothing            → (um, Nothing)
                        Just (inst', r) →
                            ( um { umInstances = HM.insert uid inst'
                                                           (umInstances um) }
                            , Just r )
            case mRes of
                Nothing → Lua.pushnil >> return 1
                Just (defName, cond1, sharp1, cApp, sApp) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 defName)
                    Lua.setfield (-2) "defName"
                    Lua.pushnumber (Lua.Number (realToFrac cond1))
                    Lua.setfield (-2) "condition"
                    Lua.pushnumber (Lua.Number (realToFrac sharp1))
                    Lua.setfield (-2) "sharpness"
                    Lua.pushnumber (Lua.Number (realToFrac cApp))
                    Lua.setfield (-2) "conditionApplied"
                    Lua.pushnumber (Lua.Number (realToFrac sApp))
                    Lua.setfield (-2) "sharpnessApplied"
                    return 1
        _ → Lua.pushnil >> return 1

-- | Apply repair deltas to one instance, clamping each axis to 0..100.
--   Returns the new instance plus a report tuple (defName, newCondition,
--   newSharpness, conditionApplied, sharpnessApplied) where the *applied*
--   figures are the post-clamp differences — so a caller/test sees that
--   topping up an already-full axis applied 0.
applyRepair ∷ Float → Float → ItemInstance
            → (ItemInstance, (Text, Float, Float, Float, Float))
applyRepair condD sharpD it =
    let cond0  = iiCondition it
        sharp0 = iiSharpness it
        cond1  = clampWear (cond0 + condD)
        sharp1 = clampWear (sharp0 + sharpD)
    in ( it { iiCondition = cond1, iiSharpness = sharp1 }
       , (iiDefName it, cond1, sharp1, cond1 - cond0, sharp1 - sharp0) )
  where clampWear x = max 0 (min 100 x)

-- | Repair the first instance in a list whose id matches; Nothing if
--   none does. Preserves list order (the slot is rewritten in place).
--   Also returns the repaired instance so the caller can react to its
--   new condition (the accessory branch re-derives its buffs).
repairInList ∷ Word64 → Float → Float → [ItemInstance]
             → Maybe ([ItemInstance], ItemInstance
                     , (Text, Float, Float, Float, Float))
repairInList _   _     _      []     = Nothing
repairInList iid condD sharpD (x:xs)
    | iiInstanceId x ≡ iid =
        let (x', r) = applyRepair condD sharpD x in Just (x' : xs, x', r)
    | otherwise =
        (\(rest, ri, r) → (x : rest, ri, r))
            <$> repairInList iid condD sharpD xs

-- | Repair the equipped instance whose id matches; Nothing if no slot
--   holds it.
repairInEquip ∷ Word64 → Float → Float → HM.HashMap Text ItemInstance
              → Maybe ( HM.HashMap Text ItemInstance, ItemInstance
                      , (Text, Float, Float, Float, Float) )
repairInEquip iid condD sharpD eq =
    case [ (slot, it) | (slot, it) ← HM.toList eq, iiInstanceId it ≡ iid ] of
        ((slot, it) : _) →
            let (it', r) = applyRepair condD sharpD it
            in Just (HM.insert slot it' eq, it', r)
        [] → Nothing

-- | Re-derive worn-accessory buffs into the unit's `uiModifiers` after a
--   repair changed one accessory's condition, so a condition-scaled buff
--   (e.g. technogoggles' perception) tracks the repair. Folds the buffs
--   of EVERY worn accessory over the modifier map IN LIST ORDER, exactly
--   as a fresh sequence of `equipAccessory` calls would: same-source
--   modifiers collapse (dedup by display_name) so the LAST-equipped copy
--   of a duplicated accessory wins. Re-deriving from the whole list — not
--   just the repaired instance — is what keeps repairing an *older*
--   duplicate from silently switching the live buff to its condition.
refreshAccessoryBuffs ∷ ItemManager → [ItemInstance]
                      → HM.HashMap Text [StatModifier]
                      → HM.HashMap Text [StatModifier]
refreshAccessoryBuffs itemMgr accs mods0 =
    foldl' (\mods inst → applyAccessoryBuffs itemMgr inst mods) mods0 accs

-- | The pure core of unit.repairItem (#300) and the repair.repairAt
--   policy layer (#301): search inventory, then equipment, then worn
--   accessories for `iid` and apply the deltas there, refreshing
--   condition-scaled accessory buffs same as above. Nothing if the unit
--   holds no instance with that id.
applyRepairToUnit ∷ Word64 → Float → Float → ItemManager → UnitInstance
                  → Maybe (UnitInstance, (Text, Float, Float, Float, Float))
applyRepairToUnit iid condD sharpD itemMgr inst =
    case repairInList iid condD sharpD (uiInventory inst) of
        Just (inv', _, r) → Just (inst { uiInventory = inv' }, r)
        Nothing →
          case repairInEquip iid condD sharpD (uiEquipment inst) of
            Just (eq', _, r) → Just (inst { uiEquipment = eq' }, r)
            Nothing →
              case repairInList iid condD sharpD (uiAccessories inst) of
                Just (accs', _, r) →
                    let mods' = refreshAccessoryBuffs itemMgr accs'
                                                       (uiModifiers inst)
                    in Just (inst { uiAccessories = accs'
                                  , uiModifiers   = mods' }, r)
                Nothing → Nothing

-- | Apply one accessory's buffs to a modifier map: def lookup + the
--   shared Unit.Stats.applyItemBuffs. The modifier source is the item's
--   display_name and same-source modifiers on a stat collapse, so this
--   REPLACES that source's stale modifier rather than stacking. A no-op
--   for items with no buffs (or no def in scope).
applyAccessoryBuffs ∷ ItemManager → ItemInstance
                    → HM.HashMap Text [StatModifier]
                    → HM.HashMap Text [StatModifier]
applyAccessoryBuffs itemMgr inst mods =
    case lookupItemDef (iiDefName inst) itemMgr of
        Nothing   → mods
        Just iDef → applyItemBuffs (idDisplayName iDef)
                                   (iiCondition inst)
                                   (idBuffs iDef) mods

-- | Helper: adjust the fill of the first ItemInstance matching defName.
--   Clamps to [0, capacity] looked up via the ItemManager. Returns the
--   resulting inventory and the actual applied delta (post-clamp).
adjustFirstFill
    ∷ ItemManager → Text → Float → [ItemInstance] → ([ItemInstance], Float)
adjustFirstFill itemMgr defName delta = go
  where
    go [] = ([], 0)
    go (x : xs)
      | iiDefName x ≡ defName =
          let cap = case lookupItemDef defName itemMgr of
                  Just d  → case idContainer d of
                      Just c  → icCapacity c
                      Nothing → iiCurrentFill x   -- no container → no headroom
                  Nothing → iiCurrentFill x
              newFill = max 0 (min cap (iiCurrentFill x + delta))
              applied = newFill - iiCurrentFill x
              x'      = x { iiCurrentFill = newFill }
          in (x' : xs, applied)
      | otherwise =
          let (xs', applied) = go xs
          in (x : xs', applied)

-- | Rewrite the fill of the instance with this id in a list, clamped to
--   [0, capacity] via the ItemManager. Nothing if no element matches.
--   Preserves order (in-place slot); mirrors adjustFirstFill but keyed
--   by instance id instead of defName.
adjustFillById
    ∷ ItemManager → Word64 → Float → [ItemInstance]
    → Maybe ([ItemInstance], Float)
adjustFillById _ _ _ [] = Nothing
adjustFillById itemMgr iid delta (x : xs)
    | iiInstanceId x ≡ iid =
        let cap = case lookupItemDef (iiDefName x) itemMgr of
                Just d  → case idContainer d of
                    Just c  → icCapacity c
                    Nothing → iiCurrentFill x   -- no container → no headroom
                Nothing → iiCurrentFill x
            newFill = max 0 (min cap (iiCurrentFill x + delta))
            applied = newFill - iiCurrentFill x
        in Just (x { iiCurrentFill = newFill } : xs, applied)
    | otherwise = do
        (xs', applied) ← adjustFillById itemMgr iid delta xs
        pure (x : xs', applied)
