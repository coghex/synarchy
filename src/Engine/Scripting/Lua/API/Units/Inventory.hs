{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units.Inventory
  ( unitAddItemFn
  , findHeldItemById
  , unitGetItemTempFn
  , unitSetItemTempFn
  , unitRemoveItemFn
  , unitDropEquipmentToGroundFn
  , unitDropItemToGroundFn
  , unitDropItemByIdFn
  , unitGetItemContentsFn
  , unitGetInventoryFn
  , removeFirstByName
  , popFirstByNameIx
  , insertAt
  , popFirstWhereIx
  )
    where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), activeWorldState, freshItemInstanceId)
import Unit.Types
import Engine.Asset.Handle (TextureHandle(..))
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Ground (spawnGroundItem)
import World.Types (WorldManager(..), WorldState(..), WorldGenParams(..))
import World.Weather.Ambient (ambientTempAt)
import Item.Temperature (effectiveItemTemp)
import Item.Types (ItemInstance(..), itemMatches, itemContentsSig, ItemDef(..), ItemContainer(..), ItemFood(..), ItemWeapon(..), ItemBuff(..), lookupItemDef, itemTotalWeight, qualityTierLabel)


-- | The active (shown) world, or the first one if none is explicitly
--   shown. Mirrors the helper in API.Items.
activeWorldU ∷ EngineEnv → IO (Maybe WorldState)
activeWorldU = activeWorldState

-- | unit.addItem(uid, defName, fill) → bool. Adds a new ItemInstance
--   to the unit's inventory. Fill is clamped to the def's container
--   capacity (or zeroed for non-containers). Returns false if the
--   unit or item def doesn't exist.
unitAddItemFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitAddItemFn env = do
    idArg    ← Lua.tointeger 1
    nameArg  ← Lua.tostring 2
    fillArg  ← Lua.tonumber 3
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid     = UnitId (fromIntegral n)
                defName = TE.decodeUtf8Lenient nameBS
                mFillIn = case fillArg of
                    Just (Lua.Number d) → Just (realToFrac d ∷ Float)
                    _ → Nothing
            ok ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                case lookupItemDef defName itemMgr of
                    Nothing → return False
                    Just def → do
                        -- No fill arg → the def's default_fill (a quinoa
                        -- sack arrives full); explicit fill wins. Clamped
                        -- to capacity, non-containers stay 0.
                        let clampedFill = case idContainer def of
                                Just c  → max 0 (min (icCapacity c)
                                            (fromMaybe (icDefaultFill c) mFillIn))
                                Nothing → 0
                        qual ← rollItemSpec (idQualitySpec def)
                                            (statRNGRef env)
                        cond ← rollItemSpec (idConditionSpec def)
                                            (statRNGRef env)
                        wght ← rollItemWeight def (statRNGRef env)
                        iid ← freshItemInstanceId env
                        let inst' = ItemInstance
                                { iiDefName     = defName
                                , iiCurrentFill = clampedFill
                                , iiQuality     = qual
                                , iiCondition   = cond
                                , iiWeight      = wght
                                , iiSharpness   = 100.0
                                , iiContents    = []
                                , iiInstanceId  = iid
                                , iiTemp        = Nothing
                                }
                        atomicModifyIORef' (unitManagerRef env) $ \um →
                            case HM.lookup uid (umInstances um) of
                                Nothing → (um, False)
                                Just u  →
                                    let u' = u { uiInventory =
                                                  uiInventory u ++ [inst'] }
                                    in (um { umInstances = HM.insert uid u'
                                                            (umInstances um) },
                                        True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | Ambient air temperature (°C) at a unit's tile, read from the unit's
--   OWN page's gen params — the same elevation-corrected helper thermo
--   and the item-cooling tick use (#308/#344). Nothing when the page has
--   no live world or no gen params yet.
unitAmbientTemp ∷ EngineEnv → UnitInstance → IO (Maybe Float)
unitAmbientTemp env inst = do
    wm ← readIORef (worldManagerRef env)
    case lookup (uiPage inst) (wmWorlds wm) of
        Nothing → pure Nothing
        Just ws → do
            mp ← readIORef (wsGenParamsRef ws)
            pure $ fmap (\p → ambientTempAt (wgpSeed p) (wgpPlates p)
                                  (wgpClimateState p) (wgpWorldSize p)
                                  (floor (uiGridX inst))
                                  (floor (uiGridY inst))) mp

-- | The instance with this id, searched across the unit's inventory,
--   equipment, and worn accessories (same reach as unit.repairItem).
findHeldItemById ∷ Word64 → UnitInstance → Maybe ItemInstance
findHeldItemById iid inst =
    case [ it | it ← uiInventory inst
                     ++ HM.elems (uiEquipment inst)
                     ++ uiAccessories inst
              , iiInstanceId it ≡ iid ] of
        (it:_) → Just it
        []     → Nothing

-- | Rewrite the temperature of the instance with this id in a list;
--   Nothing if no element matches. Preserves order (in-place slot).
setTempInList ∷ Word64 → Maybe Float → [ItemInstance]
              → Maybe [ItemInstance]
setTempInList _   _  []     = Nothing
setTempInList iid mT (x:xs)
    | iiInstanceId x ≡ iid = Just (x { iiTemp = mT } : xs)
    | otherwise            = (x :) ⊚ setTempInList iid mT xs

-- | unit.getItemTemp(uid, instanceId) → °C | nil. The item's effective
--   temperature (#344): its tracked iiTemp when it's hotter/colder than
--   its surroundings, else the ambient at the holder's tile — a held
--   item breathes the air its holder stands in. Searched across
--   inventory, equipment, and accessories like unit.repairItem. nil if
--   the unit or instance doesn't exist (or the item is untracked and no
--   world/params exist to read an ambient from).
unitGetItemTempFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetItemTempFn env = do
    idArg   ← Lua.tointeger 1
    instArg ← Lua.tointeger 2
    case (idArg, instArg) of
        (Just n, Just iidI) → do
            let uid = UnitId (fromIntegral n)
                iid = fromIntegral iidI ∷ Word64
            mT ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → pure Nothing
                    Just inst → case findHeldItemById iid inst of
                        Nothing → pure Nothing
                        Just it → do
                            mAmb ← unitAmbientTemp env inst
                            pure $ case mAmb of
                                Just amb → Just (effectiveItemTemp amb it)
                                Nothing  → iiTemp it
            case mT of
                Just t  → do
                    Lua.pushnumber (Lua.Number (realToFrac t))
                    return 1
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | unit.setItemTemp(uid, instanceId [, temp]) → bool. Sets the held
--   instance's tracked temperature (°C) — the generic "this item was
--   made hot/cold" hook (#344): cooking / smelting outputs call this
--   (or spawn with a temp prop) and the per-page tick then cools the
--   item toward ambient. Omitting temp (or passing nil) clears the
--   tracked value — the item snaps back to "at ambient". Same
--   instance-id targeting across inventory / equipment / accessories
--   as unit.repairItem. False if the unit or instance doesn't exist.
unitSetItemTempFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetItemTempFn env = do
    idArg   ← Lua.tointeger 1
    instArg ← Lua.tointeger 2
    tArg    ← Lua.tonumber 3
    case (idArg, instArg) of
        (Just n, Just iidI) → do
            let uid = UnitId (fromIntegral n)
                iid = fromIntegral iidI ∷ Word64
                mT  = case tArg of
                    Just (Lua.Number d) → Just (realToFrac d ∷ Float)
                    _                   → Nothing
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing   → (um, False)
                    Just inst →
                        let commit inst' = um { umInstances =
                                HM.insert uid inst' (umInstances um) }
                        in case setTempInList iid mT (uiInventory inst) of
                            Just inv' →
                                (commit inst { uiInventory = inv' }, True)
                            Nothing →
                              case [ slot | (slot, it) ← HM.toList
                                                (uiEquipment inst)
                                          , iiInstanceId it ≡ iid ] of
                                (slot:_) →
                                    let eq' = HM.adjust
                                            (\it → it { iiTemp = mT })
                                            slot (uiEquipment inst)
                                    in (commit inst { uiEquipment = eq' }
                                       , True)
                                [] →
                                  case setTempInList iid mT
                                           (uiAccessories inst) of
                                    Just accs' →
                                        (commit inst { uiAccessories =
                                            accs' }, True)
                                    Nothing → (um, False)
            Lua.pushboolean ok
            return 1
        _ → Lua.pushboolean False >> return 1

-- | unit.removeItem(uid, defName) → bool. Removes the FIRST inventory
--   instance with the matching defName. Returns true if something was
--   removed, false if the unit doesn't exist or has no such item.
--   Used by Phase 5 eat_from_inventory to consume food after the
--   nutrition is applied.
unitRemoveItemFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitRemoveItemFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid     = UnitId (fromIntegral n)
                defName = TE.decodeUtf8Lenient nameBS
            removed ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just u  →
                        case removeFirstByName defName (uiInventory u) of
                            Nothing      → (um, False)
                            Just newInv  →
                                let u' = u { uiInventory = newInv }
                                in (um { umInstances = HM.insert uid u'
                                                        (umInstances um) }, True)
            Lua.pushboolean removed
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | Drop the first ItemInstance whose defName matches. Returns Nothing
--   if no match (caller can distinguish "removed" from "not found").
removeFirstByName ∷ Text → [ItemInstance] → Maybe [ItemInstance]
removeFirstByName _ [] = Nothing
removeFirstByName name (x:xs)
    | iiDefName x ≡ name = Just xs
    | otherwise          = (x :) <$> removeFirstByName name xs

-- | Like 'removeFirstByName' but EXTRACTS the popped instance AND
--   reports the 0-based index it was removed from. Cross-owner
--   transfers carry this so a failed move can splice the instance back
--   at its ORIGINAL position rather than the front — UI
--   (unit.getInventory / building.getStorage) and gameplay rely on
--   stable insertion order, so a rolled-back transfer must leave the
--   source list byte-for-byte unchanged.
popFirstByNameIx ∷ Text → [ItemInstance]
                → Maybe (ItemInstance, Int, [ItemInstance])
popFirstByNameIx = go 0
  where
    go _ _    [] = Nothing
    go i name (x:xs)
        | iiDefName x ≡ name = Just (x, i, xs)
        | otherwise          = fmap (\(it, j, rest) → (it, j, x:rest))
                                    (go (i + 1) name xs)

-- | Insert @x@ at index @i@. If @i@ is past the end (the list shrank
--   under a concurrent edit between pop and rollback) it appends —
--   graceful degradation, never a crash.
insertAt ∷ Int → a → [a] → [a]
insertAt i x xs = let (pre, post) = splitAt i xs in pre ++ x : post

-- | Like 'popFirstByNameIx' but matches on an arbitrary predicate, so a
--   caller can target a specific 'iiInstanceId' (#67) instead of the
--   first defName match. Same all-or-nothing index reporting so a
--   rolled-back transfer can splice the instance back at its original
--   slot. Pair with 'itemMatches' to get id-then-defName fallback.
popFirstWhereIx ∷ (ItemInstance → Bool) → [ItemInstance]
                → Maybe (ItemInstance, Int, [ItemInstance])
popFirstWhereIx p = go 0
  where
    go _ [] = Nothing
    go i (x:xs)
        | p x       = Just (x, i, xs)
        | otherwise = fmap (\(it, j, rest) → (it, j, x:rest))
                           (go (i + 1) xs)

-- | unit.dropEquipmentToGround(uid, slotId) → bool
--
-- Removes whatever is equipped in `slotId` and drops it on the ground at
-- the unit's tile, PRESERVING the exact ItemInstance (condition /
-- sharpness / quality / fill). Used when a hand or arm is severed or
-- destroyed and the unit can no longer hold its weapon. Returns false if
-- the slot is empty, the unit is gone, or no world is active (the item is
-- left equipped rather than vanishing).
unitDropEquipmentToGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDropEquipmentToGroundFn env = do
    uidArg  ← Lua.tointeger 1
    slotArg ← Lua.tostring 2
    case (uidArg, slotArg) of
        (Just n, Just slotBS) → do
            let uid    = UnitId (fromIntegral n)
                slotId = TE.decodeUtf8Lenient slotBS
            -- Resolve the world FIRST so we never strip the item from the
            -- unit when there's nowhere to drop it.
            mWs ← Lua.liftIO $ activeWorldU env
            case mWs of
                Nothing → Lua.pushboolean False >> return 1
                Just ws → do
                    mDrop ← Lua.liftIO $
                        atomicModifyIORef' (unitManagerRef env) $ \um →
                            case HM.lookup uid (umInstances um) of
                                Nothing → (um, Nothing)
                                Just inst →
                                    case HM.lookup slotId (uiEquipment inst) of
                                        Nothing → (um, Nothing)
                                        Just it →
                                            let inst' = inst
                                                  { uiEquipment =
                                                      HM.delete slotId
                                                        (uiEquipment inst) }
                                            in ( um { umInstances =
                                                        HM.insert uid inst'
                                                          (umInstances um) }
                                               , Just (it, uiGridX inst,
                                                           uiGridY inst) )
                    case mDrop of
                        Nothing → Lua.pushboolean False >> return 1
                        Just (it, gx, gy) → do
                            _ ← Lua.liftIO $
                                atomicModifyIORef' (wsGroundItemsRef ws) $
                                    spawnGroundItem it gx gy
                            Lua.pushboolean True
                            return 1
        _ → Lua.pushboolean False >> return 1

-- | unit.dropItemToGround(uid, defName) → bool. Removes the FIRST
--   inventory instance with the matching defName and lays it on the
--   ground at the unit's tile, PRESERVING the exact ItemInstance
--   (condition / sharpness / quality / fill) — the inventory inverse
--   of item.pickupGround, and the inventory sibling of
--   unit.dropEquipmentToGround. First-match-by-def semantics, same as
--   unit.removeItem; when the EXACT instance matters (the craft AI's
--   output deposit) use unit.dropItemById instead. Returns false if
--   the unit is gone, carries no such item, or no world is active
--   (the item stays in the inventory rather than vanishing).
unitDropItemToGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDropItemToGroundFn env = do
    uidArg  ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (uidArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid     = UnitId (fromIntegral n)
                defName = TE.decodeUtf8Lenient nameBS
            -- Resolve the world FIRST so we never strip the item from
            -- the unit when there's nowhere to drop it.
            mWs ← Lua.liftIO $ activeWorldU env
            case mWs of
                Nothing → Lua.pushboolean False >> return 1
                Just ws → do
                    mDrop ← Lua.liftIO $
                        atomicModifyIORef' (unitManagerRef env) $ \um →
                            case HM.lookup uid (umInstances um) of
                                Nothing → (um, Nothing)
                                Just inst →
                                    case popFirstByNameIx defName
                                             (uiInventory inst) of
                                        Nothing → (um, Nothing)
                                        Just (it, _, rest) →
                                            let inst' = inst
                                                  { uiInventory = rest }
                                            in ( um { umInstances =
                                                        HM.insert uid inst'
                                                          (umInstances um) }
                                               , Just (it, uiGridX inst,
                                                           uiGridY inst) )
                    case mDrop of
                        Nothing → Lua.pushboolean False >> return 1
                        Just (it, gx, gy) → do
                            _ ← Lua.liftIO $
                                atomicModifyIORef' (wsGroundItemsRef ws) $
                                    spawnGroundItem it gx gy
                            Lua.pushboolean True
                            return 1
        _ → Lua.pushboolean False >> return 1

-- | unit.dropItemById(uid, instanceId) → bool. Like dropItemToGround
--   but targets ONE exact ItemInstance by its instance id instead of
--   first-match-by-def. The craft AI (#329) deposits the ids
--   craft.executeAt returns with this, so a same-def item already in
--   the crafter's inventory is never dropped in place of the fresh
--   output (whose quality / condition / temp belong to THIS craft).
unitDropItemByIdFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDropItemByIdFn env = do
    uidArg ← Lua.tointeger 1
    iidArg ← Lua.tointeger 2
    case (uidArg, iidArg) of
        (Just n, Just i) → do
            let uid = UnitId (fromIntegral n)
                iid = fromIntegral i ∷ Word64
            -- Resolve the world FIRST so we never strip the item from
            -- the unit when there's nowhere to drop it.
            mWs ← Lua.liftIO $ activeWorldU env
            case mWs of
                Nothing → Lua.pushboolean False >> return 1
                Just ws → do
                    mDrop ← Lua.liftIO $
                        atomicModifyIORef' (unitManagerRef env) $ \um →
                            case HM.lookup uid (umInstances um) of
                                Nothing → (um, Nothing)
                                Just inst →
                                    case popFirstById iid
                                             (uiInventory inst) of
                                        Nothing → (um, Nothing)
                                        Just (it, rest) →
                                            let inst' = inst
                                                  { uiInventory = rest }
                                            in ( um { umInstances =
                                                        HM.insert uid inst'
                                                          (umInstances um) }
                                               , Just (it, uiGridX inst,
                                                           uiGridY inst) )
                    case mDrop of
                        Nothing → Lua.pushboolean False >> return 1
                        Just (it, gx, gy) → do
                            _ ← Lua.liftIO $
                                atomicModifyIORef' (wsGroundItemsRef ws) $
                                    spawnGroundItem it gx gy
                            Lua.pushboolean True
                            return 1
        _ → Lua.pushboolean False >> return 1

-- | Pop the top-level inventory instance with the given instance id.
popFirstById ∷ Word64 → [ItemInstance] → Maybe (ItemInstance, [ItemInstance])
popFirstById _ [] = Nothing
popFirstById iid (x:xs)
    | iiInstanceId x ≡ iid = Just (x, xs)
    | otherwise            = (\(it, rest) → (it, x : rest))
                             <$> popFirstById iid xs

-- | unit.getItemContents(uid, defName[, instanceId]) → array of { defName,
--   displayName, count, fill, condition, weight, ... }, GROUPED by item type
--   (10 bandages → one entry with count=10), for the targeted item-container
--   (the exact instance when an id is given, else the FIRST inventory item
--   matching `defName`). `weight` is each item's TRUE per-instance mass
--   (empty case + fill + nested contents), so a filled bottle reports its
--   real weight, not the empty-bottle def weight. Empty table if it holds
--   nothing; nil if the unit or that item isn't found.
unitGetItemContentsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetItemContentsFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    -- Optional 3rd arg: target a specific container instance by id (#67),
    -- so two same-def kits don't show each other's contents. Absent/0 →
    -- first inventory item matching defName (AI / tooltip callers).
    instArg ← Lua.tointeger 3
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid    = UnitId (fromIntegral n)
                want   = TE.decodeUtf8Lenient nameBS
                wantId = maybe 0 fromIntegral instArg
            mRes ← Lua.liftIO $ do
                um      ← readIORef (unitManagerRef env)
                itemMgr ← readIORef (itemManagerRef env)
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing → Nothing
                    Just inst →
                        case [ i | i ← uiInventory inst, itemMatches wantId want i ] of
                            (kit : _) → Just (iiContents kit, itemMgr)
                            []        → Nothing
            case mRes of
                Nothing → Lua.pushnil >> return 1
                Just (contents, itemMgr) → do
                    -- Group identical contents by defName. A kit holds at
                    -- most a handful of types, so a flat defName grouping
                    -- (rather than the cargo panel's defName+quality+cond
                    -- key) reads fine — tools are count 1, consumables have
                    -- no condition spread.
                    -- Carry each item's true per-instance mass (empty case +
                    -- fill + nested contents, via itemTotalWeight) into the
                    -- group, NOT the static def weight: a filled antiseptic /
                    -- antibiotics bottle weighs its contents, so the Contents
                    -- panel must not show the empty-bottle weight. Items in a
                    -- defName group are identical (consumables are fill 0,
                    -- bottles are count 1), so keeping the representative's
                    -- per-item weight is exact and the panel's weight×count
                    -- gives the right total.
                    let grouped = HM.toList $ HM.fromListWith
                            (\(c1, f, cond, w) (c2, _, _, _) → (c1 + c2, f, cond, w))
                            [ ( iiDefName i
                              , (1 ∷ Int, iiCurrentFill i, iiCondition i
                                , itemTotalWeight itemMgr i) )
                            | i ← contents ]
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] grouped) $
                      \(idx, (dname, (cnt, fill, cond, wt))) → do
                        let mDef = lookupItemDef dname itemMgr
                            disp = maybe dname idDisplayName mDef
                            cat  = maybe "Misc" idCategory mDef
                            tex  = maybe (-1) (\d → let TextureHandle t =
                                                          idTexture d in t) mDef
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 dname)
                        Lua.setfield (-2) "defName"
                        Lua.pushstring (TE.encodeUtf8 disp)
                        Lua.setfield (-2) "displayName"
                        Lua.pushstring (TE.encodeUtf8 cat)
                        Lua.setfield (-2) "category"
                        Lua.pushinteger (fromIntegral cnt)
                        Lua.setfield (-2) "count"
                        Lua.pushnumber (Lua.Number (realToFrac wt))
                        Lua.setfield (-2) "weight"
                        Lua.pushinteger (fromIntegral tex)
                        Lua.setfield (-2) "iconTex"
                        Lua.pushnumber (Lua.Number (realToFrac fill))
                        Lua.setfield (-2) "fill"
                        Lua.pushnumber (Lua.Number (realToFrac cond))
                        Lua.setfield (-2) "condition"
                        Lua.rawseti (-2) (fromIntegral idx)
                    return 1
        _ → Lua.pushnil >> return 1

-- | unit.getInventory(uid) → array of item tables, or nil if the unit
--   doesn't exist. Each table has:
--     defName        — ItemDef key (e.g. "canteen_steel_2l")
--     displayName    — UI-facing name from YAML
--     weight         — empty kg
--     currentFill    — litres held (0 for non-containers)
--     capacity       — litres max (nil for non-containers)
--     holds          — fluid kind (nil for non-containers)
unitGetInventoryFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetInventoryFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mInv ← Lua.liftIO $ do
                um      ← readIORef (unitManagerRef env)
                itemMgr ← readIORef (itemManagerRef env)
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing   → Nothing
                    Just inst → Just (uiInventory inst, itemMgr)
            case mInv of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just (insts, itemMgr) → do
                    Lua.newtable
                    -- Use 1-based Lua indexing for a real array.
                    forM_ (zip [1 ..] insts) $ \(i, inst) → do
                        Lua.newtable
                        let name = iiDefName inst
                            mDef = lookupItemDef name itemMgr
                            baseName = case mDef of
                                Just d  → idDisplayName d
                                Nothing → name
                            -- Condition 0 ⇒ broken (Combat.Resolution
                            -- weapon wear); tag the name everywhere it shows.
                            broken = iiCondition inst ≤ 0
                            displayName = if broken
                                          then baseName <> " (broken)"
                                          else baseName
                            -- True carried mass of THIS instance — empty
                            -- case + fill + nested contents — so a stocked
                            -- kit / filled canteen shows its real weight and
                            -- the inventory footer matches getCarryingWeight,
                            -- and two kits whose contents diverged read as
                            -- different weights (#67A).
                            weight = itemTotalWeight itemMgr inst
                            mContainer = mDef >>= idContainer
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.setfield (-2) "defName"
                        -- Process-unique identity so right-click actions
                        -- (equip / store / contents) target THIS instance,
                        -- not the first inventory match by defName (#67).
                        Lua.pushinteger (fromIntegral (iiInstanceId inst))
                        Lua.setfield (-2) "instanceId"
                        Lua.pushstring (TE.encodeUtf8 displayName)
                        Lua.setfield (-2) "displayName"
                        Lua.pushboolean broken
                        Lua.setfield (-2) "broken"
                        Lua.pushnumber (Lua.Number (realToFrac (iiSharpness inst)))
                        Lua.setfield (-2) "sharpness"
                        Lua.pushnumber (Lua.Number (realToFrac weight))
                        Lua.setfield (-2) "weight"
                        Lua.pushnumber (Lua.Number (realToFrac (iiCurrentFill inst)))
                        Lua.setfield (-2) "currentFill"
                        -- Tracked temperature (°C) — present only while
                        -- the item is hotter/colder than its
                        -- surroundings (#344); absent = at ambient
                        -- (unit.getItemTemp gives the effective value).
                        case iiTemp inst of
                            Just t → do
                                Lua.pushnumber (Lua.Number (realToFrac t))
                                Lua.setfield (-2) "temp"
                            Nothing → pure ()
                        -- Signature of nested contents so item-containers
                        -- (kits) split by internal state in the row key (#67A).
                        Lua.pushstring (TE.encodeUtf8 (itemContentsSig inst))
                        Lua.setfield (-2) "contentsKey"
                        -- Only surface quality / condition when the def
                        -- actually declares a spec for them — otherwise
                        -- callers (e.g. inventory tooltip) would show
                        -- "100%" for items like canteens / rations that
                        -- conceptually don't have these qualities.
                        case mDef of
                            Just d | Just _ ← idQualitySpec d → do
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iiQuality inst)))
                                Lua.setfield (-2) "quality"
                                -- Named tier (#345), e.g. "excellent" at
                                -- 95% — the tooltip suffix / name reader.
                                case qualityTierLabel d (iiQuality inst) of
                                    Just tier → do
                                        Lua.pushstring (TE.encodeUtf8 tier)
                                        Lua.setfield (-2) "qualityTier"
                                    Nothing → pure ()
                            _ → pure ()
                        case mDef >>= idConditionSpec of
                            Just _ → do
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iiCondition inst)))
                                Lua.setfield (-2) "condition"
                            Nothing → pure ()
                        -- Display-side fields the inventory UI needs.
                        -- Defaulted when the def is missing so the
                        -- renderer always sees a complete row.
                        case mDef of
                            Just d → do
                                Lua.pushstring (TE.encodeUtf8 (idKind d))
                                Lua.setfield (-2) "kind"
                                Lua.pushstring (TE.encodeUtf8 (idCategory d))
                                Lua.setfield (-2) "category"
                                Lua.pushstring (TE.encodeUtf8 (idMake d))
                                Lua.setfield (-2) "make"
                                Lua.pushstring (TE.encodeUtf8 (idMaterial d))
                                Lua.setfield (-2) "material"
                                Lua.pushboolean (idUnequippable d)
                                Lua.setfield (-2) "unequippable"
                                unless (null (idBuffs d)) $ do
                                    Lua.newtable
                                    forM_ (zip [1 ∷ Int ..] (idBuffs d))
                                        $ \(j, b) → do
                                            Lua.newtable
                                            Lua.pushstring
                                                (TE.encodeUtf8 (ibStat b))
                                            Lua.setfield (-2) "stat"
                                            Lua.pushnumber (Lua.Number
                                                (realToFrac (ibAmount b)))
                                            Lua.setfield (-2) "amount"
                                            Lua.pushnumber (Lua.Number
                                                (realToFrac (ibPercent b)))
                                            Lua.setfield (-2) "percent"
                                            Lua.pushboolean
                                                (ibScalesWithCondition b)
                                            Lua.setfield (-2)
                                                "scalesWithCondition"
                                            Lua.rawseti (-2) (fromIntegral j)
                                    Lua.setfield (-2) "buffs"
                                let TextureHandle tex = idTexture d
                                Lua.pushinteger (fromIntegral tex)
                                Lua.setfield (-2) "iconTex"
                            Nothing → do
                                Lua.pushstring "misc"
                                Lua.setfield (-2) "kind"
                                Lua.pushstring "Misc"
                                Lua.setfield (-2) "category"
                        case mContainer of
                            Just c → do
                                Lua.pushnumber (Lua.Number (realToFrac (icCapacity c)))
                                Lua.setfield (-2) "capacity"
                                Lua.pushstring (TE.encodeUtf8 (icHolds c))
                                Lua.setfield (-2) "holds"
                            Nothing → pure ()
                        case mDef >>= idFood of
                            Just f → do
                                Lua.newtable                -- food
                                Lua.newtable                -- food.nutrition
                                Lua.pushnumber (Lua.Number (realToFrac (ifCalories f)))
                                Lua.setfield (-2) "calories"
                                Lua.pushnumber (Lua.Number (realToFrac (ifCaloriesPerKg f)))
                                Lua.setfield (-2) "caloriesPerKg"
                                Lua.setfield (-2) "nutrition"
                                Lua.setfield (-2) "food"
                            Nothing → pure ()
                        case mDef >>= idWeapon of
                            Just w → do
                                Lua.newtable
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iwBladeLength w)))
                                Lua.setfield (-2) "bladeLength"
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iwBaseSharpness w)))
                                Lua.setfield (-2) "baseSharpness"
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iwStabEff w)))
                                Lua.setfield (-2) "stabEffectiveness"
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iwSlashEff w)))
                                Lua.setfield (-2) "slashEffectiveness"
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iwBluntEff w)))
                                Lua.setfield (-2) "bluntEffectiveness"
                                Lua.setfield (-2) "weapon"
                            Nothing → pure ()
                        Lua.rawseti (-2) (fromIntegral (i ∷ Int))
                    return 1
