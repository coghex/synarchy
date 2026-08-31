{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Units.Cargo
  ( unitTransferItemToBuildingFn
  , unitTransferItemToUnitFn
  , unitDepositToCargoFn
  , unitWithdrawFromCargoFn
  , unitGetCarryingWeightFn
  )
    where

import UPrelude
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Building.Knowledge.Live
    (ContainerObserver, containerObserver, revealContainerForUnit)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv, loggerRef)
import Engine.Core.Log (LogCategory(..), logWarn)
import Unit.Types
import Building.Types (BuildingId(..), BuildingInstance(..), BuildingDef(..), BuildingManager(..))
import Item.Types (itemMatches, itemTotalWeight)
import Engine.Scripting.Lua.API.Units.Inventory (popFirstByNameIx, insertAt, popFirstWhereIx)
import World.Page.Types (WorldPageId)

-- | #1087: the narrow live view a container-knowledge reveal needs,
--   projected from this module's existing capability records.
observerFor ∷ EngineEnv → ContainerObserver
observerFor env = containerObserver
    (toBuildingCapability env) (toWorldSimCapability env)
    (toContentRegistriesViewCapability env)

-- | #1673: the page floor these four verbs enforce. Laxity here has
--   always meant "no adjacency check, no receiver-eligibility check" —
--   it never meant permission to move an exact 'Item.Types.ItemInstance'
--   between two WORLDS. 'Unit.Transfer.reachable' requires identical
--   pages on the strict surface even where adjacency is deliberately
--   deferred, and #1205 / #1208 / #1593 put every other engine verb on
--   the entity's OWN page rather than the active one.
--
--   True when the counterpart is KNOWN to sit on another page. A
--   'Nothing' counterpart is a vanished endpoint, not a cross-page
--   one: those already have their own all-or-nothing rollback path
--   below, and answering "cross-page" for them would change what a
--   same-page caller sees when its destination dies mid-call.
crossPage ∷ Maybe WorldPageId → WorldPageId → Bool
crossPage (Just there) here = there ≢ here
crossPage Nothing      _    = False

-- | The page a live building sits on, or 'Nothing' if it is gone.
buildingPage ∷ EngineEnv → BuildingId → IO (Maybe WorldPageId)
buildingPage env bid = do
    bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
    pure (biPage <$> HM.lookup bid (bmInstances bm))

-- | The page a live unit stands on, or 'Nothing' if it is gone.
unitPage ∷ EngineEnv → UnitId → IO (Maybe WorldPageId)
unitPage env uid = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    pure (uiPage <$> HM.lookup uid (umInstances um))

-- | unit.transferItemToBuilding(uid, bid, defName) → bool. Atomic
--   move of one ItemInstance from the unit's inventory to the
--   building's biMaterialsDelivered list for that defName. Preserves
--   quality / condition / currentFill on the instance so a delivered
--   electric motor at 100% comes back out at 100% (or its degraded
--   state) on a future deconstruction.
--
--   Returns true on success. If the unit lacks a matching item OR
--   the building has vanished between pop and deliver, returns false
--   and logs a warning (the latter case is a tiny race window — the
--   AI just queried the building one tick prior).
--
--   #1673: also returns false, mutating nothing, when the unit and the
--   building sit on different world pages. See 'crossPage'.
unitTransferItemToBuildingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTransferItemToBuildingFn env = do
    uidArg  ← Lua.tointeger 1
    bidArg  ← Lua.tointeger 2
    nameArg ← Lua.tostring 3
    case (uidArg, bidArg, nameArg) of
        (Just nU, Just nB, Just nameBS) → do
            let uid     = UnitId (fromIntegral nU)
                bid     = BuildingId (fromIntegral nB)
                defName = TE.decodeUtf8Lenient nameBS
            -- #1673: refuse a cross-page pair BEFORE the pop, inside the
            -- same transaction that would perform it, so nothing is
            -- popped, spliced or duplicated on the way out.
            mBPage ← Lua.liftIO $ buildingPage env bid
            mItem ← Lua.liftIO $ atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, Nothing)
                    Just u | crossPage mBPage (uiPage u) → (um, Nothing)
                    Just u →
                        case popFirstByNameIx defName (uiInventory u) of
                            Nothing → (um, Nothing)
                            Just (item, ix, newInv) →
                                let u' = u { uiInventory = newInv }
                                in (um { umInstances = HM.insert uid u'
                                                                (umInstances um) }
                                   , Just (item, ix))
            case mItem of
                Nothing → do
                    Lua.pushboolean False
                    return 1
                Just (item, ix) → do
                    delivered ← Lua.liftIO $
                        atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
                            case HM.lookup bid (bmInstances bm) of
                                Nothing → (bm, False)
                                Just inst →
                                    let current = HM.lookupDefault [] defName
                                                    (biMaterialsDelivered inst)
                                        newMap  = HM.insert defName (item : current)
                                                    (biMaterialsDelivered inst)
                                        inst'   = inst
                                            { biMaterialsDelivered = newMap }
                                    in (bm { bmInstances = HM.insert bid inst'
                                                            (bmInstances bm) }
                                       , True)
                    unless delivered $ do
                        -- Destination vanished between pop and deliver:
                        -- splice the popped instance back into the
                        -- unit's inventory at its ORIGINAL index so the
                        -- move stays all-or-nothing — list order is
                        -- gameplay/UI-visible, so a rollback must leave
                        -- the source unchanged, not move the item.
                        restored ← Lua.liftIO $
                            atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                                case HM.lookup uid (umInstances um) of
                                    Nothing → (um, False)
                                    Just u →
                                        let u' = u { uiInventory =
                                                       insertAt ix item
                                                         (uiInventory u) }
                                        in (um { umInstances = HM.insert uid u'
                                                                (umInstances um) }
                                           , True)
                        unless restored $ do
                            logger ← Lua.liftIO $ readIORef (loggerRef env)
                            Lua.liftIO $ logWarn logger CatThread $
                                "transferItemToBuilding: building "
                                <> tshow nB
                                <> " gone between pop and deliver and unit "
                                <> tshow nU
                                <> " also vanished — "
                                <> defName <> " lost"
                    Lua.pushboolean delivered
                    return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.transferItemToUnit(fromUid, toUid, defName[, instanceId]) →
--   bool. Atomic move of one ItemInstance from one unit's inventory to
--   another's. Both units live in the same manager ref, so the pop and
--   the push happen in a single atomicModifyIORef' — the item can
--   never be duplicated or dropped by a thread interleaving. Quality /
--   condition / currentFill are preserved exactly (this is how
--   acolytes pull build materials off the technomule without
--   re-rolling them). No capacity check here — the Lua caller gates
--   on carrying capacity the same way pickup does.
--   Optional 4th arg: transfer the EXACT source instance (itemMatches,
--   same convention as depositToCargo/equip/equipAccessory) — without
--   it, a caller pulling back a specific instance it fetched earlier
--   (#302's repair AI returning spare gear to the technomule) could
--   silently pop a DIFFERENT same-defName item the source happens to
--   also be carrying. Absent/0 → first defName match (legacy/AI callers
--   moving fungible materials, where any matching instance will do).
--   Returns false if either unit is missing or the source lacks a
--   matching item; the transfer is all-or-nothing. #1673: false too
--   when the two units stand on different world pages ('crossPage').
unitTransferItemToUnitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTransferItemToUnitFn env = do
    fromArg ← Lua.tointeger 1
    toArg   ← Lua.tointeger 2
    nameArg ← Lua.tostring 3
    instArg ← Lua.tointeger 4
    case (fromArg, toArg, nameArg) of
        (Just nF, Just nT, Just nameBS) | nF ≢ nT → do
            let fromUid = UnitId (fromIntegral nF)
                toUid   = UnitId (fromIntegral nT)
                defName = TE.decodeUtf8Lenient nameBS
                wantId  = maybe 0 fromIntegral instArg
            ok ← Lua.liftIO $ atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                case (HM.lookup fromUid (umInstances um),
                      HM.lookup toUid   (umInstances um)) of
                    -- #1673: both units live in this one manager, so the
                    -- page comparison happens INSIDE the same atomic
                    -- modify as the move — a cross-page pair can never
                    -- observe a half-applied state.
                    (Just uF, Just uT) | uiPage uF ≢ uiPage uT → (um, False)
                    (Just uF, Just uT) →
                        case popFirstWhereIx (itemMatches wantId defName)
                                             (uiInventory uF) of
                            Nothing → (um, False)
                            Just (item, _, newInv) →
                                let uF' = uF { uiInventory = newInv }
                                    uT' = uT { uiInventory =
                                                 uiInventory uT ++ [item] }
                                    insts = HM.insert toUid uT'
                                          $ HM.insert fromUid uF'
                                          $ umInstances um
                                in (um { umInstances = insts }, True)
                    _ → (um, False)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.depositToCargo(uid, bid, defName) → bool. Moves one matching
--   ItemInstance from the unit's loose inventory into the building's
--   biStorage. Capacity-checked (rejects if the new total weight
--   would exceed bdStorageCapacity). Quality / condition / fill on
--   the instance are preserved exactly. No adjacency check — that
--   lives in the Lua caller (AI walks the unit close first; the
--   right-click menu only enables when the unit is adjacent).
--
--   #1673: refuses, mutating nothing, when the unit and the building
--   are on different world pages ('crossPage'). Adjacency stays the
--   caller's business; the PAGE never was latitude.
--
--   #1087: a SUCCESSFUL deposit by a player-commandable unit also
--   reveals the container's contents to the player, so the AI's own
--   hauling keeps the remembered view current. A refusal (no room, no
--   matching item, a cross-page pair) or a rolled-back deposit reveals
--   nothing — the call below is reached only on the committed path.
unitDepositToCargoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDepositToCargoFn env = do
    uidArg  ← Lua.tointeger 1
    bidArg  ← Lua.tointeger 2
    nameArg ← Lua.tostring 3
    -- Optional 4th arg: deposit the EXACT inventory instance (#67), so a
    -- merged "Store" row stores the canteen the player sees, not the
    -- first defName match. Absent/0 → first match (AI auto-store).
    instArg ← Lua.tointeger 4
    case (uidArg, bidArg, nameArg) of
        (Just nU, Just nB, Just nameBS) → do
            let uid     = UnitId (fromIntegral nU)
                bid     = BuildingId (fromIntegral nB)
                defName = TE.decodeUtf8Lenient nameBS
                wantId  = maybe 0 fromIntegral instArg
            -- Capacity pre-check: read-only snapshot. Weighs the ACTUAL
            -- ItemInstance that will be popped below (the first match in
            -- the unit's inventory) via itemTotalWeight, so fill and
            -- nested contents are counted — the same recursive measure
            -- used for the items already in storage. A filled container
            -- or stocked kit can be kilograms heavier than its def mean,
            -- so checking idWeight here would let it overfill the cargo.
            okFits ← Lua.liftIO $ do
                bm      ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
                itemMgr ← readReadOnlyRef
                    (crvItemManagerRef (toContentRegistriesViewCapability env))
                um      ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
                pure $ fromMaybe False $ do
                    inst         ← HM.lookup bid (bmInstances bm)
                    def          ← HM.lookup (biDefName inst) (bmDefs bm)
                    u            ← HM.lookup uid (umInstances um)
                    (item, _, _) ← popFirstWhereIx (itemMatches wantId defName)
                                                   (uiInventory u)
                    let cap     = bdStorageCapacity def
                        current = sum (map (itemTotalWeight itemMgr)
                                           (biStorage inst))
                    pure (cap > 0 ∧ current + itemTotalWeight itemMgr item <= cap)
            if not okFits then do
                Lua.pushboolean False
                return 1
            else do
                -- #1673: as in transferItemToBuilding, the refusal is the
                -- pop's own guard. Answering Nothing here returns false
                -- without mutating either side AND without reaching the
                -- #1087 container reveal below, which is committed-path
                -- only.
                mBPage ← Lua.liftIO $ buildingPage env bid
                mItem ← Lua.liftIO $ atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, Nothing)
                        Just u | crossPage mBPage (uiPage u) → (um, Nothing)
                        Just u →
                            case popFirstWhereIx (itemMatches wantId defName)
                                                 (uiInventory u) of
                                Nothing → (um, Nothing)
                                Just (item, ix, newInv) →
                                    let u' = u { uiInventory = newInv }
                                    in (um { umInstances = HM.insert uid u'
                                                            (umInstances um) }
                                       , Just (item, ix))
                case mItem of
                    Nothing → do
                        Lua.pushboolean False
                        return 1
                    Just (item, ix) → do
                        ok ← Lua.liftIO $
                            atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
                                case HM.lookup bid (bmInstances bm) of
                                    Nothing → (bm, False)
                                    Just inst →
                                        let inst' = inst
                                                { biStorage = item : biStorage inst }
                                        in (bm { bmInstances =
                                                    HM.insert bid inst'
                                                        (bmInstances bm) }
                                           , True)
                        unless ok $ do
                            -- Destination building vanished between pop
                            -- and deposit: splice the popped instance back
                            -- into the unit's inventory at its ORIGINAL
                            -- index (all-or-nothing; order is visible).
                            restored ← Lua.liftIO $
                                atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                                    case HM.lookup uid (umInstances um) of
                                        Nothing → (um, False)
                                        Just u →
                                            let u' = u { uiInventory =
                                                           insertAt ix item
                                                             (uiInventory u) }
                                            in (um { umInstances =
                                                       HM.insert uid u'
                                                         (umInstances um) }
                                               , True)
                            unless restored $ do
                                logger ← Lua.liftIO $ readIORef (loggerRef env)
                                Lua.liftIO $ logWarn logger CatThread $
                                    "depositToCargo: building "
                                    <> tshow nB
                                    <> " gone between pop and deposit and unit "
                                    <> tshow nU
                                    <> " also vanished — "
                                    <> defName <> " lost"
                        when ok $ Lua.liftIO $ void $
                            revealContainerForUnit (observerFor env)
                                (ucUnitManagerRef (toUnitCombatCapability env))
                                uid bid
                        Lua.pushboolean ok
                        return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.withdrawFromCargo(uid, bid, defName) → bool. Reverse of
--   depositToCargo: pops one matching ItemInstance from biStorage,
--   appends to the unit's loose inventory. Not gated by unit
--   carrying-capacity — units can hold above their cap (with stat
--   penalties handled elsewhere). Adjacency check lives in the Lua
--   caller, same as deposit.
--
--   #1673: refuses, mutating nothing and revealing nothing, when the
--   unit and the building are on different world pages ('crossPage').
--
--   #1087: like deposit, a SUCCESSFUL withdrawal by a player-commandable
--   unit reveals the container's remaining contents.
unitWithdrawFromCargoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitWithdrawFromCargoFn env = do
    uidArg  ← Lua.tointeger 1
    bidArg  ← Lua.tointeger 2
    nameArg ← Lua.tostring 3
    -- Optional 4th arg: withdraw the EXACT stored instance (#67) the
    -- player clicked in the cargo panel, not the first defName match.
    -- The id targets an item in the BUILDING's storage (exposed by
    -- building.getStorage). Absent/0 → first match.
    instArg ← Lua.tointeger 4
    case (uidArg, bidArg, nameArg) of
        (Just nU, Just nB, Just nameBS) → do
            let uid     = UnitId (fromIntegral nU)
                bid     = BuildingId (fromIntegral nB)
                defName = TE.decodeUtf8Lenient nameBS
                wantId  = maybe 0 fromIntegral instArg
            -- #1673: the mirror of deposit — here the BUILDING is popped
            -- first, so the unit's page is what gets captured and the
            -- guard rides the building transaction. No pop, no reveal.
            mUPage ← Lua.liftIO $ unitPage env uid
            mItem ← Lua.liftIO $ atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
                case HM.lookup bid (bmInstances bm) of
                    Nothing → (bm, Nothing)
                    Just inst | crossPage mUPage (biPage inst) → (bm, Nothing)
                    Just inst →
                        case popFirstWhereIx (itemMatches wantId defName)
                                             (biStorage inst) of
                            Nothing → (bm, Nothing)
                            Just (item, ix, newStorage) →
                                let inst' = inst { biStorage = newStorage }
                                in (bm { bmInstances = HM.insert bid inst'
                                                          (bmInstances bm) }
                                   , Just (item, ix))
            case mItem of
                Nothing → do
                    Lua.pushboolean False
                    return 1
                Just (item, ix) → do
                    ok ← Lua.liftIO $
                        atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                            case HM.lookup uid (umInstances um) of
                                Nothing → (um, False)
                                Just u →
                                    let u' = u
                                            { uiInventory = uiInventory u ++ [item] }
                                    in (um { umInstances = HM.insert uid u'
                                                            (umInstances um) }
                                       , True)
                    unless ok $ do
                        -- Destination unit vanished between pop and
                        -- append: splice the popped instance back into
                        -- the building's storage at its ORIGINAL index
                        -- (all-or-nothing; storage order is visible).
                        restored ← Lua.liftIO $
                            atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
                                case HM.lookup bid (bmInstances bm) of
                                    Nothing → (bm, False)
                                    Just inst →
                                        let inst' = inst
                                                { biStorage = insertAt ix item
                                                                (biStorage inst) }
                                        in (bm { bmInstances = HM.insert bid inst'
                                                                  (bmInstances bm) }
                                           , True)
                        unless restored $ do
                            logger ← Lua.liftIO $ readIORef (loggerRef env)
                            Lua.liftIO $ logWarn logger CatThread $
                                "withdrawFromCargo: unit "
                                <> tshow nU
                                <> " gone between pop and append and building "
                                <> tshow nB
                                <> " also vanished — "
                                <> defName <> " lost"
                    when ok $ Lua.liftIO $ void $
                        revealContainerForUnit (observerFor env)
                                (ucUnitManagerRef (toUnitCombatCapability env))
                                uid bid
                    Lua.pushboolean ok
                    return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.getCarryingWeight(uid) → Float kg. Sum of full item weight
--   via 'itemTotalWeight' (instance weight + container fill at the
--   container's per-unit fill weight + nested container contents)
--   across loose inventory + equipped slot items + accessories. Worn
--   gear counts the same as carried gear by design: it's the same mass.
--   Used by the auto-store AI utility (fill_fraction = this / cap) and
--   the pickup/fetch capacity gates.
unitGetCarryingWeightFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetCarryingWeightFn env = do
    uidArg ← Lua.tointeger 1
    case uidArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mW ← Lua.liftIO $ do
                um      ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
                itemMgr ← readReadOnlyRef
                    (crvItemManagerRef (toContentRegistriesViewCapability env))
                let weightOf = itemTotalWeight itemMgr
                pure $ do
                    u ← HM.lookup uid (umInstances um)
                    let invW = sum (map weightOf (uiInventory u))
                        eqW  = sum (map weightOf (HM.elems (uiEquipment u)))
                        accW = sum (map weightOf (uiAccessories u))
                    pure (invW + eqW + accW ∷ Float)
            case mW of
                Just w → do
                    Lua.pushnumber (Lua.Number (realToFrac w))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
