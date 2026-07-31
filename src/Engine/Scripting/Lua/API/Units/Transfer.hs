{-# LANGUAGE Strict #-}
-- | Lua surface for the player-managed transfer contract (#1000).
--   Four verbs, all routed through the pure policy in
--   "Unit.Transfer" so B1's context menu, C1's paired inventory panel
--   and C2's walk-then-commit share one eligibility, proximity and
--   capacity rule instead of three:
--
--   * @unit.checkTransfer@   — create-time validation (no mutation)
--   * @unit.commitTransfer@  — revalidate and move, atomically
--   * @unit.transferContract@ — the engine's identifier vocabulary
--   * @unit.transferReceiverInfo@ — read-only receiver eligibility (#1014)
--
--   Deliberately separate from "Engine.Scripting.Lua.API.Units.Cargo":
--   those verbs stay exactly as they are (no capacity check for
--   unit-to-unit, no adjacency check at all) because the AI fetch,
--   repair and medic ladders depend on that laxity.
module Engine.Scripting.Lua.API.Units.Transfer
  ( unitCheckTransferFn
  , unitCommitTransferFn
  , unitTransferContractFn
  , unitTransferReceiverInfoFn
  )
    where

import UPrelude
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv)
import Unit.Types
import Unit.Stats (effectiveStat)
import Unit.Transfer
import Building.Types
    (BuildingId(..), BuildingActivity(..), BuildingDef(..)
    , BuildingInstance(..), BuildingManager(..), currentActivity
    , footprintDistAt)
import Item.Types (ItemInstance(..), ItemManager, itemTotalWeight)
import World.Page.Types (WorldPageId(..))
import Engine.Scripting.Lua.API.Units.Inventory (insertAt)
import Engine.Scripting.Lua.API.Units.List (prettifyDefName)

-- | Everything read out of the live refs in one pass, so a scene is
--   built from a single consistent view rather than five interleaved
--   reads.
data LiveState = LiveState
    { lsUnits     ∷ !UnitManager
    , lsBuildings ∷ !BuildingManager
    , lsItems     ∷ !ItemManager
    , lsNow       ∷ !Double
    }

readLiveState ∷ EngineEnv → IO LiveState
readLiveState env = LiveState
    ⊚ readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    ⊛ readIORef (bcBuildingManagerRef (toBuildingCapability env))
    ⊛ readIORef (crItemManagerRef (toContentRegistriesCapability env))
    ⊛ readIORef (wsGameTimeRef (toWorldSimCapability env))

-- | Project the source unit. Equipment slots and accessories are
--   carried alongside the loose inventory so the policy can answer
--   "worn, not transferable" instead of "no such item".
sourceView ∷ UnitManager → UnitId → Maybe TransferSourceView
sourceView um uid = do
    u ← HM.lookup uid (umInstances um)
    pure TransferSourceView
        { tsvPage      = uiPage u
        , tsvTile      = (floor (uiGridX u), floor (uiGridY u))
        , tsvInventory = uiInventory u
        , tsvEquipped  = HM.elems (uiEquipment u) ++ uiAccessories u
        }

buildingReceiver ∷ LiveState → BuildingId → Maybe TransferReceiverView
buildingReceiver ls bid = do
    inst ← HM.lookup bid (bmInstances (lsBuildings ls))
    def  ← HM.lookup (biDefName inst) (bmDefs (lsBuildings ls))
    pure $ BuildingReceiverAt BuildingReceiverView
        { brvPage         = biPage inst
        , brvAnchor       = (biAnchorX inst, biAnchorY inst)
        , brvTileSize     = (biTileW inst, biTileH inst)
        , brvBuilt        = currentActivity (lsNow ls) inst def ≡ Built
        , brvCapacity     = bdStorageCapacity def
        , brvStorage      = biStorage inst
        , brvStoredWeight =
            sum (map (itemTotalWeight (lsItems ls)) (biStorage inst))
        }

-- | Project a unit receiver. Capacity is the modifier-applied
--   @carrying_capacity@ stat against the getCarryingWeight measure
--   (inventory + equipment + accessories at full recursive weight) —
--   the same pair the pickup and fetch gates already use. A unit with
--   no such stat projects capacity 0, which the policy reads as no
--   room.
unitReceiver ∷ LiveState → UnitId → Maybe TransferReceiverView
unitReceiver ls uid = do
    u ← HM.lookup uid (umInstances (lsUnits ls))
    let weighOne = itemTotalWeight (lsItems ls)
        marker   = maybe False udTransferReceiver
                     (HM.lookup (uiDefName u) (umDefs (lsUnits ls)))
        capBase  = HM.lookupDefault 0 "carrying_capacity" (uiStats u)
        capMods  = HM.lookupDefault [] "carrying_capacity" (uiModifiers u)
    pure $ UnitReceiverAt UnitReceiverView
        { urvPage          = uiPage u
        , urvTile          = (floor (uiGridX u), floor (uiGridY u))
        , urvIsReceiver    = marker
        , urvCapacity      = effectiveStat (lsNow ls) capBase capMods
        , urvInventory     = uiInventory u
        , urvCarriedWeight = sum (map weighOne (uiInventory u))
                           + sum (map weighOne (HM.elems (uiEquipment u)))
                           + sum (map weighOne (uiAccessories u))
        }

sceneFor ∷ LiveState → TransferRequest → TransferScene
sceneFor ls req = TransferScene
    { tscSource   = sourceView (lsUnits ls) (trSource req)
    , tscReceiver = case trReceiver req of
        ReceiverBuilding bid → buildingReceiver ls bid
        ReceiverUnit     uid → unitReceiver ls uid
    , tscWeigh    = itemTotalWeight (lsItems ls)
    }

-- | Parse the five/six shared arguments:
--   @(sourceUid, receiverKind, receiverId, defName, instanceId[, quantity])@.
--   @receiverKind@ is @"building"@ or @"unit"@; anything else is an
--   argument error, not a transfer refusal.
readRequest ∷ Lua.LuaE Lua.Exception (Maybe TransferRequest)
readRequest = do
    srcArg  ← Lua.tointeger 1
    kindArg ← Lua.tostring 2
    rcvArg  ← Lua.tointeger 3
    nameArg ← Lua.tostring 4
    instArg ← Lua.tointeger 5
    qtyArg  ← Lua.tointeger 6
    pure $ do
        nS   ← srcArg
        kind ← TE.decodeUtf8Lenient ⊚ kindArg
        nR   ← rcvArg
        name ← TE.decodeUtf8Lenient ⊚ nameArg
        (receiver, op) ← case kind of
            "building" → Just ( ReceiverBuilding (BuildingId (fromIntegral nR))
                              , ToBuildingStorage )
            "unit"     → Just ( ReceiverUnit (UnitId (fromIntegral nR))
                              , ToUnitInventory )
            _          → Nothing
        pure TransferRequest
            { trSource     = UnitId (fromIntegral nS)
            , trReceiver   = receiver
            , trInstanceId = maybe 0 fromIntegral instArg
            , trDefName    = name
            , trQuantity   = maybe 1 fromIntegral qtyArg
            , trOperation  = op
            }

-- | Push @{ ok, state, operation[, reason][, cause][, instanceId] }@.
pushResult ∷ TransferRequest → TransferState → Maybe Word64
           → Lua.LuaE Lua.Exception Lua.NumResults
pushResult req st mInst = do
    Lua.newtable
    Lua.pushboolean (case st of
        TransferCompleted → True
        TransferQueued    → True
        _                 → False)
    Lua.setfield (-2) "ok"
    pushTextField "state" (transferStateId st)
    pushTextField "operation" (transferOperationId (trOperation req))
    case st of
        TransferFailed f → do
            pushTextField "reason" (transferReasonId (tfReason f))
            case tfCause f of
                Just c  → pushTextField "cause" (transferReasonId c)
                Nothing → pure ()
        _ → pure ()
    case mInst of
        Just iid → do
            Lua.pushinteger (fromIntegral iid)
            Lua.setfield (-2) "instanceId"
        Nothing → pure ()
    return 1

pushTextField ∷ Lua.Name → Text → Lua.LuaE Lua.Exception ()
pushTextField key val = do
    Lua.pushstring (TE.encodeUtf8 val)
    Lua.setfield (-2) key

-- | Bad arguments are distinct from a refused transfer: nil, not a
--   result table with a reason that would look like policy.
pushArgError ∷ Lua.LuaE Lua.Exception Lua.NumResults
pushArgError = Lua.pushnil >> return 1

-- | @unit.checkTransfer(sourceUid, kind, receiverId, defName,
--   instanceId[, quantity])@ → result table. Pure validation: reports
--   the state a freshly created request would hold (@queued@ or
--   @failed@ with the offending reason) and mutates nothing. The
--   answer is advisory by construction — commitTransfer revalidates.
unitCheckTransferFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCheckTransferFn env = do
    mReq ← readRequest
    case mReq of
        Nothing  → pushArgError
        Just req → do
            ls ← Lua.liftIO $ readLiveState env
            let q = queueTransfer (sceneFor ls req) req
            pushResult req (qtState q) Nothing

-- | @unit.commitTransfer(sourceUid, kind, receiverId, defName,
--   instanceId[, quantity])@ → result table. Revalidates every
--   precondition against freshly read state and, when they all still
--   hold, moves exactly that one instance exactly once. A refusal
--   leaves both inventories untouched — including their ORDER.
unitCommitTransferFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCommitTransferFn env = do
    mReq ← readRequest
    case mReq of
        Nothing  → pushArgError
        Just req → do
            outcome ← Lua.liftIO $ case trReceiver req of
                ReceiverUnit toUid    → commitToUnit env req toUid
                ReceiverBuilding toBid → commitToBuilding env req toBid
            case outcome of
                Left f     → pushResult req (TransferFailed f) Nothing
                Right item → pushResult req TransferCompleted
                                        (Just (iiInstanceId item))

-- | Unit → unit. Both inventories live in the SAME manager ref, so
--   revalidation and both writes happen inside one
--   atomicModifyIORef' — no interleaving can duplicate or drop the
--   instance, and no rollback is needed because there is never a
--   half-applied state.
commitToUnit ∷ EngineEnv → TransferRequest → UnitId
             → IO (Either TransferFailure ItemInstance)
commitToUnit env req toUid = do
    itemMgr ← readIORef (crItemManagerRef (toContentRegistriesCapability env))
    now     ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    bm      ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
    atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
        let ls    = LiveState { lsUnits = um, lsBuildings = bm
                              , lsItems = itemMgr, lsNow = now }
            scene = sceneFor ls req
        in case commitTransfer scene req of
            Left f  → (um, Left f)
            Right c → case (HM.lookup (trSource req) (umInstances um)
                           , HM.lookup toUid (umInstances um)) of
                (Just uF, Just uT) →
                    let uF' = uF { uiInventory = tcSourceInventory c }
                        uT' = uT { uiInventory = tcReceiverItems c }
                        insts = HM.insert toUid uT'
                              $ HM.insert (trSource req) uF'
                              $ umInstances um
                    in (um { umInstances = insts }, Right (tcItem c))
                -- Unreachable: commitTransfer only succeeds when both
                -- projections resolved out of this very map.
                _ → (um, Left (staleFailure ReasonSourceMissing))

-- | Unit → building storage. The two sides live in different refs, so
--   this follows the pop-then-push-then-roll-back shape depositToCargo
--   already uses: validate against a fresh scene, pop under the unit
--   ref, then push under the building ref. A failed push splices the
--   instance back at its ORIGINAL index.
--
--   EVERY precondition is revalidated inside that transaction, each
--   against whichever side owns it: the pop re-checks the exact
--   instance is still at the planned index AND the source's LIVE page
--   and tile, and the push re-checks the receiver's LIVE eligibility,
--   capacity and footprint against the tile the source was actually
--   standing on when the item left its hands. Validating proximity
--   only from the opening snapshot would let a unit that walked away
--   (or was teleported) mid-call still deposit from out of range.
commitToBuilding ∷ EngineEnv → TransferRequest → BuildingId
                 → IO (Either TransferFailure ItemInstance)
commitToBuilding env req bid = do
    ls ← readLiveState env
    let scene = sceneFor ls req
    case commitTransfer scene req of
        Left f  → pure (Left f)
        Right c → do
            let uid = trSource req
                ix  = tcIndex c
            popped ← atomicModifyIORef'
                        (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, Left ReasonSourceMissing)
                    Just u → case drop ix (uiInventory u) of
                        (live:_)
                            | ix ≥ 0
                            , iiInstanceId live ≡ iiInstanceId (tcItem c) →
                                let tile = ( floor (uiGridX u)
                                           , floor (uiGridY u) )
                                    u' = u { uiInventory =
                                               removeIndex ix (uiInventory u) }
                                in if inReachOf (tscReceiver scene)
                                                (uiPage u) tile
                                   then (um { umInstances =
                                                HM.insert uid u' (umInstances um) }
                                        , Right (live, uiPage u, tile))
                                   else (um, Left ReasonOutOfRange)
                        _ → (um, Left ReasonInstanceMissing)
            case popped of
                Left r → pure (Left (staleFailure r))
                Right (item, srcPage, srcTile) → do
                    stored ← atomicModifyIORef'
                        (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
                        case storeInto bm bid item (lsItems ls) (lsNow ls)
                                       srcPage srcTile of
                            Left r    → (bm, Left r)
                            Right bm' → (bm', Right ())
                    case stored of
                        Right () → pure (Right item)
                        Left r   → do
                            -- All-or-nothing: put it back where it was.
                            atomicModifyIORef'
                                (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                                case HM.lookup uid (umInstances um) of
                                    Nothing → (um, ())
                                    Just u →
                                        let u' = u { uiInventory =
                                                       insertAt ix item
                                                         (uiInventory u) }
                                        in (um { umInstances =
                                                   HM.insert uid u'
                                                     (umInstances um) }, ())
                            pure (Left (staleFailure r))

-- | Is @(page, tile)@ within the contract's reach of this receiver?
--   Reads the projection the policy itself measured against, so the
--   two can't disagree about what "adjacent" means.
inReachOf ∷ Maybe TransferReceiverView → WorldPageId → (Int, Int) → Bool
inReachOf (Just (BuildingReceiverAt b)) page tile =
    page ≡ brvPage b ∧ footprintDistAt (brvAnchor b) (brvTileSize b) tile ≤ 1
inReachOf (Just (UnitReceiverAt u)) page (sx, sy) =
    let (ux, uy) = urvTile u
    in page ≡ urvPage u ∧ max (abs (sx - ux)) (abs (sy - uy)) ≤ 1
inReachOf Nothing _ _ = False

-- | Final receiver-side re-check, run under the building ref: the
--   building still exists, is still Built, still has storage, is still
--   in reach of where the source was standing when the item left, and
--   the instance still fits.
storeInto ∷ BuildingManager → BuildingId → ItemInstance → ItemManager
          → Double → WorldPageId → (Int, Int)
          → Either TransferReason BuildingManager
storeInto bm bid item itemMgr now srcPage srcTile = do
    inst ← maybe (Left ReasonReceiverMissing) Right
                 (HM.lookup bid (bmInstances bm))
    def  ← maybe (Left ReasonReceiverMissing) Right
                 (HM.lookup (biDefName inst) (bmDefs bm))
    unless (currentActivity now inst def ≡ Built ∧ bdStorageCapacity def > 0) $
        Left ReasonReceiverIneligible
    unless (biPage inst ≡ srcPage
              ∧ footprintDistAt (biAnchorX inst, biAnchorY inst)
                                (biTileW inst, biTileH inst) srcTile ≤ 1) $
        Left ReasonOutOfRange
    let stored = sum (map (itemTotalWeight itemMgr) (biStorage inst))
    unless (stored + itemTotalWeight itemMgr item ≤ bdStorageCapacity def) $
        Left ReasonReceiverFull
    let inst' = inst { biStorage = item : biStorage inst }
    pure bm { bmInstances = HM.insert bid inst' (bmInstances bm) }

removeIndex ∷ Int → [a] → [a]
removeIndex i xs = let (pre, post) = splitAt i xs in pre ++ drop 1 post

-- | @unit.transferContract()@ → @{ states, reasons, operations }@,
--   each an array of the engine's stable identifiers. The Lua-side
--   queue B1/C1/C2 build reads its vocabulary from here rather than
--   hardcoding strings that could drift from the Haskell enums.
unitTransferContractFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTransferContractFn _ = do
    Lua.newtable
    pushArrayField "states" allTransferStateIds
    pushArrayField "reasons" (map transferReasonId allTransferReasons)
    pushArrayField "operations"
        (map transferOperationId allTransferOperations)
    return 1

pushArrayField ∷ Lua.Name → [Text] → Lua.LuaE Lua.Exception ()
pushArrayField key vals = do
    Lua.newtable
    forM_ (zip [1 ..] vals) $ \(i, v) → do
        Lua.pushstring (TE.encodeUtf8 v)
        Lua.rawseti (-2) (fromIntegral (i ∷ Int))
    Lua.setfield (-2) key

-- | @unit.transferReceiverInfo(kind, id)@ → @{ eligible, displayName,
--   page, gridX, gridY }@ | nil. The B1 (#1014) read-only projection
--   'planTransfer' itself is missing: reports whether @(kind, id)@ is
--   an eligible transfer receiver via the SAME 'receiverEligible'
--   predicate the contract enforces, plus the display name and
--   location a session record needs. nil when @kind@ isn't
--   @"building"@/@"unit"@ or the id doesn't resolve to a live
--   instance — the caller can't tell "ineligible" from "gone" apart
--   from that, which is deliberate: both mean "don't offer Transfer".
--
--   Independent of adjacency and of any chosen item on purpose — B1
--   needs this to work for a receiver the source unit hasn't reached
--   yet (requirement 6), and before C1 has picked an item at all.
unitTransferReceiverInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTransferReceiverInfoFn env = do
    kindArg ← Lua.tostring 1
    idArg   ← Lua.tointeger 2
    case (kindArg, idArg) of
        (Just kindBS, Just n) → do
            let kind = TE.decodeUtf8Lenient kindBS
            ls ← Lua.liftIO $ readLiveState env
            case kind of
                "building" → pushReceiverInfo
                    (buildingReceiver ls (BuildingId (fromIntegral n)))
                    (buildingDisplayName ls (BuildingId (fromIntegral n)))
                "unit" → pushReceiverInfo
                    (unitReceiver ls (UnitId (fromIntegral n)))
                    (unitDisplayName ls (UnitId (fromIntegral n)))
                _ → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

buildingDisplayName ∷ LiveState → BuildingId → Maybe Text
buildingDisplayName ls bid = do
    inst ← HM.lookup bid (bmInstances (lsBuildings ls))
    def  ← HM.lookup (biDefName inst) (bmDefs (lsBuildings ls))
    pure (bdDisplayName def)

-- | Mirrors 'unitGetInfoFn'\'s own displayName fallback (species
--   display_name, else the prettified def name) so this and
--   @unit.getInfo@ can never report two different names for the same
--   unit.
unitDisplayName ∷ LiveState → UnitId → Maybe Text
unitDisplayName ls uid = do
    inst ← HM.lookup uid (umInstances (lsUnits ls))
    let mDef = HM.lookup (uiDefName inst) (umDefs (lsUnits ls))
    pure (fromMaybe (prettifyDefName (uiDefName inst)) (mDef >>= udDisplayName))

pushReceiverInfo ∷ Maybe TransferReceiverView → Maybe Text
                 → Lua.LuaE Lua.Exception Lua.NumResults
pushReceiverInfo Nothing _ = Lua.pushnil >> return 1
pushReceiverInfo (Just view) mName = do
    Lua.newtable
    Lua.pushboolean (receiverEligible view)
    Lua.setfield (-2) "eligible"
    pushTextField "displayName" (fromMaybe "" mName)
    let (WorldPageId page, (gx, gy)) = case view of
            BuildingReceiverAt b → (brvPage b, brvAnchor b)
            UnitReceiverAt     u → (urvPage u, urvTile u)
    pushTextField "page" page
    Lua.pushinteger (fromIntegral gx)
    Lua.setfield (-2) "gridX"
    Lua.pushinteger (fromIntegral gy)
    Lua.setfield (-2) "gridY"
    return 1
