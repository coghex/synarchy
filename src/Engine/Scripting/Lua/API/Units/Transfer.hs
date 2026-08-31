{-# LANGUAGE Strict #-}
-- | Lua surface for the player-managed transfer contract (#1000,
--   generalized to endpoint pairs and ordered instance batches by
--   #1085). Four verbs, all routed through the pure policy in
--   "Unit.Transfer" so C1's paired inventory panel and C2's
--   walk-then-commit share one eligibility, proximity and capacity rule
--   instead of three:
--
--   * @unit.checkTransfer@       — create-time validation (no mutation)
--   * @unit.commitTransfer@      — revalidate and move, per item
--   * @unit.transferContract@    — the engine's identifier vocabulary
--   * @unit.transferEndpointInfo@ — read-only endpoint projection
--
--   Both verbs take ONE named request table:
--
--   > { source      = { kind = "unit",     id = 42 },
--   >   destination = { kind = "building", id = 7 },
--   >   items       = { { instanceId = 101, defName = "ration" }, … } }
--
--   and answer with @{ accepted, completion, outcomes }@ — one outcome
--   per requested item, in request order. A malformed call (bad types,
--   missing endpoint fields, unknown endpoint kind, out-of-range
--   endpoint id) is an ARGUMENT error and returns nil; a structurally
--   valid batch that policy refuses is a table, so the two can never be
--   confused.
--
--   Deliberately separate from "Engine.Scripting.Lua.API.Units.Cargo":
--   those verbs stay exactly as they are (no capacity check for
--   unit-to-unit, no adjacency check at all) because the AI fetch,
--   repair and medic ladders depend on that laxity.
module Engine.Scripting.Lua.API.Units.Transfer
  ( unitCheckTransferFn
  , unitCommitTransferFn
  , unitTransferContractFn
  , unitTransferEndpointInfoFn
    -- * Cross-manager internals, exposed for the rollback gate
    --
    --   'commitCross' takes its pop and push halves as arguments, and
    --   its ROLLBACK branch only runs when the destination refuses
    --   after the source already popped. Inside one call the plan and
    --   the push re-read the same refs and therefore always agree, so
    --   nothing a single-threaded test can arrange makes that branch
    --   fire through @unit.commitTransfer@ — it exists for a genuinely
    --   concurrent mutation. Exporting these lets
    --   'Test.Headless.Unit.TransferApi' drive the REAL pop, the REAL
    --   restore and the REAL manager writes against a stub push, which
    --   is the only way to gate "a failed cross-manager write restores
    --   the source item at its original index" — and, since #1274, the
    --   branch where a stub push destroys the source first, so the
    --   restore has nothing to splice into.
  , commitCross
  , popUnit
  , popBuilding
  , LivePlacement(..)
  , PopStep
  , PushStep
  , RestoreStep
    -- * Internals shared with the ORDER executor (#1247)
    --
    --   "Engine.Scripting.Lua.API.Units.TransferOrder" drives the same
    --   contract at a distance: it creates a durable order from the
    --   identical request table, and commits it through the identical
    --   per-endpoint-pair atomic paths once the carrier arrives. These
    --   are exported so it REUSES the one parser, the one live-scene
    --   projection, the one commit dispatcher and the one result
    --   encoder rather than growing a second copy of any of them —
    --   a second request parser in particular would be free to drift on
    --   exactly the malformed-versus-refused boundary this module's
    --   own haddock spells out.
  , LiveState(..)
  , readLiveState
  , sceneFor
  , endpointView
  , readRequest
  , commitOneLive
  , pushBatchResult
  , pushRequestError
  , pushArgError
  , pushTextField
  )
    where

import UPrelude
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv)
import Unit.Types
import Unit.Faction (isPlayerCommandable)
import Unit.Stats (effectiveStat)
import Unit.Transfer
import Building.Types
    (BuildingId(..), BuildingActivity(..), BuildingDef(..)
    , BuildingInstance(..), BuildingManager(..), currentActivity
    , footprintDistBetween)
import Building.Knowledge.Live (containerObserver, revealContainerForUnit)
import Item.Types (ItemInstance(..), ItemManager, itemTotalWeight)
import World.Page.Types (WorldPageId(..))
import Engine.Scripting.Lua.API.Equipment.Render (pushItemInstance)
import Engine.Scripting.Lua.Util (isDenseArray)
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
    ⊛ readReadOnlyRef (crvItemManagerRef (toContentRegistriesViewCapability env))
    ⊛ readIORef (wsGameTimeRef (toWorldSimCapability env))

-- | Where an endpoint physically is, re-read INSIDE a commit
--   transaction. The pop side captures it so the push side can measure
--   reach against where the source actually was when the item left its
--   hands, not against the opening snapshot.
data LivePlacement = LivePlacement
    { lpPage   ∷ !WorldPageId
    , lpAnchor ∷ !(Int, Int)
    , lpSize   ∷ !(Int, Int)
    }

-- * Projections

-- | Project a unit endpoint. Eligibility is
--   'Unit.Faction.isPlayerCommandable' of its live faction — #1085
--   deleted the @transfer_receiver@ data marker, so an acolyte and a
--   technomule are equally valid endpoints and a wolf is not. Capacity
--   is the modifier-applied @carrying_capacity@ stat against the
--   getCarryingWeight measure (inventory + equipment + accessories at
--   full recursive weight) — the same pair the pickup and fetch gates
--   already use. A unit with no such stat projects capacity 0, which
--   the policy reads as no room.
unitEndpoint ∷ LiveState → UnitId → Maybe TransferEndpointView
unitEndpoint ls uid = do
    u ← HM.lookup uid (umInstances (lsUnits ls))
    let weighOne = itemTotalWeight (lsItems ls)
        capBase  = HM.lookupDefault 0 "carrying_capacity" (uiStats u)
        capMods  = HM.lookupDefault [] "carrying_capacity" (uiModifiers u)
    pure $ UnitEndpointAt UnitEndpointView
        { uevPage          = uiPage u
        , uevTile          = (floor (uiGridX u), floor (uiGridY u))
        , uevCommandable   = isPlayerCommandable (uiFactionId u)
        , uevCapacity      = effectiveStat (lsNow ls) capBase capMods
        , uevInventory     = uiInventory u
        , uevEquipped      = HM.elems (uiEquipment u) ⧺ uiAccessories u
        , uevCarriedWeight = sum (map weighOne (uiInventory u))
                           + sum (map weighOne (HM.elems (uiEquipment u)))
                           + sum (map weighOne (uiAccessories u))
        }

buildingEndpoint ∷ LiveState → BuildingId → Maybe TransferEndpointView
buildingEndpoint ls bid = do
    inst ← HM.lookup bid (bmInstances (lsBuildings ls))
    def  ← HM.lookup (biDefName inst) (bmDefs (lsBuildings ls))
    pure $ BuildingEndpointAt BuildingEndpointView
        { bevPage         = biPage inst
        , bevAnchor       = (biAnchorX inst, biAnchorY inst)
        , bevTileSize     = (biTileW inst, biTileH inst)
        , bevBuilt        = currentActivity (lsNow ls) inst def ≡ Built
        , bevCapacity     = bdStorageCapacity def
        , bevStorage      = biStorage inst
        , bevStoredWeight =
            sum (map (itemTotalWeight (lsItems ls)) (biStorage inst))
        }

endpointView ∷ LiveState → TransferEndpoint → Maybe TransferEndpointView
endpointView ls (EndpointUnit uid)     = unitEndpoint ls uid
endpointView ls (EndpointBuilding bid) = buildingEndpoint ls bid

sceneFor ∷ LiveState → TransferEndpoint → TransferEndpoint → TransferScene
sceneFor ls from to = TransferScene
    { tscSource      = endpointView ls from
    , tscDestination = endpointView ls to
    , tscWeigh       = itemTotalWeight (lsItems ls)
    }

-- * Request parsing

-- | The whole request table at the given stack index. Anything
--   structurally wrong is Nothing — an ARGUMENT error, distinct from a
--   policy refusal. An empty @items@ array parses fine and is refused
--   by 'validateBatch' as @empty_batch@; a MISSING or non-table @items@
--   is malformed and rejected here.
--
--   The index must be ABSOLUTE (positive): the field reads below push
--   and pop around it, so a relative index would slide underneath them.
--   @unit.checkTransfer@/@unit.commitTransfer@ pass 1;
--   @unit.createTransferOrder@ (#1247) passes 2, the request sitting
--   after the acting unit's id.
readRequest ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe TransferRequest)
readRequest idx = do
    ty ← Lua.ltype idx
    if ty ≢ Lua.TypeTable then pure Nothing else do
        mSrc   ← readEndpointField idx "source"
        mDst   ← readEndpointField idx "destination"
        mItems ← readItemsField idx
        pure $ TransferRequest ⊚ mSrc ⊛ mDst ⊛ mItems

-- | @{ kind = "unit"|"building", id = n }@ at the given stack index.
--   The id must be a non-negative value inside the 'Word32' range both
--   'UnitId' and 'BuildingId' are; anything else is malformed rather
--   than a refusal, so a negative Lua integer can never wrap into a
--   live id.
readEndpointAt ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe TransferEndpoint)
readEndpointAt idx = do
    ty ← Lua.ltype idx
    if ty ≢ Lua.TypeTable then pure Nothing else do
        mKind ← readStringField idx "kind"
        mId   ← readIntegerField idx "id"
        pure $ do
            kind ← mKind
            n    ← mId
            unless (n ≥ 0 ∧ n ≤ 4294967295) Nothing
            case kind of
                "unit"     → Just (EndpointUnit (UnitId (fromIntegral n)))
                "building" → Just (EndpointBuilding
                                     (BuildingId (fromIntegral n)))
                _          → Nothing

-- | A field that must GENUINELY be a string. Lua's own @tostring@
--   coercion would happily turn a number into one, and a malformed type
--   is an argument error here, not a def name that can never match.
readStringField ∷ Lua.StackIndex → Lua.Name
                → Lua.LuaE Lua.Exception (Maybe Text)
readStringField idx name = do
    _  ← Lua.getfield idx name
    ty ← Lua.ltype (-1)
    r  ← if ty ≢ Lua.TypeString then pure Nothing else do
            mBS ← Lua.tostring (-1)
            pure (TE.decodeUtf8Lenient ⊚ mBS)
    Lua.pop 1
    pure r

-- | The integer mirror of 'readStringField': a numeric string does not
--   silently become an id.
readIntegerField ∷ Lua.StackIndex → Lua.Name
                 → Lua.LuaE Lua.Exception (Maybe Lua.Integer)
readIntegerField idx name = do
    _  ← Lua.getfield idx name
    ty ← Lua.ltype (-1)
    r  ← if ty ≢ Lua.TypeNumber then pure Nothing else Lua.tointeger (-1)
    Lua.pop 1
    pure r

readEndpointField ∷ Lua.StackIndex → Lua.Name
                  → Lua.LuaE Lua.Exception (Maybe TransferEndpoint)
readEndpointField idx name = do
    _ ← Lua.getfield idx name
    r ← readEndpointAt (-1)
    Lua.pop 1
    pure r

-- | The ordered @items@ array. Every entry must be
--   @{ instanceId = <integer>, defName = <string> }@. The instance id is
--   kept SIGNED: a zero or negative value is a per-item
--   @instance_unspecified@ refusal the policy reports, NOT a parse
--   failure and never a 'Word64' that wrapped.
--
--   The table must be a DENSE one-based array. 'Lua.rawlen' returns a
--   *border*, not a count, so a sparse @{ [1] = a, [3] = b }@ can report
--   length 1 — a plain @1..n@ loop would then move @a@, silently drop
--   @b@, and emit no outcome for it, breaking the contract that
--   @accepted = true@ carries exactly one outcome per requested item.
--   Which of the two lengths a given hole layout reports is a detail of
--   the table's internals, so this rejects the whole shape as a
--   malformed ARGUMENT rather than processing a prefix.
readItemsField ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe [TransferItemRef])
readItemsField idx = do
    _  ← Lua.getfield idx "items"
    ty ← Lua.ltype (-1)
    r  ← if ty ≢ Lua.TypeTable then pure Nothing else do
            dense ← isDenseArray (-1)
            if not dense then pure Nothing else do
                n ← Lua.rawlen (-1)
                let go i acc
                      | i > fromIntegral n = pure (Just (reverse acc))
                      | otherwise = do
                          _    ← Lua.rawgeti (-1) i
                          mRef ← readItemRefAt (-1)
                          Lua.pop 1
                          case mRef of
                              Nothing  → pure Nothing
                              Just ref → go (i + 1) (ref : acc)
                go 1 []
    Lua.pop 1
    pure r

readItemRefAt ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe TransferItemRef)
readItemRefAt idx = do
    ty ← Lua.ltype idx
    if ty ≢ Lua.TypeTable then pure Nothing else do
        mInst ← readIntegerField idx "instanceId"
        mName ← readStringField idx "defName"
        pure $ TransferItemRef ⊚ (fromIntegral ⊚ mInst) ⊛ mName

-- * Result encoding

-- | @{ accepted = true, completion, outcomes = { … } }@ — exactly one
--   outcome per requested item, in request order, INCLUDING items that
--   failed the initial check and were therefore never attempted, so a
--   caller can always recompute @completion@ from @outcomes@.
pushBatchResult ∷ TransferBatch → TransferCompletion
                → Lua.LuaE Lua.Exception Lua.NumResults
pushBatchResult batch completion = do
    Lua.newtable
    Lua.pushboolean True
    Lua.setfield (-2) "accepted"
    pushTextField "completion" (transferCompletionId completion)
    Lua.newtable
    forM_ (zip [1 ..] (tbEntries batch)) $ \(i, q) → do
        Lua.newtable
        Lua.pushinteger (fromIntegral (tirInstanceId (qtItem q)))
        Lua.setfield (-2) "instanceId"
        pushTextField "defName" (tirDefName (qtItem q))
        pushTextField "state" (transferStateId (qtState q))
        case qtState q of
            TransferFailed f → do
                pushTextField "reason" (transferReasonId (tfReason f))
                forM_ (tfCause f) $ \c →
                    pushTextField "cause" (transferReasonId c)
            _ → pure ()
        Lua.rawseti (-2) (fromIntegral (i ∷ Int))
    Lua.setfield (-2) "outcomes"
    return 1

-- | @{ accepted = false, reason, outcomes = {} }@ — a whole-request
--   rejection. No item was checked and nothing was mutated, so there
--   are deliberately no outcomes to report.
pushRequestError ∷ TransferRequestError
                 → Lua.LuaE Lua.Exception Lua.NumResults
pushRequestError e = do
    Lua.newtable
    Lua.pushboolean False
    Lua.setfield (-2) "accepted"
    pushTextField "reason" (transferRequestErrorId e)
    Lua.newtable
    Lua.setfield (-2) "outcomes"
    return 1

pushTextField ∷ Lua.Name → Text → Lua.LuaE Lua.Exception ()
pushTextField key val = do
    Lua.pushstring (TE.encodeUtf8 val)
    Lua.setfield (-2) key

-- | Bad arguments are distinct from a refused transfer: nil, not a
--   result table with a reason that would look like policy.
pushArgError ∷ Lua.LuaE Lua.Exception Lua.NumResults
pushArgError = Lua.pushnil >> return 1

-- * The two batch verbs

-- | @unit.checkTransfer(request)@ → result table | nil. Pure
--   validation: reports the state a freshly created order would hold
--   per item (@queued@, or @failed@ with the offending reason) and
--   mutates nothing. Items are checked IN ORDER against a
--   progressively updated snapshot, so capacity is remeasured after
--   each provisionally accepted item and twelve items with room for
--   eight queue the first eight. The answer is advisory by
--   construction — commitTransfer revalidates.
unitCheckTransferFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCheckTransferFn env = do
    mReq ← readRequest 1
    case mReq of
        Nothing  → pushArgError
        Just req → do
            ls ← Lua.liftIO $ readLiveState env
            case checkBatch (sceneFor ls (trSource req) (trDestination req))
                            req of
                Left e  → pushRequestError e
                Right b → pushBatchResult b (checkCompletion b)

-- | @unit.commitTransfer(request)@ → result table | nil. Runs the same
--   ordered initial check, then commits each still-queued item
--   SEQUENTIALLY through the atomic single-item path for its endpoint
--   pair, re-reading live state every time. One item's failure neither
--   rolls back a successful sibling nor stops a later one; there is no
--   batch-wide transaction or prepare phase.
unitCommitTransferFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCommitTransferFn env = do
    mReq ← readRequest 1
    case mReq of
        Nothing  → pushArgError
        Just req → do
            outcome ← Lua.liftIO $ commitRequestLive env req
            case outcome of
                Left e  → pushRequestError e
                Right b → pushBatchResult b (commitCompletion b)

commitRequestLive ∷ EngineEnv → TransferRequest
                  → IO (Either TransferRequestError TransferBatch)
commitRequestLive env req = do
    ls ← readLiveState env
    case checkBatch (sceneFor ls from to) req of
        Left e  → pure (Left e)
        Right b → do
            entries ← mapM attempt (tbEntries b)
            pure (Right b { tbEntries = entries })
  where
    from = trSource req
    to   = trDestination req
    attempt q
        | qtState q ≢ TransferQueued = pure q
        | otherwise = do
            r ← commitOneLive env from to (qtItem q)
            pure $ case r of
                Left f  → q { qtState = TransferFailed f }
                Right _ → q { qtState = TransferCompleted }

-- | Dispatch one item to the atomic path its endpoint pair needs. All
--   four pairs are live: the two same-manager pairs commit inside a
--   single 'atomicModifyIORef'', the two cross-manager pairs
--   pop-then-push with an order-preserving rollback.
commitOneLive ∷ EngineEnv → TransferEndpoint → TransferEndpoint
              → TransferItemRef → IO (Either TransferFailure ItemInstance)
commitOneLive env from to ref = case (from, to) of
    (EndpointUnit a, EndpointUnit b) →
        commitUnitToUnit env a b ref
    (EndpointBuilding a, EndpointBuilding b) →
        commitBuildingToBuilding env a b ref
    (EndpointUnit a, EndpointBuilding b) →
        commitCross env from to (popUnit a) (pushBuilding b) ref
            ⌦ revealAfter env a b
    (EndpointBuilding a, EndpointUnit b) →
        commitCross env from to (popBuilding a) (pushUnit b) ref
            ⌦ revealAfter env b a

-- | #1087: a successful transfer between a player unit and a building
--   container teaches the player what is in there.
--
--   Fires only on the Right branch, so the rollback can never reach it,
--   and only AFTER the push committed, so the record snapshots the
--   FINAL post-commit storage — the two properties A3 established for
--   'depositToCargo'. It runs per successfully moved ITEM, which is
--   what keeps a PARTIAL batch honest: the record reflects the state
--   the last item that actually landed left behind.
--
--   Both unit↔building directions reveal, matching A3's own coverage:
--   it already reveals on 'withdrawFromCargo' as well as
--   'depositToCargo', and only hooked the deposit direction of the
--   strict path because A2 is what makes a building SOURCE expressible
--   at all. Building↔building deliberately does not: there is no
--   acting unit, and 'revealContainerForUnit' is gated on one.
revealAfter ∷ EngineEnv → UnitId → BuildingId
            → Either TransferFailure ItemInstance
            → IO (Either TransferFailure ItemInstance)
revealAfter env uid bid outcome = do
    case outcome of
        Right _ → void $ revealContainerForUnit
            (containerObserver (toBuildingCapability env)
                               (toWorldSimCapability env)
                               (toContentRegistriesViewCapability env))
            (ucUnitManagerRef (toUnitCombatCapability env)) uid bid
        Left _  → pure ()
    pure outcome

-- | Unit → unit. Both inventories live in the SAME manager ref, so
--   revalidation and both writes happen inside one
--   'atomicModifyIORef'' — no interleaving can duplicate or drop the
--   instance, and no rollback is needed because there is never a
--   half-applied state.
commitUnitToUnit ∷ EngineEnv → UnitId → UnitId → TransferItemRef
                 → IO (Either TransferFailure ItemInstance)
commitUnitToUnit env fromUid toUid ref = do
    itemMgr ← readReadOnlyRef (crvItemManagerRef (toContentRegistriesViewCapability env))
    now     ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    bm      ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
    atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
        let ls    = LiveState { lsUnits = um, lsBuildings = bm
                              , lsItems = itemMgr, lsNow = now }
            from  = EndpointUnit fromUid
            to    = EndpointUnit toUid
            scene = sceneFor ls from to
        in case commitItem scene from to ref of
            Left f  → (um, Left f)
            Right c → case ( HM.lookup fromUid (umInstances um)
                           , HM.lookup toUid (umInstances um) ) of
                (Just uF, Just uT) →
                    let uF'   = uF { uiInventory = tcSourceItems c }
                        uT'   = uT { uiInventory = tcDestinationItems c }
                        insts = HM.insert toUid uT'
                              $ HM.insert fromUid uF'
                              $ umInstances um
                    in (um { umInstances = insts }, Right (tcItem c))
                -- Unreachable: commitItem only succeeds when both
                -- projections resolved out of this very map.
                _ → (um, Left (staleFailure ReasonSourceMissing))

-- | Building → building. Same shape as unit → unit for the same
--   reason: both storages live in the building manager's one ref, so
--   the whole move is a single atomic update with nothing to roll
--   back.
commitBuildingToBuilding ∷ EngineEnv → BuildingId → BuildingId
                         → TransferItemRef
                         → IO (Either TransferFailure ItemInstance)
commitBuildingToBuilding env fromBid toBid ref = do
    itemMgr ← readReadOnlyRef (crvItemManagerRef (toContentRegistriesViewCapability env))
    now     ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    um      ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
        let ls    = LiveState { lsUnits = um, lsBuildings = bm
                              , lsItems = itemMgr, lsNow = now }
            from  = EndpointBuilding fromBid
            to    = EndpointBuilding toBid
            scene = sceneFor ls from to
        in case commitItem scene from to ref of
            Left f  → (bm, Left f)
            Right c → case ( HM.lookup fromBid (bmInstances bm)
                           , HM.lookup toBid (bmInstances bm) ) of
                (Just bF, Just bT) →
                    let bF'   = bF { biStorage = tcSourceItems c }
                        bT'   = bT { biStorage = tcDestinationItems c }
                        insts = HM.insert toBid bT'
                              $ HM.insert fromBid bF'
                              $ bmInstances bm
                    in (bm { bmInstances = insts }, Right (tcItem c))
                _ → (bm, Left (staleFailure ReasonSourceMissing))

-- | Pop the planned instance out of a source that lives in one ref.
--   Returns the popped instance plus the source's LIVE placement, so
--   the push side can measure reach against where the source actually
--   was when the item left rather than against the opening snapshot.
type PopStep = EngineEnv → Int → ItemInstance
             → IO (Either TransferReason (ItemInstance, LivePlacement))

-- | Push an instance into a destination that lives in the other ref,
--   re-checking every destination-side precondition against the
--   source's live placement.
type PushStep = EngineEnv → ItemInstance → LivePlacement
              → IO (Either TransferReason ())

-- | Restore a popped instance at its ORIGINAL index after a failed
--   push. Inventory order is player-visible, so "unchanged" means
--   order-preserving, not merely same-multiset.
--
--   The result reports whether the splice-back actually happened
--   (#1274). It fails with 'ReasonSourceMissing' when the source is no
--   longer in its manager — another thread tore it down between the pop
--   and this call — because there is then nothing to restore into and
--   'commitCross' must say so rather than discard the outcome.
type RestoreStep = EngineEnv → Int → ItemInstance
                 → IO (Either TransferReason ())

-- | The cross-manager shape depositToCargo already uses: validate
--   against a fresh scene, pop under the source's ref, push under the
--   destination's ref, and splice the instance back at its original
--   index if the push fails.
--
--   EVERY precondition is revalidated inside that transaction, each
--   against whichever side owns it: the pop re-checks the exact
--   instance is still at the planned index AND the source's LIVE
--   eligibility and placement, and the push re-checks the
--   destination's LIVE eligibility, capacity and reach. Validating
--   proximity only from the opening snapshot would let a unit that
--   walked away (or was teleported) mid-call still deposit from out of
--   range.
--
--   CONCURRENT SOURCE TEARDOWN IS THE ONE EXCEPTION to #190's
--   restore-on-failure guarantee (#1274). The pop and the push are
--   separate transactions on separate refs, so another thread —
--   @handleUnitDestroyCommand@, @BuildingDestroy@ — can remove the
--   source instance in the window between them. The restore then has
--   nothing to splice back into, and the item is CONSUMED WITH THE
--   SOURCE: that is exactly what teardown does to the rest of the
--   source's contents, which it deletes with the instance and never
--   spills to the ground, so the in-flight item ends up where it would
--   have anyway. No path duplicates it — the failing push wrote
--   nothing, and the vanished source cannot be written to.
--
--   What this must NOT do is report that silently. Failure precedence:
--   a restore that fails because the source vanished OVERRIDES the
--   destination's refusal, so the caller is told
--   @staleFailure ReasonSourceMissing@ (@became_stale/source_missing@)
--   rather than a stale destination-side reason that would imply a
--   splice-back which never happened. A restore that SUCCEEDS keeps
--   returning the destination's original refusal, unchanged.
commitCross ∷ EngineEnv → TransferEndpoint → TransferEndpoint
            → (PopStep, RestoreStep) → PushStep → TransferItemRef
            → IO (Either TransferFailure ItemInstance)
commitCross env from to (pop, restore) push ref = do
    ls ← readLiveState env
    case commitItem (sceneFor ls from to) from to ref of
        Left f  → pure (Left f)
        Right c → do
            popped ← pop env (tcIndex c) (tcItem c)
            case popped of
                Left r → pure (Left (staleFailure r))
                Right (item, srcPlace) → do
                    stored ← push env item srcPlace
                    case stored of
                        Right () → pure (Right item)
                        Left r   → do
                            restored ← restore env (tcIndex c) item
                            pure $ Left $ staleFailure $ case restored of
                                Right () → r
                                Left rr  → rr

popUnit ∷ UnitId → (PopStep, RestoreStep)
popUnit uid = (pop, restore)
  where
    pop env ix planned =
        atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
            case HM.lookup uid (umInstances um) of
                Nothing → (um, Left ReasonSourceMissing)
                Just u
                    | not (isPlayerCommandable (uiFactionId u)) →
                        (um, Left ReasonSourceIneligible)
                    | otherwise → case drop ix (uiInventory u) of
                        (live : _)
                            | ix ≥ 0
                            , iiInstanceId live ≡ iiInstanceId planned →
                                let tile = ( floor (uiGridX u)
                                           , floor (uiGridY u) )
                                    u'   = u { uiInventory =
                                                 removeIndex ix (uiInventory u) }
                                    place = LivePlacement
                                              { lpPage   = uiPage u
                                              , lpAnchor = tile
                                              , lpSize   = (1, 1) }
                                in ( um { umInstances =
                                            HM.insert uid u' (umInstances um) }
                                   , Right (live, place) )
                        _ → (um, Left ReasonInstanceMissing)
    -- The unit was destroyed between the pop and here: its whole
    -- uiInventory went with it, so there is no inventory left to splice
    -- into and the caller is told so (#1274).
    restore env ix item =
        atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
            case HM.lookup uid (umInstances um) of
                Nothing → (um, Left ReasonSourceMissing)
                Just u  →
                    let u' = u { uiInventory = insertAt ix item (uiInventory u) }
                    in ( um { umInstances = HM.insert uid u' (umInstances um) }
                       , Right () )

popBuilding ∷ BuildingId → (PopStep, RestoreStep)
popBuilding bid = (pop, restore)
  where
    pop env ix planned = do
        now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
        atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
            case resolveBuilding bm bid of
                Nothing → (bm, Left ReasonSourceMissing)
                Just (inst, def)
                    | currentActivity now inst def ≢ Built
                      ∨ bdStorageCapacity def ≤ 0 →
                        (bm, Left ReasonSourceIneligible)
                    | otherwise → case drop ix (biStorage inst) of
                        (live : _)
                            | ix ≥ 0
                            , iiInstanceId live ≡ iiInstanceId planned →
                                let inst' = inst { biStorage =
                                                     removeIndex ix
                                                       (biStorage inst) }
                                    place = LivePlacement
                                              { lpPage   = biPage inst
                                              , lpAnchor = ( biAnchorX inst
                                                           , biAnchorY inst )
                                              , lpSize   = ( biTileW inst
                                                           , biTileH inst ) }
                                in ( bm { bmInstances =
                                            HM.insert bid inst' (bmInstances bm) }
                                   , Right (live, place) )
                        _ → (bm, Left ReasonInstanceMissing)
    -- Demolished between the pop and here: biStorage went with the
    -- instance, so there is nothing to splice into (#1274).
    restore env ix item =
        atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
            case HM.lookup bid (bmInstances bm) of
                Nothing → (bm, Left ReasonSourceMissing)
                Just inst →
                    let inst' = inst { biStorage =
                                         insertAt ix item (biStorage inst) }
                    in ( bm { bmInstances =
                                HM.insert bid inst' (bmInstances bm) }
                       , Right () )

-- | Final destination-side re-check under the building ref: the
--   building still exists, is still Built with storage, is still in
--   reach of where the source was when the item left, and the instance
--   still fits.
pushBuilding ∷ BuildingId → PushStep
pushBuilding bid env item srcPlace = do
    itemMgr ← readReadOnlyRef (crvItemManagerRef (toContentRegistriesViewCapability env))
    now     ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
        case resolveBuilding bm bid of
            Nothing → (bm, Left ReasonReceiverMissing)
            Just (inst, def)
                | currentActivity now inst def ≢ Built
                  ∨ bdStorageCapacity def ≤ 0 →
                    (bm, Left ReasonReceiverIneligible)
                | not (inReach srcPlace (biPage inst)
                                (biAnchorX inst, biAnchorY inst)
                                (biTileW inst, biTileH inst)) →
                    (bm, Left ReasonOutOfRange)
                | sum (map (itemTotalWeight itemMgr) (biStorage inst))
                    + itemTotalWeight itemMgr item > bdStorageCapacity def →
                    (bm, Left ReasonReceiverFull)
                | otherwise →
                    let inst' = inst { biStorage = item : biStorage inst }
                    in ( bm { bmInstances =
                                HM.insert bid inst' (bmInstances bm) }
                       , Right () )

-- | The unit-destination mirror of 'pushBuilding': still exists, still
--   player-commandable, still in reach, still has capacity.
pushUnit ∷ UnitId → PushStep
pushUnit uid env item srcPlace = do
    itemMgr ← readReadOnlyRef (crvItemManagerRef (toContentRegistriesViewCapability env))
    now     ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
        case HM.lookup uid (umInstances um) of
            Nothing → (um, Left ReasonReceiverMissing)
            Just u
                | not (isPlayerCommandable (uiFactionId u)) →
                    (um, Left ReasonReceiverIneligible)
                | not (inReach srcPlace (uiPage u)
                                ( floor (uiGridX u), floor (uiGridY u) )
                                (1, 1)) →
                    (um, Left ReasonOutOfRange)
                | otherwise →
                    let weighOne = itemTotalWeight itemMgr
                        capBase  = HM.lookupDefault 0 "carrying_capacity"
                                                      (uiStats u)
                        capMods  = HM.lookupDefault [] "carrying_capacity"
                                                       (uiModifiers u)
                        cap      = effectiveStat now capBase capMods
                        carried  = sum (map weighOne (uiInventory u))
                                 + sum (map weighOne
                                            (HM.elems (uiEquipment u)))
                                 + sum (map weighOne (uiAccessories u))
                    in if cap > 0 ∧ carried + weighOne item ≤ cap
                       then let u' = u { uiInventory = uiInventory u ⧺ [item] }
                            in ( um { umInstances =
                                        HM.insert uid u' (umInstances um) }
                               , Right () )
                       else (um, Left ReasonReceiverFull)

resolveBuilding ∷ BuildingManager → BuildingId
                → Maybe (BuildingInstance, BuildingDef)
resolveBuilding bm bid = do
    inst ← HM.lookup bid (bmInstances bm)
    def  ← HM.lookup (biDefName inst) (bmDefs bm)
    pure (inst, def)

-- | Same page and Chebyshev ≤ 1 between the two occupied rectangles —
--   the identical measure 'Unit.Transfer.withinReach' applies, so the
--   in-transaction re-check and the policy can't disagree about what
--   "adjacent" means.
inReach ∷ LivePlacement → WorldPageId → (Int, Int) → (Int, Int) → Bool
inReach src page anchor size =
    lpPage src ≡ page
      ∧ footprintDistBetween (lpAnchor src) (lpSize src) anchor size ≤ 1

removeIndex ∷ Int → [a] → [a]
removeIndex i xs = let (pre, post) = splitAt i xs in pre ++ drop 1 post

-- * Vocabulary and endpoint introspection

-- | @unit.transferContract()@ →
--   @{ states, reasons, requestErrors, endpointKinds }@. The three
--   lists are arrays of the engine's stable identifiers;
--   @endpointKinds@ is a SET keyed by name (@endpointKinds.unit@,
--   @endpointKinds.building@) rather than an array, so no Lua caller
--   depends on a position the Haskell enum could reorder.
--   @requestErrors@ carries the whole-request ids (@empty_batch@,
--   @duplicate_instance@) that never appear as a per-item reason, so a
--   caller can validate an @accepted = false@ refusal with the same
--   discipline it applies to a per-item one.
unitTransferContractFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTransferContractFn _ = do
    Lua.newtable
    pushArrayField "states" allTransferStateIds
    pushArrayField "reasons" (map transferReasonId allTransferReasons)
    pushArrayField "requestErrors"
        (map transferRequestErrorId allTransferRequestErrors)
    Lua.newtable
    forM_ allTransferEndpointKinds $ \k → do
        Lua.pushboolean True
        Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 (transferEndpointKindId k)))
    Lua.setfield (-2) "endpointKinds"
    return 1

pushArrayField ∷ Lua.Name → [Text] → Lua.LuaE Lua.Exception ()
pushArrayField key vals = do
    Lua.newtable
    forM_ (zip [1 ..] vals) $ \(i, v) → do
        Lua.pushstring (TE.encodeUtf8 v)
        Lua.rawseti (-2) (fromIntegral (i ∷ Int))
    Lua.setfield (-2) key

-- | @unit.transferEndpointInfo({ kind = …, id = … })@ →
--   @{ eligible, displayName, page, gridX, gridY, tileW, tileH,
--      capacity, storedWeight, contents }@ | nil.
--
--   @gridX@\/@gridY@ and @tileW@\/@tileH@ are the two halves of the ONE
--   'endpointRect' the adjacency rule itself measures against, so a
--   caller that walks a unit to an endpoint and a caller that asks
--   whether it is close enough cannot disagree about where the endpoint
--   is or how big it is. A unit reports a 1x1 rect; a multi-tile
--   building reports its real footprint, which is why anchor-to-anchor
--   distance would strand a walker beside the far end of one (#1250).
--
--   Answers for EITHER endpoint kind, in either role: the eligibility
--   rule is role-independent (a unit is player-commandable, a building
--   is Built with storage), which is exactly what the context menu
--   needs before an item has been chosen or the source has walked
--   anywhere. nil when @kind@ isn't @"building"@/@"unit"@, the id is
--   out of range, or it doesn't resolve to a live instance — the caller
--   can't tell "ineligible" from "gone" apart from that, which is
--   deliberate: both mean "don't offer Transfer".
--
--   @storedWeight@ is the endpoint's live recursive load measured by
--   the SAME rule the capacity gate uses — for a unit that is
--   inventory + equipment + accessories including fill and nested
--   contents, even though @contents@ lists loose inventory only.
--
--   Independent of adjacency and of any chosen item on purpose.
unitTransferEndpointInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTransferEndpointInfoFn env = do
    mEp ← readEndpointAt 1
    case mEp of
        Nothing → Lua.pushnil >> return 1
        Just ep → do
            ls ← Lua.liftIO $ readLiveState env
            case endpointView ls ep of
                Nothing → Lua.pushnil >> return 1
                Just v  → pushEndpointInfo ls ep v

pushEndpointInfo ∷ LiveState → TransferEndpoint → TransferEndpointView
                 → Lua.LuaE Lua.Exception Lua.NumResults
pushEndpointInfo ls ep view = do
    Lua.newtable
    Lua.pushboolean (endpointEligible view)
    Lua.setfield (-2) "eligible"
    pushTextField "displayName" (fromMaybe "" (endpointDisplayName ls ep))
    let WorldPageId page      = endpointPage view
        ((gx, gy), (tw, th))  = endpointRect view
    pushTextField "page" page
    Lua.pushinteger (fromIntegral gx)
    Lua.setfield (-2) "gridX"
    Lua.pushinteger (fromIntegral gy)
    Lua.setfield (-2) "gridY"
    Lua.pushinteger (fromIntegral tw)
    Lua.setfield (-2) "tileW"
    Lua.pushinteger (fromIntegral th)
    Lua.setfield (-2) "tileH"
    Lua.pushnumber (Lua.Number (realToFrac (endpointCapacity view)))
    Lua.setfield (-2) "capacity"
    Lua.pushnumber (Lua.Number (realToFrac (endpointLoad view)))
    Lua.setfield (-2) "storedWeight"
    Lua.newtable
    forM_ (zip [1 ..] (endpointLooseItems view)) $ \(i, item) → do
        Lua.newtable
        pushItemInstance item (lsItems ls)
        Lua.rawseti (-2) (fromIntegral (i ∷ Int))
    Lua.setfield (-2) "contents"
    return 1

endpointDisplayName ∷ LiveState → TransferEndpoint → Maybe Text
endpointDisplayName ls (EndpointBuilding bid) = do
    inst ← HM.lookup bid (bmInstances (lsBuildings ls))
    def  ← HM.lookup (biDefName inst) (bmDefs (lsBuildings ls))
    pure (bdDisplayName def)
-- Mirrors 'unitGetInfoFn'\'s own displayName fallback (species
-- display_name, else the prettified def name) so this and
-- @unit.getInfo@ can never report two different names for the same
-- unit.
endpointDisplayName ls (EndpointUnit uid) = do
    inst ← HM.lookup uid (umInstances (lsUnits ls))
    let mDef = HM.lookup (uiDefName inst) (umDefs (lsUnits ls))
    pure (fromMaybe (prettifyDefName (uiDefName inst)) (mDef ⌦ udDisplayName))
