{-# LANGUAGE Strict #-}
-- | Lua surface for DURABLE transfer orders (#1247, epic #1013 slice
--   UIT-2B) — the read/create/transition verbs
--   @scripts/unit_ai_transfer.lua@ drives an order through, and the
--   only way anything outside the engine touches
--   @wsTransferOrdersRef@:
--
--   * @unit.createTransferOrder@  — validate at a DISTANCE, then queue
--   * @unit.getTransferOrders@    — every order a unit is carrying
--   * @unit.advanceTransferOrder@ — @in_transit@ / @ready_to_commit@
--   * @unit.commitTransferOrder@  — commit on arrival, exactly once
--   * @unit.failTransferOrder@    — self-terminate a doomed order
--   * @unit.cancelTransferOrder@  — the player abandoned it (#1253)
--   * @unit.pruneTransferOrder@   — drop a SURFACED terminal order
--
--   __Nothing here is a second transfer policy.__ #1246 owns the store
--   ("Unit.Transfer.Orders"), #1000/#1085 own eligibility, proximity,
--   capacity, identity and the four atomic per-endpoint-pair commit
--   paths ("Unit.Transfer",
--   "Engine.Scripting.Lua.API.Units.Transfer"). This module is the
--   join: it resolves which page's store an order belongs to, decides
--   which endpoint the acting unit walks to, and reconciles what the
--   commit dispatcher returned back onto the stored entries. Every
--   verdict it reports came out of one of those two layers.
--
--   __Two things it does own__, because nothing else could:
--
--   [Range-independent creation] An order's whole premise is that the
--   endpoints are not adjacent YET. 'planItem' refuses on range before
--   it ever weighs the item, so creating an order through the ordinary
--   'checkBatch' would answer @out_of_range@ for every legitimate
--   remote order and the create-time CAPACITY gate — the one that
--   refuses a doomed trip before the carrier walks — would never run.
--   So creation passes @ReachDeferred carrierPage@
--   ('Unit.Transfer.checkBatchWith'), which relaxes adjacency and
--   nothing else — and pins both endpoints to the CARRIER's own page,
--   because an order is stored in that page's store and the integrity
--   graph scopes the acting unit and both endpoints there as BLOCKING
--   errors. @unit.checkTransfer@/@unit.commitTransfer@ are untouched.
--
--   [Arrival reconciliation] 'commitOneLive' revalidates from scratch
--   and reports a DIRECT reason. An entry reaching arrival already
--   passed its create-time checks, so a refusal there means the world
--   moved underneath it: it is recorded as @became_stale@ carrying that
--   reason as its cause ('staleOf'), the same shape
--   'Unit.Transfer.commitBatch' produces. Create-time failures are
--   never retried — only 'TransferReadyToCommit' entries are submitted,
--   so an entry that could not fit when the order was made stays
--   refused even if room appeared during the walk.
--
--   __A stored order the job can execute always has an approach.__
--   The acting unit is recorded ALONGSIDE the endpoint pair, so a
--   carrier that is neither end is expressible — and for a unit↔unit or
--   unit↔building pair that would be a queued order with nowhere to
--   walk, skipped on every tick and cancellable by nothing in this
--   slice. 'carrierCanExecute' refuses it at creation. The single
--   exception is D-10's building→building pair, which is deliberately
--   inert until a later slice can ferry between two buildings.
--
--   __Every verb is addressed by ACTING UNIT__, not by order id alone.
--   Order ids are allocated per PAGE (#1246), so an id on its own names
--   an order only once a page is known; the acting unit supplies it,
--   and re-checking @troUnit@ against it means one unit can never
--   advance or commit another's order.
--
--   __A terminal order is PRUNED, not left lying__ (#1253, UIT-5A).
--   'unitPruneTransferOrderFn' is how, and the executor calls it one
--   step after surfacing the outcome, so the store holds live work only
--   and nothing terminal rides a save. Removal is still not automatic:
--   no verb here retires an order merely because 'batchTerminal' became
--   true, because the outcome has to reach the player FIRST and only
--   the caller knows whether it has. What keeps "commit exactly once"
--   true is unchanged and does not depend on the pruning — it is the
--   LIFECYCLE: a terminal batch has no @ready_to_commit@ entry left, so
--   a second commit moves nothing, and the unit job skips terminal
--   orders entirely.
module Engine.Scripting.Lua.API.Units.TransferOrder
  ( unitCreateTransferOrderFn
  , unitGetTransferOrdersFn
  , unitAdvanceTransferOrderFn
  , unitCommitTransferOrderFn
  , unitFailTransferOrderFn
  , unitCancelTransferOrderFn
  , unitPruneTransferOrderFn
  ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import Unit.Transfer
import Unit.Transfer.Orders
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), WorldState(..))
import Engine.Scripting.Lua.API.Units.Transfer
    ( LiveState, commitOneLive, endpointView, pushArgError
    , pushBatchResult, pushRequestError, pushTextField, readLiveState
    , readRequest, sceneFor )

-- * Argument plumbing

-- | A @unit@/@building@ id argument: present, an integer, and inside
--   the 'Word32' range both id newtypes are. Mirrors
--   'Engine.Scripting.Lua.API.Units.Transfer''s own endpoint-id rule so
--   a negative Lua integer can never wrap into a live id.
readId ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe Word32)
readId idx = do
    mN ← Lua.tointeger idx
    pure $ do
        n ← mN
        unless (n ≥ 0 ∧ n ≤ 4294967295) Nothing
        pure (fromIntegral n)

readText ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe Text)
readText idx = do
    ty ← Lua.ltype idx
    if ty ≢ Lua.TypeString then pure Nothing else do
        mBS ← Lua.tostring idx
        pure (TE.decodeUtf8Lenient ⊚ mBS)

-- | @nil, "<message>"@ — the shape 'craftAddBillFn' already uses for a
--   call the engine understood but could not carry out. Deliberately
--   distinct from 'pushArgError''s bare @nil@ (a malformed argument)
--   and from a result table (policy answered).
pushErr ∷ Text → Lua.LuaE Lua.Exception Lua.NumResults
pushErr msg = do
    Lua.pushnil
    Lua.pushstring (TE.encodeUtf8 msg)
    return 2

pushBool ∷ Bool → Lua.LuaE Lua.Exception Lua.NumResults
pushBool b = Lua.pushboolean b ≫ return 1

-- * Page resolution

-- | The page @uid@ stands on, and that page's order store.
--
--   Resolved from the UNIT rather than from the active page (the rule
--   'craftAddBillFn' follows for bills): orders are per-page state and
--   a carrier can legitimately be on a loaded, non-visible page, where
--   the active-page store would be somebody else's. 'Nothing' when the
--   unit does not exist or its page is not loaded.
--
--   The page id comes back with the store because it is not merely how
--   the store was found — it is a PRECONDITION on what may be stored in
--   it. An order's acting unit and both its endpoints must live on the
--   order's own page ("World.Save.Integrity" scopes all three there as
--   blocking errors), and the carrier is what decides which page that
--   is, so creation validates the endpoints against this value rather
--   than only against each other.
unitOrderStore ∷ EngineEnv → UnitId → IO (Maybe (WorldPageId, WorldState))
unitOrderStore env uid = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    case HM.lookup uid (umInstances um) of
        Nothing → pure Nothing
        Just u  → do
            mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
            pure ((,) (uiPage u) ⊚ lookup (uiPage u) (wmWorlds mgr))

-- | Run @f@ against the store of the page @uid@ is on, having resolved
--   the order @oid@ names AND confirmed @uid@ is the one carrying it.
--   The lookup and the write are ONE 'atomicModifyIORef'', so no
--   concurrent transition can slip between them.
withOwnedOrder ∷ EngineEnv → UnitId → TransferOrderId
               → (TransferOrder → (TransferBatch → TransferBatch))
               → IO (Maybe TransferOrder)
withOwnedOrder env uid oid f = do
    mWs ← unitOrderStore env uid
    case mWs of
        Nothing → pure Nothing
        Just (_, ws) → atomicModifyIORef' (wsTransferOrdersRef ws) $ \orders →
            case lookupTransferOrder oid orders of
                Just order | troUnit order ≡ uid →
                    let (orders', _) = updateTransferOrder oid (f order) orders
                    in (orders', lookupTransferOrder oid orders')
                _ → (orders, Nothing)

-- * Endpoint roles

-- | Which endpoint the ACTING unit approaches, and its role name.
--
--   The counterpart is simply the end the unit is not, so this answers
--   'Nothing' exactly when the carrier is neither end — and an order
--   with no approach is one the unit job can never execute
--   ('actionableOrder' skips it on every tick).
--
--   'carrierCanExecute' is what keeps that from becoming a leak: the
--   ONE shape allowed to store an order this answers 'Nothing' for is
--   D-10's building→building, which is deliberately inert until a later
--   slice gives it an executor.
approachEndpointOf ∷ UnitId → TransferEndpoint → TransferEndpoint
                   → Maybe (Text, TransferEndpoint)
approachEndpointOf uid from to
    | from ≡ EndpointUnit uid = Just ("destination", to)
    | to   ≡ EndpointUnit uid = Just ("source", from)
    | otherwise               = Nothing

approachEndpoint ∷ UnitId → TransferBatch → Maybe (Text, TransferEndpoint)
approachEndpoint uid b = approachEndpointOf uid (tbSource b) (tbDestination b)

-- | May @uid@ be the carrier for this endpoint pair at all?
--
--   Two ways to qualify, and they are not the same thing:
--
--   * The carrier IS one of the endpoints, so 'approachEndpointOf'
--     resolves and the order has somewhere to walk. This is every
--     executable order.
--   * BOTH endpoints are buildings (D-10). The acting unit is then
--     deliberately neither end — that is the whole point of recording
--     it alongside the pair rather than deriving it — and the order is
--     storable, valid, and simply not this executor's (S1). It waits,
--     inert, for the slice that ferries between two buildings.
--
--   Anything else — a unit↔unit or unit↔building pair carried by a
--   BYSTANDER — is neither. It would queue an order that has no
--   approach, so the job skips it on every tick, and nothing in this
--   slice can cancel or retire it: it would sit pending forever and
--   ride every save. Refusing at creation is the only point where that
--   is still cheap, which is why this is a precondition rather than
--   something the job is left to notice.
carrierCanExecute ∷ UnitId → TransferEndpoint → TransferEndpoint → Bool
carrierCanExecute uid from to =
    isJust (approachEndpointOf uid from to) ∨ bothBuildings
  where
    bothBuildings = case (from, to) of
        (EndpointBuilding _, EndpointBuilding _) → True
        _                                        → False

-- * Result encoding

-- | @{ kind = "unit"|"building", id = n }@.
pushEndpointRef ∷ TransferEndpoint → Lua.LuaE Lua.Exception ()
pushEndpointRef ep = do
    Lua.newtable
    pushTextField "kind" (transferEndpointKindId (endpointKindOf ep))
    Lua.pushinteger $ fromIntegral $ case ep of
        EndpointUnit (UnitId n)         → n
        EndpointBuilding (BuildingId n) → n
    Lua.setfield (-2) "id"

-- | One stored order as Lua reads it. Beyond the raw record it carries
--   the two things the unit job would otherwise have to re-derive (and
--   could therefore get wrong):
--
--   * @approachRole@ / @approach@ — WHICH endpoint this unit walks to
--     and where it live-sits, footprint included. Absent @approach@
--     with a present @approachRole@ is the counterpart having VANISHED,
--     which is exactly the distinction the job needs to retire the
--     order as @source_missing@ versus @receiver_missing@; an absent
--     @approachRole@ means the order is not this executor's at all.
--   * @terminal@ / @pending@ — whether there is anything left to do, by
--     'batchTerminal''s own rule rather than a Lua re-count of states.
pushOrder ∷ LiveState → TransferOrder → Lua.LuaE Lua.Exception ()
pushOrder ls order = do
    let b = troBatch order
    Lua.newtable
    Lua.pushinteger (fromIntegral (unTransferOrderId (troId order)))
    Lua.setfield (-2) "id"
    pushEndpointRef (tbSource b)
    Lua.setfield (-2) "source"
    pushEndpointRef (tbDestination b)
    Lua.setfield (-2) "destination"
    Lua.pushboolean (batchTerminal b)
    Lua.setfield (-2) "terminal"
    Lua.pushinteger (fromIntegral (batchQueuedCount b))
    Lua.setfield (-2) "queued"
    Lua.pushinteger
        (fromIntegral (length (filter (not ∘ isTerminalState ∘ qtState)
                                      (tbEntries b))))
    Lua.setfield (-2) "pending"
    Lua.pushinteger (fromIntegral (batchCompletedCount b))
    Lua.setfield (-2) "completed"
    case approachEndpoint (troUnit order) b of
        Nothing → pure ()
        Just (role, ep) → do
            pushTextField "approachRole" role
            forM_ (endpointView ls ep) $ \view → do
                pushEndpointRef ep
                let WorldPageId page = endpointPage view
                    ((ax, ay), (tw, th)) = endpointRect view
                pushTextField "page" page
                Lua.pushinteger (fromIntegral ax)
                Lua.setfield (-2) "gridX"
                Lua.pushinteger (fromIntegral ay)
                Lua.setfield (-2) "gridY"
                Lua.pushinteger (fromIntegral tw)
                Lua.setfield (-2) "tileW"
                Lua.pushinteger (fromIntegral th)
                Lua.setfield (-2) "tileH"
                Lua.pushboolean (endpointEligible view)
                Lua.setfield (-2) "eligible"
                Lua.setfield (-2) "approach"
    Lua.newtable
    forM_ (zip [1 ..] (tbEntries b)) $ \(i, q) → do
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
    Lua.setfield (-2) "entries"

-- * The five verbs

-- | @unit.createTransferOrder(uid, request)@ →
--   @{ accepted, completion, outcomes, orderId? } | nil[, err]@.
--
--   Runs the ordinary create-time check with adjacency DEFERRED (see
--   the module header), then queues the result as a durable order.
--
--   @orderId@ is present exactly when an order was actually stored, and
--   that is the caller's whole refusal test. Three outcomes, matching
--   the shape @unit.checkTransfer@ already reports:
--
--   * a WHOLE-REQUEST rejection (@empty_batch@, @duplicate_instance@) —
--     @accepted = false@, no outcomes, nothing queued;
--   * a batch where NO item passed — @accepted = true@ with one refusal
--     outcome per item, and still nothing queued, because there is
--     nothing to make the trip for ('batchHasQueued');
--   * a PARTIAL batch — the fitting entries queue in request order and
--     the create-time failures ride along as terminal entries of the
--     SAME order, so twelve items into room for eight queue eight and
--     the four refusals keep their reasons (D-1).
--
--   The acting unit is recorded ALONGSIDE the endpoint pair and is not
--   required to be either of them (D-10): a building→building order is
--   storable here exactly as #1246 designed it. It simply will not be
--   executed by the unit job — see 'approachEndpoint'.
unitCreateTransferOrderFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCreateTransferOrderFn env = do
    mUid ← readId 1
    mReq ← readRequest 2
    case (mUid, mReq) of
        (Nothing, _) → pushArgError
        (_, Nothing) → pushArgError
        (Just n, Just req) → do
            let uid = UnitId n
            mWs ← Lua.liftIO $ unitOrderStore env uid
            case mWs of
                Nothing → pushErr "unit.createTransferOrder: no such unit, \
                                  \or its world page is not loaded"
                Just _ | not (carrierCanExecute uid (trSource req)
                                                    (trDestination req)) →
                    pushErr "unit.createTransferOrder: the acting unit is \
                            \neither endpoint, so the order would have \
                            \nowhere to walk and could never run; only a \
                            \building-to-building order (D-10) may name a \
                            \carrier that is neither end"
                Just (page, ws) → do
                    ls ← Lua.liftIO $ readLiveState env
                    let scene = sceneFor ls (trSource req) (trDestination req)
                    -- The CARRIER's page, not merely the endpoints'
                    -- agreement with each other: an order is stored in
                    -- this page's store and the integrity graph scopes
                    -- its acting unit and both endpoints here. See
                    -- 'ReachPolicy'.
                    case checkBatchWith (ReachDeferred page) scene req of
                        Left e  → pushRequestError e
                        Right b
                            | not (batchHasQueued b) →
                                pushBatchResult b (checkCompletion b)
                            | otherwise → do
                                mOid ← Lua.liftIO $ atomicModifyIORef'
                                    (wsTransferOrdersRef ws) $ \orders →
                                        case addTransferOrder uid b orders of
                                            Nothing → (orders, Nothing)
                                            Just (orders', oid) →
                                                (orders', Just oid)
                                case mOid of
                                    Nothing → pushErr
                                        "unit.createTransferOrder: this page's \
                                        \transfer-order id space is exhausted"
                                    Just oid → do
                                        _ ← pushBatchResult b (checkCompletion b)
                                        Lua.pushinteger
                                            (fromIntegral
                                                (unTransferOrderId oid))
                                        Lua.setfield (-2) "orderId"
                                        return 1

-- | @unit.getTransferOrders(uid)@ → array of order tables, oldest
--   first | nil, err.
--
--   Reports EVERY order the unit carries, terminal ones included: this
--   is the only read surface, and a finished order's per-item outcomes
--   are what a caller inspects to find out what actually moved. An
--   empty array is a real answer (the unit carries none); @nil@ means
--   the unit or its page could not be resolved at all.
unitGetTransferOrdersFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetTransferOrdersFn env = do
    mUid ← readId 1
    case mUid of
        Nothing → pushArgError
        Just n  → do
            let uid = UnitId n
            mWs ← Lua.liftIO $ unitOrderStore env uid
            case mWs of
                Nothing → pushErr "unit.getTransferOrders: no such unit, \
                                  \or its world page is not loaded"
                Just (_, ws) → do
                    -- ONE live-state read for the whole reply, not one
                    -- per order: two orders naming the same counterpart
                    -- must not be able to report it in two different
                    -- places because the sim ticked between them.
                    (orders, ls) ← Lua.liftIO $ (,)
                        ⊚ readIORef (wsTransferOrdersRef ws)
                        ⊛ readLiveState env
                    Lua.newtable
                    forM_ (zip [1 ..] (ordersForUnit uid orders)) $ \(i, o) → do
                        pushOrder ls o
                        Lua.rawseti (-2) (fromIntegral (i ∷ Int))
                    return 1

-- | @unit.advanceTransferOrder(uid, orderId, state)@ → true | false |
--   nil, err.
--
--   @state@ is @"in_transit"@ or @"ready_to_commit"@ — the two
--   lifecycle steps the carrier itself performs, applied through
--   'markBatchInTransit' / 'markBatchReadyToCommit'. Both are the
--   pure store's own transitions, and both only move entries already
--   in the immediately preceding state, so calling either twice is
--   harmless and neither can resurrect a terminal entry.
--
--   No other state is settable: @completed@ is the arrival commit's to
--   write, @failed@ is 'unitFailTransferOrderFn''s, and @cancelled@ is
--   'unitCancelTransferOrderFn''s. @false@ means the id names no order
--   this unit is carrying.
unitAdvanceTransferOrderFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitAdvanceTransferOrderFn env = do
    mUid   ← readId 1
    mOid   ← readId 2
    mState ← readText 3
    case (mUid, mOid, mState) of
        (Just u, Just o, Just st) → case transition st of
            Nothing → pushErr $ "unit.advanceTransferOrder: unknown state '"
                                <> st <> "' (expected in_transit or \
                                          \ready_to_commit)"
            Just f  → do
                r ← Lua.liftIO $ withOwnedOrder env (UnitId u)
                                    (TransferOrderId o) (const f)
                pushBool (isJust r)
        _ → pushArgError
  where
    transition "in_transit"      = Just markBatchInTransit
    transition "ready_to_commit" = Just markBatchReadyToCommit
    transition _                 = Nothing

-- | @unit.commitTransferOrder(uid, orderId)@ →
--   @{ accepted = true, completion, outcomes } | nil, err@.
--
--   The moment of truth. ONLY entries sitting at @ready_to_commit@ are
--   submitted, each through the same atomic per-endpoint-pair path
--   @unit.commitTransfer@ uses, in request order, re-reading live state
--   every time — so one item's refusal neither rolls back a successful
--   sibling nor stops a later one, and no item ever half-moves.
--
--   The returned outcomes are read back OUT OF THE STORE after the
--   write, not from the in-flight fold, so what the caller sees is
--   exactly what was recorded.
--
--   Calling it on an order with nothing ready moves nothing and reports
--   the batch as it stands — which is what makes a duplicate call inert
--   rather than a second transfer.
unitCommitTransferOrderFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCommitTransferOrderFn env = do
    mUid ← readId 1
    mOid ← readId 2
    case (mUid, mOid) of
        (Just u, Just o) → do
            let uid = UnitId u
                oid = TransferOrderId o
            mWs ← Lua.liftIO $ unitOrderStore env uid
            case mWs of
                Nothing → pushErr "unit.commitTransferOrder: no such unit, \
                                  \or its world page is not loaded"
                Just (_, ws) → do
                    mOrder ← Lua.liftIO $ do
                        orders ← readIORef (wsTransferOrdersRef ws)
                        pure $ case lookupTransferOrder oid orders of
                            Just order | troUnit order ≡ uid → Just order
                            _ → Nothing
                    case mOrder of
                        Nothing → pushErr "unit.commitTransferOrder: no such \
                                          \order for that unit"
                        Just order → do
                            outs ← Lua.liftIO $ commitReadyEntries env
                                                    (troBatch order)
                            mFinal ← Lua.liftIO $ withOwnedOrder env uid oid
                                        (const (applyOutcomes outs))
                            case mFinal of
                                Nothing → pushErr "unit.commitTransferOrder: \
                                                  \the order vanished mid-commit"
                                Just final → do
                                    let b = troBatch final
                                    pushBatchResult b (commitCompletion b)
        _ → pushArgError

-- | Submit every @ready_to_commit@ entry, in request order, and pair
--   each result with the INDEX it came from — positions, not instance
--   ids, because the reconciliation below must not depend on ids being
--   unique across a batch a caller hand-built.
commitReadyEntries ∷ EngineEnv → TransferBatch → IO [(Int, TransferState)]
commitReadyEntries env b =
    fmap concat $ forM (zip [0 ..] (tbEntries b)) $ \(i, q) →
        if qtState q ≢ TransferReadyToCommit then pure [] else do
            r ← commitOneLive env (tbSource b) (tbDestination b) (qtItem q)
            pure [ (i, case r of
                        Right _ → TransferCompleted
                        Left f  → TransferFailed (staleOf f)) ]

-- | An arrival refusal, as the stored entry records it.
--
--   The entry passed its create-time checks and the order arrived, so
--   whatever went wrong is the world having moved underneath it —
--   @became_stale@ with the real precondition as its CAUSE, which is
--   the same shape 'Unit.Transfer.commitBatch' writes. When
--   'commitOneLive' already reported a stale failure (its cross-manager
--   paths do, for a source torn down mid-transaction) the failure is
--   passed through untouched: re-wrapping it would bury the cause under
--   a second @became_stale@ and lose the only part that says what broke.
staleOf ∷ TransferFailure → TransferFailure
staleOf f = case tfReason f of
    ReasonBecameStale → f
    r                 → staleFailure r

-- | Write arrival outcomes back onto the stored entries by position.
--
--   The @ready_to_commit@ guard is what makes this safe against a
--   concurrent transition landing between the commit and the write:
--   only an entry still in the state it was committed FROM is
--   overwritten, so a retirement that got there first stands.
applyOutcomes ∷ [(Int, TransferState)] → TransferBatch → TransferBatch
applyOutcomes outs b = b { tbEntries = zipWith step [0 ..] (tbEntries b) }
  where
    step ∷ Int → QueuedTransfer → QueuedTransfer
    step i q = case lookup i outs of
        Just st | qtState q ≡ TransferReadyToCommit → q { qtState = st }
        _ → q

-- | @unit.failTransferOrder(uid, orderId, reason [, cause])@ → true |
--   false | nil, err.
--
--   Self-termination: every PENDING entry becomes @failed@ with the
--   given reason, terminal entries are untouched, and NOTHING moves.
--   This is the baseline durable bookkeeping for the two ways an order
--   provably cannot finish — the carrier stalled short of a counterpart
--   that is still there (@out_of_range@), and a counterpart that
--   stopped existing (@became_stale@ with @source_missing@ /
--   @receiver_missing@ as the cause). Somebody CHOOSING to abandon a
--   still-runnable order is 'unitCancelTransferOrderFn' instead, and
--   what happens to a terminal order afterwards is
--   'unitPruneTransferOrderFn'.
--
--   @reason@ and @cause@ are the contract's own ids
--   ('transferReasonFromId'), so this adds no vocabulary and the caller
--   names a refusal exactly as every other surface reports one. An
--   unrecognised id is an error rather than a silent default: guessing
--   would write a wrong explanation into durable state.
unitFailTransferOrderFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitFailTransferOrderFn env = do
    mUid    ← readId 1
    mOid    ← readId 2
    mReason ← readText 3
    causeTy ← Lua.ltype 4
    mCause  ← if causeTy ≡ Lua.TypeNone ∨ causeTy ≡ Lua.TypeNil
                then pure (Just Nothing)
                else fmap Just ⊚ readText 4
    case (mUid, mOid, mReason, mCause) of
        (Just u, Just o, Just reasonId, Just mCauseId) →
            case (transferReasonFromId reasonId
                 , traverse transferReasonFromId mCauseId) of
                (Just reason, Just cause) → do
                    let failure = TransferFailure { tfReason = reason
                                                  , tfCause  = cause }
                    r ← Lua.liftIO $ withOwnedOrder env (UnitId u)
                            (TransferOrderId o)
                            (const (failPendingBatch failure))
                    pushBool (isJust r)
                _ → pushErr $ "unit.failTransferOrder: unknown transfer \
                              \reason in (" <> reasonId <> ", "
                              <> fromMaybe "nil" mCauseId <> ")"
        _ → pushArgError

-- | @unit.cancelTransferOrder(uid, orderId)@ → true | false | nil, err.
--
--   The player abandoned an order that could still have run (#1253,
--   requirement 1): every PENDING entry becomes @cancelled@, already
--   terminal entries are left exactly as they are, and NOTHING moves.
--   'Unit.Transfer.cancelBatch' is the whole transition — pending-only
--   is what makes a cancel arriving after a partial commit record the
--   truth (six delivered, six abandoned) instead of overwriting six
--   real deliveries with an intent nobody had.
--
--   Deliberately a SEPARATE verb from 'unitFailTransferOrderFn' rather
--   than a @cancelled@ spelling accepted by
--   'unitAdvanceTransferOrderFn', because the distinction is who
--   decided: @failed@ carries a structured reason the order could not
--   finish, and a cancellation has none to carry. Neither could report
--   the other honestly.
--
--   Cancelling does NOT remove the order — 'unitPruneTransferOrderFn'
--   does, once the caller has surfaced the outcome. @false@ means the
--   id names no order this unit is carrying, which is also what a
--   second cancel of an order already pruned reports.
unitCancelTransferOrderFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCancelTransferOrderFn env = do
    mUid ← readId 1
    mOid ← readId 2
    case (mUid, mOid) of
        (Just u, Just o) → do
            r ← Lua.liftIO $ withOwnedOrder env (UnitId u) (TransferOrderId o)
                                (const cancelBatch)
            pushBool (isJust r)
        _ → pushArgError

-- | @unit.pruneTransferOrder(uid, orderId)@ → true | false | nil, err.
--
--   Drop a finished order from the store (#1253, requirement 5). The
--   store holds LIVE work only, like 'Craft.Bills': once an outcome has
--   been surfaced to the player the entries have no reader left, and
--   leaving them means a completed haul rides every later save while a
--   failed one keeps reporting its demolished endpoint to
--   "World.Save.Integrity" forever.
--
--   __Only a TERMINAL order prunes__ ('Unit.Transfer.batchTerminal'),
--   and that guard is the safety property: pruning a pending order
--   would silently abandon work the player asked for, with no
--   @cancelled@ entry and no event to say so. A caller that means to
--   stop a live order calls 'unitCancelTransferOrderFn' first, which is
--   exactly what makes the abandonment visible.
--
--   Idempotent by construction: @false@ covers an unknown id, another
--   unit's order, a still-pending one, and an order this call already
--   removed — so a repeated tick cannot double-prune, and the caller
--   needs no record of whether it has run.
unitPruneTransferOrderFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitPruneTransferOrderFn env = do
    mUid ← readId 1
    mOid ← readId 2
    case (mUid, mOid) of
        (Just u, Just o) → do
            let uid = UnitId u
                oid = TransferOrderId o
            mWs ← Lua.liftIO $ unitOrderStore env uid
            case mWs of
                Nothing → pushErr "unit.pruneTransferOrder: no such unit, \
                                  \or its world page is not loaded"
                Just (_, ws) → do
                    -- Lookup, terminality test and delete in ONE
                    -- atomicModifyIORef', so a transition landing
                    -- between them cannot make this remove an order
                    -- that had just become live again.
                    done ← Lua.liftIO $ atomicModifyIORef'
                        (wsTransferOrdersRef ws) $ \orders →
                            case lookupTransferOrder oid orders of
                                Just order
                                    | troUnit order ≡ uid
                                    , batchTerminal (troBatch order) →
                                        (fst (removeTransferOrder oid orders)
                                        , True)
                                _ → (orders, False)
                    pushBool done
        _ → pushArgError
