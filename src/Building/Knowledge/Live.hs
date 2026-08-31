{-# LANGUAGE Strict #-}
-- | The IO glue that drives "Building.Knowledge" from the real reveal
--   triggers (#1087, epic #1013 phase A3). Kept OUT of
--   "Building.Knowledge" so that module stays pure (and out of
--   "World.State.Types"' import graph, which would otherwise cycle
--   through the capability records this one projects from).
--
--   Every reveal below takes a 'ContainerObserver' — the four live
--   containers a reveal actually needs, read out of the relevant
--   capability records by 'containerObserver'. That is the same shape
--   "Unit.Transfer"'s @LiveState@ already uses, and it has two payoffs:
--   no consumer of this module gains unrestricted
--   'Engine.Core.State.EngineEnv' access
--   (@tools/engine_env_capability_audit.py@), and every trigger below is
--   exercisable from hspec against four freshly-made handles with no
--   engine boot at all. The unit-gated reveal takes the unit roster as
--   its own argument, and the two DEMOLITION helpers take the
--   world-manager ref alone — each asking for exactly what it touches.
--
--   __This record is the mutation-authority arc's production pass-on__
--   (#1896, CMA-2 of epic #1890). 'coItems' is an
--   'Engine.Core.ReadOnlyRef.ReadOnlyRef', because this module is one
--   of the item registry's 26 read-only consumers and must not be
--   handed a writable handle merely so it can weigh an observation.
--   That is why 'containerObserver' takes the reader-facing
--   'Engine.Core.Capability.ContentRegistriesView.ContentRegistriesViewCapability'
--   rather than the raw writer record and wrapping here itself — the
--   boundary has to travel INTO this record, which is exactly the case
--   a record-level boundary misses. The other three fields are outside
--   that pilot and stay raw 'IORef's.
--
--   __What reveals contents__ (epic decision 2: an INTERACTION, never
--   proximity). Every one of them lands here:
--
--     * a completed transfer commit into the container's storage
--       ('Engine.Scripting.Lua.API.Units.Transfer'\'s @unit.commitTransfer@ —
--       and, whenever #1085 generalizes that contract to both
--       directions, its withdraw half through the same call);
--     * the lax AI cargo verbs
--       ('Engine.Scripting.Lua.API.Units.Cargo'\'s @unit.depositToCargo@ /
--       @unit.withdrawFromCargo@), so the AI's own hauling keeps the
--       player's picture current;
--     * a Mode A session opening on the container (#1250's
--       @scripts\/transfer_session.lua@ calls 'revealContainer' through
--       @building.refreshContainerKnowledge@, exactly once per session,
--       on the transition to its open\/held state);
--     * the first completion of a storage-capable building
--       ('seedBuiltContainer') — the player watched it go up, so it
--       seeds KNOWN-EMPTY rather than never-inspected.
--
--   Explicitly NOT a reveal: walking past, selecting, right-clicking to
--   open the panel from a distance, and @unit.transferItemToBuilding@ —
--   that writes 'Building.Types.biMaterialsDelivered', locked
--   construction stock in a different compartment, not storage.
--
--   __Only a fully successful mutation reveals.__ Every call site fires
--   AFTER its transaction has committed, and the record snapshots the
--   container's FINAL post-commit storage with a fresh read — a failed
--   validation, a failed destination update, or a pop-then-rollback
--   leaves knowledge untouched, because the call never happens.
--
--   __One faction gate.__ Every unit-driven reveal goes through
--   'revealContainerForUnit', which asks 'Unit.Faction.isPlayerCommandable'
--   (canonically player + debug) — never a duplicated faction-tag
--   comparison at a call site.
module Building.Knowledge.Live
    ( ContainerObserver(..)
    , containerObserver
    , revealContainer
    , revealContainerForUnit
    , seedBuiltContainer
    , markPendingSeed
    , sweepPendingSeeds
    , forgetContainerEverywhere
    , forgetAllContainers
    , readContainerKnowledge
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.Capability.Building (BuildingCapability(..))
import Engine.Core.Capability.ContentRegistriesView (ContentRegistriesViewCapability(..))
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import Engine.Core.ReadOnlyRef (ReadOnlyRef, readReadOnlyRef)
import Building.Knowledge
import Building.Types
    ( BuildingActivity(..), BuildingId(..)
    , BuildingInstance(..), BuildingManager(..), currentActivity )
import Item.Types (ItemInstance, ItemManager)
import Unit.Faction (isPlayerCommandable)
import Unit.Types (UnitId, UnitInstance(..), UnitManager(..))
import World.State.Types (WorldManager(..), WorldState(..))

-- | The live containers a container-knowledge reveal reads or writes,
--   and nothing else. Deliberately narrower than the capability records
--   it is projected from.
data ContainerObserver = ContainerObserver
    { coBuildings ∷ !(IORef BuildingManager)
    , coWorlds    ∷ !(IORef WorldManager)
      -- ^ How a container's own page is resolved to the 'WorldState'
      --   that owns its knowledge record.
    , coItems     ∷ !(ReadOnlyRef ItemManager)
      -- ^ Only for weighing an observation ('itemTotalWeight' needs the
      --   def's per-unit fill weight) — so it arrives WRAPPED (#1896).
      --   This is the arc's production pass-on: the read-only boundary
      --   travels with the handle into this record, where a
      --   record-level one would have ended the moment
      --   'Engine.Core.Capability.ContentRegistriesView' was unpacked.
      --   An attempted write here does not typecheck; see
      --   @tools\/test_read_only_ref_compile.py@.
    , coGameTime  ∷ !(IORef Double)
      -- ^ The clock a reveal is stamped with — game time, not calendar.
    }

containerObserver
    ∷ BuildingCapability → WorldSimCapability → ContentRegistriesViewCapability
    → ContainerObserver
containerObserver bld sim reg = ContainerObserver
    { coBuildings = bcBuildingManagerRef bld
    , coWorlds    = wsWorldManagerRef sim
    , coItems     = crvItemManagerRef reg
    , coGameTime  = wsGameTimeRef sim
    }

-- | The state that owns a container's knowledge record, plus the
--   container's storage as of the same read. The knowledge owner is
--   page-scoped, so every reveal resolves the BUILDING's own 'biPage'
--   rather than whichever page happens to be active — a deposit into a
--   cargo hold on a hidden page must still update that page's memory,
--   not the visible one's.
containerPage
    ∷ ContainerObserver → BuildingId → IO (Maybe (WorldState, [ItemInstance]))
containerPage co bid = do
    bm ← readIORef (coBuildings co)
    case HM.lookup bid (bmInstances bm) of
        Nothing   → pure Nothing
        Just inst → do
            wm ← readIORef (coWorlds co)
            pure $ do
                ws ← lookup (biPage inst) (wmWorlds wm)
                pure (ws, biStorage inst)

-- | Take a fresh observation of @bid@'s storage and REPLACE its record.
--   Returns 'False' only when the container (or its page) is gone.
--
--   The storage list is read here, after the caller's own transaction
--   has already committed, so what lands in the record is the FINAL
--   post-commit contents rather than a value captured mid-move.
revealContainer ∷ ContainerObserver → BuildingId → IO Bool
revealContainer co bid = do
    mPage ← containerPage co bid
    case mPage of
        Nothing → pure False
        Just (ws, storage) → do
            itemMgr ← readReadOnlyRef (coItems co)
            now     ← readIORef (coGameTime co)
            atomicModifyIORef' (wsContainerKnowledgeRef ws) $ \k →
                (recordObservation itemMgr now bid storage k, ())
            pure True

-- | 'revealContainer', gated on the ACTING unit being player-commandable.
--   A wildlife or hostile unit rummaging in a container teaches the
--   player nothing, so its interaction leaves the record exactly as it
--   was — which is what lets a stale record stay stale even while the
--   contents change underneath it.
--
--   Returns 'True' only when a reveal actually happened.
--   Takes the unit-manager ref as a SEPARATE argument rather than a
--   fifth 'ContainerObserver' field: it is the only reveal with an
--   acting unit at all, and the seeding trigger in
--   "Building.Thread.Command" genuinely has no unit roster to hand.
revealContainerForUnit
    ∷ ContainerObserver → IORef UnitManager → UnitId → BuildingId → IO Bool
revealContainerForUnit co unitsRef uid bid = do
    um ← readIORef unitsRef
    let commandable = maybe False (isPlayerCommandable ∘ uiFactionId)
                            (HM.lookup uid (umInstances um))
    if commandable then revealContainer co bid else pure False

-- | Seed a just-completed storage building as KNOWN-EMPTY (requirement
--   2). Called at the FIRST transition to Built, never at spawn: a
--   worker-built building is created at zero progress and only becomes
--   Built when 'Building.Types.currentActivity' says so, and a save
--   restoring an already-built container must not masquerade as a new
--   construction event.
--
--   No-ops when a record already exists, so a re-crossing of the
--   completion threshold (or any later call) can never overwrite a real
--   observation with an empty one. Snapshots the container's ACTUAL
--   storage rather than assuming @[]@, so the seeded record can never
--   disagree with what is really in there.
seedBuiltContainer ∷ ContainerObserver → BuildingId → IO Bool
seedBuiltContainer co bid = do
    mPage ← containerPage co bid
    case mPage of
        Nothing → pure False
        Just (ws, storage) → do
            itemMgr ← readReadOnlyRef (coItems co)
            now     ← readIORef (coGameTime co)
            atomicModifyIORef' (wsContainerKnowledgeRef ws) $ \k →
                case lookupContainer bid k of
                    Just _  → (k, False)
                    Nothing → (recordObservation itemMgr now bid storage k, True)

-- | Remember that THIS SESSION just placed an instant-built container
--   ('Building.Knowledge.SeedWhenBuilt') that has not reached Built
--   yet, so 'sweepPendingSeeds' can watch for the transition. No-ops
--   when the building or its page is gone.
markPendingSeed ∷ ContainerObserver → BuildingId → IO ()
markPendingSeed co bid = do
    mPage ← containerPage co bid
    forM_ mPage $ \(ws, _) →
        atomicModifyIORef' (wsPendingContainerSeedsRef ws) $ \pending →
            (HS.insert bid pending, ())

-- | Seed every pending container whose Built transition has now
--   actually happened, on every live page. Driven once per building
--   drain (i.e. per unit tick).
--
--   Re-evaluates 'currentActivity' rather than storing a precomputed
--   deadline, so this can never disagree with the one function that
--   decides whether a building is Built. An entry whose building has
--   vanished is dropped; one still 'Appearing' is kept for next tick.
--
--   The set it walks is exactly "containers this session placed and
--   has not finished watching" — never every already-built container —
--   which is what stops a LOADED one from being seeded as though the
--   player had just watched it go up.
sweepPendingSeeds ∷ ContainerObserver → IO ()
sweepPendingSeeds co = do
    wm  ← readIORef (coWorlds co)
    bm  ← readIORef (coBuildings co)
    now ← readIORef (coGameTime co)
    forM_ (wmWorlds wm) $ \(_, ws) → do
        pending ← readIORef (wsPendingContainerSeedsRef ws)
        unless (HS.null pending) $ do
            let verdict bid = do
                    inst ← HM.lookup bid (bmInstances bm)
                    def  ← HM.lookup (biDefName inst) (bmDefs bm)
                    pure (currentActivity now inst def ≡ Built)
                classify bid = case verdict bid of
                    Nothing    → (False, False)  -- gone: drop, don't seed
                    Just True  → (False, True)   -- Built: seed, then drop
                    Just False → (True,  False)  -- still Appearing: keep
                results = [ (bid, classify bid) | bid ← HS.toList pending ]
                keep    = HS.fromList [ b | (b, (k, _)) ← results, k ]
                ready   = [ b | (b, (_, r)) ← results, r ]
            forM_ ready (seedBuiltContainer co)
            atomicModifyIORef' (wsPendingContainerSeedsRef ws) $ \live →
                (HS.intersection live keep, ())

-- | Demolition: drop this container's record. Applied to EVERY live
--   page rather than the building's own, because the caller
--   ('Building.Thread.Command') has already removed the instance by the
--   time this runs — and because a 'BuildingId' comes from one
--   session-global allocator, so it can name a container on at most one
--   page anyway. Page-correct by construction, with no dependence on
--   read\/delete ordering.
--   Takes the world-manager ref alone rather than a whole
--   'ContainerObserver': demolition has no items to weigh, no clock to
--   stamp and no acting unit, so asking for those would be four
--   containers this can't use.
forgetContainerEverywhere ∷ IORef WorldManager → BuildingId → IO ()
forgetContainerEverywhere worldsRef bid = do
    wm ← readIORef worldsRef
    forM_ (wmWorlds wm) $ \(_, ws) → do
        atomicModifyIORef' (wsContainerKnowledgeRef ws) $ \k →
            (forgetContainer bid k, ())
        -- A container demolished mid-appear must also stop being
        -- watched, or the sweep would carry a dead id forever.
        atomicModifyIORef' (wsPendingContainerSeedsRef ws) $ \pending →
            (HS.delete bid pending, ())

-- | Every page forgets every container. The clear-all counterpart of
--   'forgetContainerEverywhere' (queue-ordered teardown, #58): with no
--   buildings left, every record is by definition dangling, and a
--   record with no container has no surface that could ever clear it.
forgetAllContainers ∷ IORef WorldManager → IO ()
forgetAllContainers worldsRef = do
    wm ← readIORef worldsRef
    forM_ (wmWorlds wm) $ \(_, ws) → do
        atomicModifyIORef' (wsContainerKnowledgeRef ws) $ \_ →
            (emptyContainerKnowledge, ())
        atomicModifyIORef' (wsPendingContainerSeedsRef ws) $ \_ →
            (HS.empty, ())

-- | The record the player currently holds for @bid@, resolved through
--   the container's OWN page. 'Nothing' distinguishes "no such
--   container (or page)" from a container that resolves but has never
--   been inspected — the latter comes back as @Just Nothing@.
readContainerKnowledge
    ∷ ContainerObserver → BuildingId → IO (Maybe (Maybe ContainerRecord))
readContainerKnowledge co bid = do
    mPage ← containerPage co bid
    case mPage of
        Nothing → pure Nothing
        Just (ws, _) → do
            k ← readIORef (wsContainerKnowledgeRef ws)
            pure (Just (lookupContainer bid k))
