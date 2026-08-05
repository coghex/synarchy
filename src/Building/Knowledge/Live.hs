{-# LANGUAGE Strict #-}
-- | The IO glue that drives "Building.Knowledge" from the real reveal
--   triggers (#1087, epic #1013 phase A3). Kept OUT of
--   "Building.Knowledge" so that module stays pure (and out of
--   "World.State.Types"' import graph, which would otherwise cycle
--   through the capability records this one projects from).
--
--   Every reveal below takes a 'ContainerObserver' — the five live
--   containers a reveal actually needs, read out of the relevant
--   capability records by 'containerObserver'. That is the same shape
--   "Unit.Transfer"'s @LiveState@ already uses, and it has two payoffs:
--   no consumer of this module gains unrestricted
--   'Engine.Core.State.EngineEnv' access
--   (@tools/engine_env_capability_audit.py@), and every trigger below is
--   exercisable from hspec against five freshly-made 'IORef's with no
--   engine boot at all. The two DEMOLITION helpers take the
--   world-manager ref alone, since that is genuinely all they touch.
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
--     * a Mode A session opening on the container (#1013 C3 calls
--       'revealContainer' through @building.refreshContainerKnowledge@;
--       A3 only provides the verb);
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
    , forgetContainerEverywhere
    , forgetAllContainers
    , readContainerKnowledge
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.Capability.Building (BuildingCapability(..))
import Engine.Core.Capability.ContentRegistries (ContentRegistriesCapability(..))
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..))
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import Building.Knowledge
import Building.Types
    (BuildingId(..), BuildingInstance(..), BuildingManager(..))
import Item.Types (ItemInstance, ItemManager)
import Unit.Faction (isPlayerCommandable)
import Unit.Types (UnitId, UnitInstance(..), UnitManager(..))
import World.State.Types (WorldManager(..), WorldState(..))

-- | The live containers a container-knowledge reveal reads or writes,
--   and nothing else. Deliberately narrower than the capability records
--   it is projected from.
data ContainerObserver = ContainerObserver
    { coBuildings ∷ !(IORef BuildingManager)
    , coUnits     ∷ !(IORef UnitManager)
    , coWorlds    ∷ !(IORef WorldManager)
      -- ^ How a container's own page is resolved to the 'WorldState'
      --   that owns its knowledge record.
    , coItems     ∷ !(IORef ItemManager)
      -- ^ Only for weighing an observation ('itemTotalWeight' needs the
      --   def's per-unit fill weight).
    , coGameTime  ∷ !(IORef Double)
      -- ^ The clock a reveal is stamped with — game time, not calendar.
    }

containerObserver
    ∷ BuildingCapability → UnitCombatCapability → WorldSimCapability
    → ContentRegistriesCapability → ContainerObserver
containerObserver bld uc sim reg = ContainerObserver
    { coBuildings = bcBuildingManagerRef bld
    , coUnits     = ucUnitManagerRef uc
    , coWorlds    = wsWorldManagerRef sim
    , coItems     = crItemManagerRef reg
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
            itemMgr ← readIORef (coItems co)
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
revealContainerForUnit
    ∷ ContainerObserver → UnitId → BuildingId → IO Bool
revealContainerForUnit co uid bid = do
    um ← readIORef (coUnits co)
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
            itemMgr ← readIORef (coItems co)
            now     ← readIORef (coGameTime co)
            atomicModifyIORef' (wsContainerKnowledgeRef ws) $ \k →
                case lookupContainer bid k of
                    Just _  → (k, False)
                    Nothing → (recordObservation itemMgr now bid storage k, True)

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
    forM_ (wmWorlds wm) $ \(_, ws) →
        atomicModifyIORef' (wsContainerKnowledgeRef ws) $ \k →
            (forgetContainer bid k, ())

-- | Every page forgets every container. The clear-all counterpart of
--   'forgetContainerEverywhere' (queue-ordered teardown, #58): with no
--   buildings left, every record is by definition dangling, and a
--   record with no container has no surface that could ever clear it.
forgetAllContainers ∷ IORef WorldManager → IO ()
forgetAllContainers worldsRef = do
    wm ← readIORef worldsRef
    forM_ (wmWorlds wm) $ \(_, ws) →
        atomicModifyIORef' (wsContainerKnowledgeRef ws) $ \_ →
            (emptyContainerKnowledge, ())

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
