{-# LANGUAGE Strict #-}
-- | The ONE ordered way to go from an ENTITY to the live 'WorldState'
--   of the page it stands on (#2476).
--
--   == Why the order matters
--
--   A 'World.Page.Types.WorldPageId' is a reusable NAME. A page
--   lifecycle transition — a single-page @world.destroy@, or either
--   init path replacing a registered id — retires that incarnation's
--   entities and then registers the replacement, in that order and
--   inside one critical section
--   ('Engine.Core.State.pageLifecycleLock'). The order is the contract
--   this module is built on: __no old row is ever present while the
--   replacement is registered__.
--
--   A resolver that reads the ENTITY first and the page second gets no
--   benefit from that. Its two reads can straddle the transition — the
--   entity from before it, the page from after — and it would then hand
--   its caller a departed incarnation's unit paired with the
--   replacement's state, into which the caller writes a durable row
--   (a ground item, a transfer order, a container observation, a
--   construction receipt) that outlives the entity naming it.
--
--   Reading the PAGE SET first inverts that:
--
--     * the snapshot holds the OLD page — then whatever entity is found
--       belongs with it, and any write lands in the state that is
--       leaving;
--     * the snapshot holds the REPLACEMENT — then the entity read
--       happened after the publish, which happened after the
--       retirement, so a departed incarnation's row is already gone and
--       resolution fails.
--
--   Either way the pair is consistent. There is no third case, because
--   the transition never publishes before it retires.
--
--   == Why the snapshot is taken with 'atomicModifyIORef''
--
--   Two plain 'readIORef's are two plain loads, and the CPU this runs on
--   may reorder them — so "page first" would be an intention rather
--   than a guarantee. Taking the page snapshot through
--   'atomicModifyIORef'' (a compare-and-swap that returns the value
--   unchanged) makes it a real ordering point, so the entity read
--   cannot be hoisted above it. The manager is read by low-frequency
--   scripting verbs here, not on a per-tick path.
module World.Page.Resolve
    ( snapshotWorlds
    , resolveUnitPage
    , resolveBuildingPage
    , resolveBuildingPageWith
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Building.Types (BuildingId, BuildingInstance(..), BuildingManager(..))
import Unit.Types (UnitId, UnitInstance(..), UnitManager(..))
import World.Page.Types (WorldPageId)
import World.State.Types (WorldManager(..), WorldState)

-- | The page set, read as an ORDERING POINT rather than a plain load.
--   Every resolution below starts here, and nothing that follows may be
--   observed to have happened before it.
snapshotWorlds ∷ IORef WorldManager → IO WorldManager
snapshotWorlds ref = atomicModifyIORef' ref (\mgr → (mgr, mgr))

-- | The live page a UNIT stands on, or 'Nothing' when the unit does not
--   exist, its page is not registered, or the unit belongs to an
--   incarnation the registered page has replaced.
--
--   That third case needs no comparison of its own: it IS the second,
--   because the transition retires before it registers and this reads
--   the page set first.
resolveUnitPage ∷ IORef WorldManager → IORef UnitManager → UnitId
                → IO (Maybe (WorldPageId, WorldState))
resolveUnitPage worldsRef unitsRef uid = do
    mgr ← snapshotWorlds worldsRef
    um  ← readIORef unitsRef
    pure $ do
        inst ← HM.lookup uid (umInstances um)
        ws   ← lookup (uiPage inst) (wmWorlds mgr)
        pure (uiPage inst, ws)

-- | The building half, with the identical ordering and the identical
--   guarantee.
resolveBuildingPage ∷ IORef WorldManager → IORef BuildingManager → BuildingId
                    → IO (Maybe (WorldPageId, WorldState, BuildingInstance))
resolveBuildingPage = resolveBuildingPageWith (pure ())

-- | 'resolveBuildingPage' with a seam BETWEEN its two reads.
--
--   The ordering this module exists for is invisible from outside: both
--   orders answer identically unless a page lifecycle transition lands
--   between the reads. So a test lands one there, through this hook,
--   and proves the resolution refuses rather than pairing a departed
--   incarnation's building with the replacement's state. Production
--   passes @pure ()@ and the two reads stay adjacent.
resolveBuildingPageWith
    ∷ IO () → IORef WorldManager → IORef BuildingManager → BuildingId
    → IO (Maybe (WorldPageId, WorldState, BuildingInstance))
resolveBuildingPageWith betweenReads worldsRef buildingsRef bid = do
    mgr ← snapshotWorlds worldsRef
    betweenReads
    bm  ← readIORef buildingsRef
    pure $ do
        inst ← HM.lookup bid (bmInstances bm)
        ws   ← lookup (biPage inst) (wmWorlds mgr)
        pure (biPage inst, ws, inst)
