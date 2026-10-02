-- | The legacy solver adapter: today's production fluid solver,
--   'Sim.Fluid.Active.simulateActiveTick', run unmodified (#2719).
--
--   Nothing here copies or instruments the algorithm. The adapter seeds
--   a 'SimWorldState' through the same 'Sim.Chunk.loadedChunkState' and
--   'Sim.Chunk.activateChunk' a loaded page uses, advances it ONE real
--   tick per harness step, and reads the exact quantities back.
--
--   Representation limits are refused, never rounded or saturated:
--   the sim stores whole-z terrain ('Sim.State.Types.scsTerrain') and a
--   'Word16' active volume ('Sim.Fluid.Types.afcVolume'), so a fixture
--   with fractional terrain or a quantity past 65535 is an error here.
--
--   Quantities are read from whichever representation is authoritative
--   for each chunk, never both: the active volume grid while the chunk
--   simulates, and the passive exact plane once equilibrium deactivation
--   has baked it back ('Sim.Fluid.Active' deactivates after 200
--   unchanged ticks).
--
--   The solver records no per-face transfers, so every step reports
--   'FaceRecordsUnavailable'.
--
--   Edits are the harness's own fixture operations, applied directly to
--   the seeded cells — not the production edit path, which reseeds from
--   published tiles. An edit wakes the edited chunk and its physically
--   cardinal neighbours, resolved through the page's seam topology, but
--   only those the fixture declared resident-active: a deliberately
--   inactive neighbour stays inactive whatever happens beside it.
module RiverRuntime.Harness.Legacy
    ( LegacyState(..)
    , legacySolver
    , legacyAdapter
    , legacyInterval
    , legacyActiveChunks
    ) where

import UPrelude
import Control.Monad (foldM)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Exact (fluidUnitsPerZ)
import World.Fluid.Types (FluidCell(..), fluidVolumeOverTerrain)
import Sim.Chunk (activateChunk, loadedChunkState)
import Sim.Fluid.Active (simulateActiveTick)
import Sim.Fluid.Types (ActiveFluidCell(..))
import Sim.State.Types (SimWorldState(..), SimChunkState(..), emptySimState
                       , SimState(..), emptySimWorldState)
import Sim.Topology (simCardinalNeighbors)
import RiverRuntime.Harness.Adapter
import RiverRuntime.Harness.Fixture
import RiverRuntime.Harness.Placement

data LegacyState = LegacyState
    { lsWorld          ∷ SimWorldState
    , lsDeclaredActive ∷ HS.HashSet ChunkCoord
      -- ^ the stored chunks the fixture declared resident-active: the
      --   only ones an edit may wake
    }

-- | One production tick at the sim thread's default rate
--   ('Sim.State.Types.ssTickRate'), used as a LABEL. The adapter never
--   sleeps and never reads a clock.
legacyInterval ∷ LogicalTime
legacyInterval = LogicalTime (ssTickRate emptySimState)

legacyAdapter ∷ Adapter
legacyAdapter = Adapter legacySolver

legacySolver ∷ SolverAdapter LegacyState
legacySolver = SolverAdapter
    { saName      = "legacy"
    , saConfig    =
        [ ("solver", "Sim.Fluid.Active.simulateActiveTick")
        , ("ticks_per_step", "1")
        , ("face_records", "unavailable")
        ]
    , saIntervals = [legacyInterval]
    -- 'saIntervals' admits only 'legacyInterval': one real tick IS the
    -- step, so there is no other interval to bind.
    , saInit      = \_ → legacyInit
    , saStep      = \s → ( s { lsWorld = simulateActiveTick (lsWorld s) }
                         , FaceRecordsUnavailable )
    , saEdit      = legacyEdit
    , saObserve   = legacyObserve
    }

-- | Whether each stored chunk is currently simulating, in key order.
legacyActiveChunks ∷ LegacyState → [(ChunkCoord, Bool)]
legacyActiveChunks s =
    M.toList (M.fromList [ (cc, scsActive scs)
                         | (cc, scs) ← HM.toList (swsChunks (lsWorld s)) ])

-- | Whole-z terrain for an exact elevation, or a refusal.
wholeZ ∷ Elevation → Either Text Int
wholeZ (Elevation e)
    | e `mod` fluidUnitsPerZ ≡ 0 = Right (e `div` fluidUnitsPerZ)
    | otherwise = Left ("legacy adapter: terrain elevation " <> tshow e
                        <> " is not a whole z-level")

-- | An active volume for an exact quantity, or a refusal.
activeVolume ∷ Quantity → Either Text Word16
activeVolume (Quantity q)
    | q < 0 = Left ("legacy adapter: negative quantity " <> tshow q)
    | q > fromIntegral (maxBound ∷ Word16) =
        Left ("legacy adapter: quantity " <> tshow q <> " exceeds Word16 capacity")
    | otherwise = Right (fromIntegral q)

legacyInit ∷ PlacementMap → Fixture → Either Text LegacyState
legacyInit pm fx = do
    -- Every elevation the schedule will ever set must be representable,
    -- so a run cannot fail halfway for a reason visible at the start.
    forM_ [ e | Scheduled _ (SetTerrain _ e _) ← fxSchedule fx ] wholeZ
    chunks ← forM (M.toList (pmChunks pm)) $ \(cc, residency) → do
        let cells = [ cellAt (StoredCell cc i) | i ← [0 .. chunkSize * chunkSize - 1] ]
        terrain ← forM cells (wholeZ . csTerrain)
        fluid ← forM cells $ \case
            CellSpec e (Just (FluidSpec ty q)) → do
                _ ← activeVolume q
                pure (Just (FluidCell ty (unElevation e + unQuantity q)))
            CellSpec _ Nothing → pure Nothing
        let loaded = loadedChunkState (V.fromList fluid) (VU.fromList terrain)
            seeded = case residency of
                ResidentActive   → activateChunk loaded
                ResidentInactive → loaded
        pure (cc, residency, seeded)
    pure LegacyState
        { lsWorld = emptySimWorldState
            { swsChunks   = HM.fromList [ (cc, scs) | (cc, _, scs) ← chunks ]
            , swsActive   = True
            , swsTopology = plTopology (pmPlacement pm)
            }
        , lsDeclaredActive = HS.fromList
            [ cc | (cc, ResidentActive, _) ← chunks ]
        }
  where
    -- A fixture tile's declared state, or a padding wall.
    cellAt sc = case M.lookup sc (pmInverse pm) of
        Just t  → initialCell fx t
        Nothing → CellSpec (M.findWithDefault (fxBaseTerrain fx) sc (pmPadding pm)) Nothing

legacyObserve ∷ LegacyState → Observation
legacyObserve s = M.fromList
    [ (StoredCell cc i, cellAt scs i)
    | (cc, scs) ← HM.toList (swsChunks (lsWorld s))
    , i ← [0 .. chunkSize * chunkSize - 1] ]
  where
    cellAt scs i =
        let terrZ = scsTerrain scs VU.! i
            fluid
                | scsActive scs = case scsActiveFluid scs V.! i of
                    Just afc | afcVolume afc > 0 →
                        Just (afcType afc, Quantity (fromIntegral (afcVolume afc)))
                    _ → Nothing
                | otherwise = case scsFluid scs V.! i of
                    Just fc | fluidVolumeOverTerrain terrZ fc > 0 →
                        Just (fcType fc, Quantity (fluidVolumeOverTerrain terrZ fc))
                    _ → Nothing
        in CellState (Elevation (terrZ * fluidUnitsPerZ)) fluid

legacyEdit ∷ [CellEdit] → LegacyState → Either Text LegacyState
legacyEdit edits s = do
    world' ← foldM applyOne (lsWorld s) edits
    let topo = swsTopology world'
        edited = HS.fromList (map (scChunk . ceCell) edits)
        woken = HS.filter (`HS.member` lsDeclaredActive s) $ HS.unions
            (edited : [ HS.fromList (simCardinalNeighbors topo cc)
                      | cc ← HS.toList edited ])
        wake scs
            | scsActive scs = scs { scsEquilTicks = 0 }
            | otherwise     = activateChunk scs
    pure s { lsWorld = world'
                { swsChunks = HS.foldl' (\m cc → HM.adjust wake cc m)
                                        (swsChunks world') woken } }
  where
    applyOne world (CellEdit (StoredCell cc i) (CellState e fluid)) = do
        scs ← maybe (Left ("legacy adapter: no stored chunk " <> tshow cc)) Right
                    (HM.lookup cc (swsChunks world))
        terrZ ← wholeZ e
        passive ← case fluid of
            Nothing → pure Nothing
            Just (ty, q) → do
                _ ← activeVolume q
                pure (Just (FluidCell ty (unElevation e + unQuantity q)))
        active ← case fluid of
            Nothing → pure Nothing
            Just (ty, q) → do
                vol ← activeVolume q
                -- Keep the cell's own record (its flow-direction bits)
                -- when the edit leaves its fluid unchanged.
                pure $ Just $ case scsActiveFluid scs V.! i of
                    Just afc | afcType afc ≡ ty, afcVolume afc ≡ vol → afc
                    _ → ActiveFluidCell ty vol 0
        let scs' = scs
                { scsTerrain     = scsTerrain scs VU.// [(i, terrZ)]
                , scsFluid       = scsFluid scs V.// [(i, passive)]
                , scsActiveFluid = if scsActive scs
                                   then scsActiveFluid scs V.// [(i, active)]
                                   else scsActiveFluid scs
                }
        pure world { swsChunks = HM.insert cc scs' (swsChunks world) }
