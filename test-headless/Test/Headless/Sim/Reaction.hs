{-# LANGUAGE Strict #-}
-- | Pure tests for unlike-fluid contact in the active fluid simulation
--   (#2481, FR-1 of epic #2480).
--
--   Before this fix every occupied-destination write in
--   "Sim.Fluid.Active" kept the DESTINATION's fluid type and added the
--   incoming volume, so lava arriving in water silently became water and
--   water arriving in lava silently became lava. There are FOUR transfer
--   mechanisms but FIVE such write branches, and each one gets its own
--   fixture below so restoring any single branch to that behavior fails
--   its own example rather than hiding behind an earlier phase:
--
--     1. seam exchange (@transferCell@ via @reconcileSeams@)
--     2. @phaseGravity@
--     3. @phaseLateral@, snapshot-OCCUPIED destination
--     4. @phaseLateral@, snapshot-EMPTY destination filled live earlier
--        in the same phase
--     5. @phaseWaterfall@
--
--   Every fixture isolates ONE branch by terrain. Cells outside the
--   fixture's basin sit on 'wallZ' terrain: a wall neighbour is higher
--   than every fixture cell (so it drives neither gravity nor a
--   waterfall), is never at equal terrain (so it drives no lateral
--   equalization), and never holds fluid. Which of the three in-chunk
--   phases can act is then chosen purely by the relative terrain of the
--   two live cells, and the per-fixture comments say which one that is
--   and why the other two are inert.
--
--   Two reachability facts about this transfer geometry are load-bearing
--   for the acceptance matrix and are recorded here rather than expressed
--   as impossible fixtures:
--
--   * __The lateral branches cannot reach the 'Word16' capacity edge.__
--     The snapshot-occupied branch needs @diff > 1@, so against a
--     destination holding @maxBound - 1@ the largest representable source
--     (@maxBound@) yields @diff == 1@ and never transfers at all. The
--     snapshot-empty branch's destination can only have been filled by
--     earlier neighbours in the same phase, each paying at most
--     @srcVol \`div\` 4 <= 16383@, and a cell has at most three other
--     cardinal neighbours — under @maxBound - 1@. The capacity edge is
--     therefore exercised at gravity, waterfall and the seam, and every
--     branch (lateral included) satisfies the bounded-transfer rule by
--     routing through the one shared transition,
--     'Sim.Fluid.Reaction.applyTransfer'.
--
--   * __A waterfall's destination can never be the exhausted side.__
--     Reaching the waterfall branch at all requires suppressing gravity
--     at the same pair, which means the destination's fluid surface is at
--     least as high as the source's despite sitting at least two z lower
--     — so the destination necessarily holds MORE volume than the source
--     and can never be the smaller side of a contact. The
--     water-source/lava-destination ordering is therefore covered as a
--     reaction without an event, which is exactly what requirement 2
--     prescribes for a contact whose lava side survives.
module Test.Headless.Sim.Reaction (spec) where

import UPrelude
import Test.Hspec
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import Sim.State.Types
    (SimWorldState(..), SimChunkState(..), emptySimWorldState)
import Sim.Topology (SimTopology(..), simTopologyForParams, simSeamNeighbor)
import Sim.Fluid.Types (ActiveFluidCell(..), volumeToSurface)
import Sim.Fluid.Active (simulateActiveTick)
import Sim.Fluid.Reaction
    (SolidProduct(..), SolidificationEvent(..))
import Sim.Thread (fastSettleWorld)

n ∷ Int
n = chunkSize * chunkSize

-- | Terrain of a cell outside a fixture's basin. High enough that no
--   phase can act across the step in either direction, and always dry.
wallZ ∷ Int
wallZ = 10

idxOf ∷ Int → Int → Int
idxOf lx ly = ly * chunkSize + lx

homeChunk ∷ ChunkCoord
homeChunk = ChunkCoord 0 0

cell ∷ FluidType → Word16 → Maybe ActiveFluidCell
cell t vol = Just (ActiveFluidCell t vol 0)

-- | Terrain walled at 'wallZ' everywhere except the listed cells, each
--   of which is placed at its own z.
terrainWith ∷ [(Int, Int)] → VU.Vector Int
terrainWith placed = VU.replicate n wallZ VU.// placed

fluidWith ∷ [(Int, Maybe ActiveFluidCell)] → V.Vector (Maybe ActiveFluidCell)
fluidWith placed = V.replicate n Nothing V.// placed

-- | An ACTIVE chunk from an explicit terrain vector and fluid grid.
mkChunk ∷ VU.Vector Int → V.Vector (Maybe ActiveFluidCell) → SimChunkState
mkChunk terrain active = SimChunkState
    { scsFluid       = V.replicate n Nothing
    , scsTerrain     = terrain
    , scsSettleTicks = 0
    , scsActive      = True
    , scsActiveFluid = active
    , scsEquilTicks  = 0
    , scsSideDeco    = VU.replicate n 0
    -- These fixtures drive the pure phases directly; the freshness fence
    -- (#1596) lives on the world thread.
    , scsEditGen     = 0
    }

mkState ∷ SimTopology → [(ChunkCoord, SimChunkState)] → SimWorldState
mkState topo chunks = emptySimWorldState
    { swsChunks   = HM.fromList chunks
    , swsActive   = True
    , swsTopology = topo
    }

-- | The single-chunk fixture shape: one active chunk at 'homeChunk' on a
--   page with no seam.
oneChunk ∷ [(Int, Int)] → [(Int, Maybe ActiveFluidCell)] → SimWorldState
oneChunk terrain fluid =
    mkState SimFlatTopology
        [ (homeChunk, mkChunk (terrainWith terrain) (fluidWith fluid)) ]

-- * Observations

cellAt ∷ ChunkCoord → Int → SimWorldState → Maybe ActiveFluidCell
cellAt cc idx sws = case HM.lookup cc (swsChunks sws) of
    Nothing  → Nothing
    Just scs → scsActiveFluid scs V.! idx

homeAt ∷ Int → Int → SimWorldState → Maybe ActiveFluidCell
homeAt lx ly = cellAt homeChunk (idxOf lx ly)

-- | Total ACTIVE fluid volume across every chunk. Only meaningful while
--   no chunk has deactivated: 'deactivateInPlace' bakes rounded surfaces
--   through 'volumeToSurface' and discards the exact grid, so an
--   after-deactivation total is a different representation, not a
--   conservation result.
activeVolume ∷ SimWorldState → Int
activeVolume sws = sum
    [ fromIntegral (afcVolume afc)
    | scs ← HM.elems (swsChunks sws)
    , Just afc ← V.toList (scsActiveFluid scs) ]

events ∷ SimWorldState → [SolidificationEvent]
events = toList . swsSolidEvents

eventCoords ∷ SimWorldState → [(ChunkCoord, Int)]
eventCoords sws = [ (sevChunk e, sevIndex e) | e ← events sws ]

decoAt ∷ ChunkCoord → Int → SimWorldState → Word8
decoAt cc idx sws = case HM.lookup cc (swsChunks sws) of
    Nothing  → 0
    Just scs → scsSideDeco scs VU.! idx

isActiveAt ∷ ChunkCoord → SimWorldState → Bool
isActiveAt cc sws = maybe False scsActive (HM.lookup cc (swsChunks sws))

passiveAt ∷ ChunkCoord → Int → SimWorldState → Maybe FluidCell
passiveAt cc idx sws = case HM.lookup cc (swsChunks sws) of
    Nothing  → Nothing
    Just scs → scsFluid scs V.! idx

-- | Overwrite one cell of one chunk's live active grid. Used only to
--   stage a LATER tick's starting state (fresh lava arriving at a
--   coordinate that already solidified once).
setCell ∷ ChunkCoord → Int → Maybe ActiveFluidCell
        → SimWorldState → SimWorldState
setCell cc idx mc sws = sws
    { swsChunks = HM.adjust
        (\scs → scs { scsActiveFluid = scsActiveFluid scs V.// [(idx, mc)] })
        cc (swsChunks sws) }

setEquilTicks ∷ Int → SimWorldState → SimWorldState
setEquilTicks k sws = sws
    { swsChunks = HM.map (\scs → scs { scsEquilTicks = k }) (swsChunks sws) }

setChunkActive ∷ Bool → SimWorldState → SimWorldState
setChunkActive a sws = sws
    { swsChunks = HM.map (\scs → scs { scsActive = a }) (swsChunks sws) }

-- | The exact reaction bookkeeping requirement 2 asks for: total active
--   volume before equals total after plus TWICE the summed per-side
--   consumption (both sides lose the same amount). Only valid across a
--   non-deactivating tick, which every fixture that calls this is.
consumptionBalances ∷ SimWorldState → SimWorldState → Int → Expectation
consumptionBalances before after perSide =
    activeVolume before `shouldBe` activeVolume after + 2 * perSide

-- * Seam fixtures
--
-- Chunk A's east edge column (lx = chunkSize-1) faces chunk B's west
-- edge column (lx = 0). Both chunks are walled everywhere except their
-- one seam cell, so no in-chunk phase can act and the seam pass is the
-- only mover.

seamIdxA, seamIdxB ∷ Int
seamIdxA = idxOf (chunkSize - 1) 8
seamIdxB = idxOf 0 8

-- | A seam chunk holding fluid at ONE of its two edge cells. Terrain is
--   placed on BOTH edge columns so one helper serves the +X source and
--   the +X destination; only the cell the fixture names holds fluid.
mkSeamChunk ∷ Int → Int → Maybe ActiveFluidCell → SimChunkState
mkSeamChunk terrZ idx mc =
    mkChunk (terrainWith [(seamIdxA, terrZ), (seamIdxB, terrZ)])
            (fluidWith [(idx, mc)])

-- | A two-chunk seam world: chunk @ca@ holds @fa@ at its EAST edge cell
--   on terrain @za@; chunk @cb@ holds @fb@ at its WEST edge cell on
--   terrain @zb@.
seamWorld ∷ SimTopology
          → ChunkCoord → Int → Maybe ActiveFluidCell
          → ChunkCoord → Int → Maybe ActiveFluidCell
          → SimWorldState
seamWorld topo ca za fa cb zb fb = mkState topo
    [ (ca, mkSeamChunk za seamIdxA fa)
    , (cb, mkSeamChunk zb seamIdxB fb) ]

-- * Cylindrical wrap fixtures (#2044)
--
-- The same anchors "Test.Headless.Sim.Seam" uses: on a worldSize-64
-- page, @wrapXA@'s raw +X neighbour is stored as @wrapXB@, whose key has
-- BOTH components changed.

seamWorldSize ∷ Int
seamWorldSize = 64

cylTopo ∷ SimTopology
cylTopo = simTopologyForParams
    defaultWorldGenParams { wgpWorldSize = seamWorldSize }

wrapXA, wrapXB ∷ ChunkCoord
wrapXA = ChunkCoord 16 (-15)
wrapXB = ChunkCoord (-15) 17

spec ∷ Spec
spec = do

    -- * Branch 2 of 5: phaseGravity
    --
    -- Source one z above an occupied lower destination. The drop is
    -- exactly 1, so the waterfall branch (which needs drop > 1) cannot
    -- fire; the terrains differ, so lateral cannot either.
    describe "gravity into an occupied unlike destination" $ do
        let gravityWorld srcT srcV dstT dstV = oneChunk
                [ (idxOf 8 8, 1), (idxOf 9 8, 0) ]
                [ (idxOf 8 8, cell srcT srcV), (idxOf 9 8, cell dstT dstV) ]

        describe "lava moving into water (lava is the smaller side)" $ do
            let before = gravityWorld Lava 3 Lake 5
                after  = simulateActiveTick before

            it "consumes the smaller volume from BOTH sides" $ do
                homeAt 8 8 after `shouldBe` Nothing
                homeAt 9 8 after `shouldBe` cell Lake 2

            it "moves none of the requested transfer" $
                -- Master added the 1-unit gravity request to the
                -- destination and kept its type: Lava 2 / Lake 6.
                consumptionBalances before after 3

            it "emits one event naming the exhausted lava cell" $
                events after `shouldBe`
                    [ SolidificationEvent
                        { sevChunk        = homeChunk
                        , sevIndex        = idxOf 8 8
                        , sevWaterChunk   = homeChunk
                        , sevWaterType    = Lake
                        , sevConsumed     = 3
                        , sevStoneTop     = 2
                        , sevWaterSurface = volumeToSurface 0 2
                        , sevProduct      = SolidObsidian
                        } ]

        describe "water moving into lava (lava is the smaller side)" $ do
            let before = gravityWorld Lake 5 Lava 3
                after  = simulateActiveTick before

            it "consumes the smaller volume from BOTH sides" $ do
                homeAt 8 8 after `shouldBe` cell Lake 2
                homeAt 9 8 after `shouldBe` Nothing

            it "balances the per-side consumption" $
                consumptionBalances before after 3

            it "names the exhausted lava DESTINATION and picks basalt" $
                -- The remaining water surface (2) stands above the new
                -- stone top (1), so the stone ends up submerged.
                events after `shouldBe`
                    [ SolidificationEvent
                        { sevChunk        = homeChunk
                        , sevIndex        = idxOf 9 8
                        , sevWaterChunk   = homeChunk
                        , sevWaterType    = Lake
                        , sevConsumed     = 3
                        , sevStoneTop     = 1
                        , sevWaterSurface = volumeToSurface 1 2
                        , sevProduct      = SolidBasalt
                        } ]

        describe "equal volumes" $ do
            let before = gravityWorld Lava 5 Lake 5
                after  = simulateActiveTick before

            it "empties both cells" $ do
                homeAt 8 8 after `shouldBe` Nothing
                homeAt 9 8 after `shouldBe` Nothing
                activeVolume after `shouldBe` 0

            it "still emits exactly one event, for the lava side" $
                map (\e → (sevIndex e, sevConsumed e)) (events after)
                    `shouldBe` [(idxOf 8 8, 5)]

        it "emits nothing when only the WATER side is exhausted" $ do
            -- Lava 5 against Lake 3: the water runs out, the lava
            -- survives at 2, and a water cell reaching zero is not a
            -- solidification.
            let after = simulateActiveTick (gravityWorld Lava 5 Lake 3)
            homeAt 8 8 after `shouldBe` cell Lava 2
            homeAt 9 8 after `shouldBe` Nothing
            events after `shouldBe` []

    -- * Branch 3 of 5: phaseLateral, snapshot-OCCUPIED destination
    --
    -- Flat basin, so gravity and waterfall are both inert and only
    -- lateral equalization can move anything.
    describe "lateral equalization into an occupied unlike destination" $ do
        let lateralWorld srcT srcV dstT dstV = oneChunk
                [ (idxOf 8 8, 0), (idxOf 9 8, 0) ]
                [ (idxOf 8 8, cell srcT srcV), (idxOf 9 8, cell dstT dstV) ]

        describe "water moving into lava" $ do
            let before = lateralWorld River 10 Lava 2
                after  = simulateActiveTick before

            it "reacts instead of adding to the destination" $ do
                -- Master: River 8 / Lava 4 (destination keeps its type
                -- and gains the 2-unit transfer).
                homeAt 8 8 after `shouldBe` cell River 8
                homeAt 9 8 after `shouldBe` Nothing

            it "balances the per-side consumption" $
                consumptionBalances before after 2

            it "emits a basalt event for the exhausted lava cell" $
                events after `shouldBe`
                    [ SolidificationEvent
                        { sevChunk        = homeChunk
                        , sevIndex        = idxOf 9 8
                        , sevWaterChunk   = homeChunk
                        , sevWaterType    = River
                        , sevConsumed     = 2
                        , sevStoneTop     = 1
                        , sevWaterSurface = volumeToSurface 0 8
                        , sevProduct      = SolidBasalt
                        } ]

        describe "lava moving into water" $ do
            let before = lateralWorld Lava 10 Lake 2
                after  = simulateActiveTick before

            it "reacts, consuming the smaller (water) side entirely" $ do
                homeAt 8 8 after `shouldBe` cell Lava 8
                homeAt 9 8 after `shouldBe` Nothing

            it "balances the per-side consumption" $
                consumptionBalances before after 2

            it "emits nothing, because the lava side survived" $
                -- This branch cannot exhaust a lava SOURCE: it only
                -- fires when the source holds strictly more than the
                -- destination, so the source is never the smaller side.
                events after `shouldBe` []

    -- * Branch 4 of 5: phaseLateral, snapshot-EMPTY destination that an
    --   earlier source in the SAME phase has already filled.
    --
    -- Basin (8,8) — (9,8) — (10,8), all flat. The middle cell starts
    -- empty, so the snapshot marks it empty for every source this phase;
    -- the lower-index source spills into it first and the higher-index
    -- source then meets a LIVE occupied cell through the same branch.
    describe "lateral spill into a live-occupied snapshot-empty cell" $ do
        let laneWorld leftT leftV rightT rightV = oneChunk
                [ (idxOf 8 8, 0), (idxOf 9 8, 0), (idxOf 10 8, 0) ]
                [ (idxOf 8 8, cell leftT leftV)
                , (idxOf 10 8, cell rightT rightV) ]

        describe "lava fills the cell, water arrives second" $ do
            let before = laneWorld Lava 12 Lake 12
                after  = simulateActiveTick before

            it "pins the branch: the middle cell is empty to start with" $
                homeAt 9 8 before `shouldBe` Nothing

            it "reacts with what the earlier source left there" $ do
                -- (8,8) spills 12 `div` 4 = 3 units of Lava into the
                -- empty middle; (10,8) then requests 3 into it and finds
                -- Lava 3 live. Master silently added Lake to the Lava
                -- cell, leaving Lava 6 in the middle.
                homeAt 8 8 after `shouldBe` cell Lava 9
                homeAt 9 8 after `shouldBe` Nothing
                homeAt 10 8 after `shouldBe` cell Lake 9

            it "balances the per-side consumption" $
                consumptionBalances before after 3

            it "names the middle cell, which is where the lava died" $
                map (\e → (sevChunk e, sevIndex e, sevConsumed e))
                    (events after)
                    `shouldBe` [(homeChunk, idxOf 9 8, 3)]

        describe "water fills the cell, lava arrives second" $ do
            -- 40 `div` 4 = 10 units of Lake reach the middle, so the
            -- 8-unit lava source that follows is the smaller side and is
            -- itself exhausted — the same branch, the other ordering,
            -- and the event lands on the incoming source instead.
            let before = laneWorld Lake 40 Lava 8
                after  = simulateActiveTick before

            it "reacts and exhausts the arriving lava" $ do
                homeAt 8 8 after `shouldBe` cell Lake 30
                homeAt 9 8 after `shouldBe` cell Lake 2
                homeAt 10 8 after `shouldBe` Nothing

            it "balances the per-side consumption" $
                consumptionBalances before after 8

            it "emits an obsidian event naming the arriving lava cell" $
                events after `shouldBe`
                    [ SolidificationEvent
                        { sevChunk        = homeChunk
                        , sevIndex        = idxOf 10 8
                        , sevWaterChunk   = homeChunk
                        , sevWaterType    = Lake
                        , sevConsumed     = 8
                        , sevStoneTop     = 1
                        , sevWaterSurface = volumeToSurface 0 2
                        , sevProduct      = SolidObsidian
                        } ]

    -- * Branch 5 of 5: phaseWaterfall
    --
    -- A drop of 3 makes the waterfall branch eligible; the destination
    -- is deep enough that its fluid surface stands at or above the
    -- source's, which is what keeps gravity from firing at the same
    -- pair first.
    describe "waterfall into an occupied unlike destination" $ do
        let fallWorld srcT srcV dstT dstV = oneChunk
                [ (idxOf 8 8, 3), (idxOf 9 8, 0) ]
                [ (idxOf 8 8, cell srcT srcV), (idxOf 9 8, cell dstT dstV) ]

        describe "lava falling into water" $ do
            let before = fallWorld Lava 1 Lake 22
                after  = simulateActiveTick before

            it "pins the isolation: gravity cannot fire at this pair" $
                -- Source surface 3 + 1 = 4; destination surface 4. A
                -- non-positive surface difference is gravity's own
                -- guard, so the fall is the only eligible transfer.
                volumeToSurface 3 1 `shouldBe` volumeToSurface 0 22

            it "reacts instead of pouring into the destination" $ do
                homeAt 8 8 after `shouldBe` Nothing
                homeAt 9 8 after `shouldBe` cell Lake 21

            it "balances the per-side consumption" $
                consumptionBalances before after 1

            it "emits an obsidian event for the exhausted lava" $
                events after `shouldBe`
                    [ SolidificationEvent
                        { sevChunk        = homeChunk
                        , sevIndex        = idxOf 8 8
                        , sevWaterChunk   = homeChunk
                        , sevWaterType    = Lake
                        , sevConsumed     = 1
                        , sevStoneTop     = 4
                        , sevWaterSurface = volumeToSurface 0 21
                        , sevProduct      = SolidObsidian
                        } ]

            it "marks no waterfall side-deco, because nothing fell" $
                decoAt homeChunk (idxOf 8 8) after `shouldBe` 0

        describe "water falling into lava" $ do
            let before = fallWorld Lake 1 Lava 22
                after  = simulateActiveTick before

            it "reacts, consuming the smaller (water) side entirely" $ do
                homeAt 8 8 after `shouldBe` Nothing
                homeAt 9 8 after `shouldBe` cell Lava 21

            it "balances the per-side consumption" $
                consumptionBalances before after 1

            it "emits nothing, because the lava destination survived" $
                events after `shouldBe` []

    -- * Branch 1 of 5: the cross-chunk seam (transferCell)
    describe "seam exchange between unlike chunks" $ do
        let a = ChunkCoord 0 0
            b = ChunkCoord 1 0

        describe "lava crossing the seam into water" $ do
            -- Unequal terrain, so the seam's gravity rule runs: the
            -- lava side is one z higher and is also the smaller volume.
            let before = seamWorld SimFlatTopology
                            a 1 (cell Lava 3) b 0 (cell Lake 5)
                after  = simulateActiveTick before

            it "reacts across the seam instead of retyping the arrival" $ do
                cellAt a seamIdxA after `shouldBe` Nothing
                cellAt b seamIdxB after `shouldBe` cell Lake 2

            it "balances the per-side consumption" $
                consumptionBalances before after 3

            it "names the exhausted lava cell on the SOURCE side" $
                events after `shouldBe`
                    [ SolidificationEvent
                        { sevChunk        = a
                        , sevIndex        = seamIdxA
                        , sevWaterChunk   = b
                        , sevWaterType    = Lake
                        , sevConsumed     = 3
                        , sevStoneTop     = 2
                        , sevWaterSurface = volumeToSurface 0 2
                        , sevProduct      = SolidObsidian
                        } ]

        describe "ocean crossing the seam into lava" $ do
            -- Equal terrain, so the seam's lateral rule runs and the
            -- water side is the source. The exhausted lava is on the
            -- DESTINATION side of the same seam.
            let before = seamWorld SimFlatTopology
                            a 0 (cell Ocean 20) b 0 (cell Lava 3)
                after  = simulateActiveTick before

            it "reacts across the seam" $ do
                cellAt a seamIdxA after `shouldBe` cell Ocean 17
                cellAt b seamIdxB after `shouldBe` Nothing

            it "balances the per-side consumption" $
                consumptionBalances before after 3

            it "names the exhausted lava cell on the far side, as basalt" $
                -- Ocean contact is basalt outright (D-5's first clause).
                events after `shouldBe`
                    [ SolidificationEvent
                        { sevChunk        = b
                        , sevIndex        = seamIdxB
                        , sevWaterChunk   = a
                        , sevWaterType    = Ocean
                        , sevConsumed     = 3
                        , sevStoneTop     = 1
                        , sevWaterSurface = volumeToSurface 0 17
                        , sevProduct      = SolidBasalt
                        } ]

        describe "across the cylindrical U wrap boundary (#2044)" $ do
            it "pins the fixture: the far side really is a wrapped key" $
                simSeamNeighbor cylTopo 1 0 wrapXA `shouldBe` wrapXB

            it "names the canonical stored key when the lava is near-side" $ do
                let after = simulateActiveTick
                        (seamWorld cylTopo
                            wrapXA 1 (cell Lava 3) wrapXB 0 (cell Lake 5))
                eventCoords after `shouldBe` [(wrapXA, seamIdxA)]
                -- …and names the far side's own canonical key as the
                -- WATER participant (#2485 requirement 3): FR-2 admits
                -- both halves of a seam contact together, so a wrapped
                -- contact that named an unwrapped water key would be
                -- judged against a chunk the page stores nothing under.
                map sevWaterChunk (events after) `shouldBe` [wrapXB]
                cellAt wrapXA seamIdxA after `shouldBe` Nothing
                cellAt wrapXB seamIdxB after `shouldBe` cell Lake 2

            it "names the canonical stored key when the lava is far-side" $ do
                let after = simulateActiveTick
                        (seamWorld cylTopo
                            wrapXA 0 (cell Ocean 20) wrapXB 0 (cell Lava 3))
                eventCoords after `shouldBe` [(wrapXB, seamIdxB)]
                map sevWaterChunk (events after) `shouldBe` [wrapXA]
                cellAt wrapXA seamIdxA after `shouldBe` cell Ocean 17
                cellAt wrapXB seamIdxB after `shouldBe` Nothing

    -- * Requirement 5: snapshot-planned requests are paid from the LIVE
    --   source, and a reaction can drain it past what the plan assumed.
    describe "a second snapshot-planned request after a reaction" $ do
        -- (8,8) sits one z above TWO occupied neighbours, so gravity
        -- plans a request into each from one frozen snapshot. The first
        -- annihilates the whole 4-unit lava source; the second must find
        -- nothing left to spend.
        let before = oneChunk
                [ (idxOf 8 7, 0), (idxOf 8 8, 1), (idxOf 9 8, 0) ]
                [ (idxOf 8 7, cell Lake 6)
                , (idxOf 8 8, cell Lava 4)
                , (idxOf 9 8, cell Lake 6) ]
            after = simulateActiveTick before

        it "spends the source only once" $ do
            homeAt 8 8 after `shouldBe` Nothing
            homeAt 8 7 after `shouldBe` cell Lake 2
            -- Untouched: the second request found an exhausted source.
            homeAt 9 8 after `shouldBe` cell Lake 6

        it "neither overdraws nor recreates the exhausted source" $
            consumptionBalances before after 4

        it "emits exactly one event for the one exhausted lava cell" $
            map (\e → (sevIndex e, sevConsumed e)) (events after)
                `shouldBe` [(idxOf 8 8, 4)]

    -- * Requirement 4: refill is ordinary, and does not cancel the event
    describe "a cell emptied by annihilation, refilled the same tick" $ do
        -- Gravity from the raised (8,7) annihilates the lava at (8,8);
        -- lateral from (9,8) — flat with (8,8) — then refills it under
        -- the ordinary empty-cell rule.
        let before = oneChunk
                [ (idxOf 8 7, 1), (idxOf 8 8, 0), (idxOf 9 8, 0) ]
                [ (idxOf 8 7, cell Lake 7)
                , (idxOf 8 8, cell Lava 1)
                , (idxOf 9 8, cell Lake 20) ]
            after = simulateActiveTick before

        it "refills the coordinate with the incoming type and volume" $
            homeAt 8 8 after `shouldBe` cell Lake 5

        it "leaves the two water cells correctly debited" $ do
            homeAt 8 7 after `shouldBe` cell Lake 6
            homeAt 9 8 after `shouldBe` cell Lake 15

        it "balances the per-side consumption" $
            consumptionBalances before after 1

        it "keeps the solidification event exactly once" $
            events after `shouldBe`
                [ SolidificationEvent
                    { sevChunk        = homeChunk
                    , sevIndex        = idxOf 8 8
                    , sevWaterChunk   = homeChunk
                    , sevWaterType    = Lake
                    , sevConsumed     = 1
                    , sevStoneTop     = 1
                    , sevWaterSurface = volumeToSurface 1 6
                    , sevProduct      = SolidBasalt
                    } ]

        it "emits a NEW event when fresh lava is exhausted there later" $ do
            -- The at-most-one rule is per TICK, not per coordinate for
            -- the life of the page.
            let restaged = setCell homeChunk (idxOf 8 8) (cell Lava 1)
                    (simulateActiveTick (oneChunk
                        [ (idxOf 8 8, 1), (idxOf 9 8, 0) ]
                        [ (idxOf 8 8, cell Lava 3)
                        , (idxOf 9 8, cell Lake 5) ]))
                twice = simulateActiveTick restaged
            map (\e → (sevIndex e, sevConsumed e)) (events twice)
                `shouldBe` [(idxOf 8 8, 3), (idxOf 8 8, 1)]

    -- * Requirement 6: ordinary transfers are preserved and bounded
    describe "compatible and empty destinations still take a transfer" $ do
        it "an empty destination takes the SOURCE's type" $ do
            let after = simulateActiveTick (oneChunk
                    [ (idxOf 8 8, 0), (idxOf 9 8, 0) ]
                    [ (idxOf 8 8, cell Lava 8) ])
            homeAt 8 8 after `shouldBe` cell Lava 6
            homeAt 9 8 after `shouldBe` cell Lava 2
            events after `shouldBe` []

        it "a compatible occupied destination keeps its OWN type" $ do
            -- Water types are one class: Lake into River adds volume and
            -- reacts with nothing.
            let before = oneChunk
                    [ (idxOf 8 8, 0), (idxOf 9 8, 0) ]
                    [ (idxOf 8 8, cell Lake 10), (idxOf 9 8, cell River 2) ]
                after  = simulateActiveTick before
            homeAt 8 8 after `shouldBe` cell Lake 8
            homeAt 9 8 after `shouldBe` cell River 4
            activeVolume after `shouldBe` activeVolume before
            events after `shouldBe` []

        it "gravity moves only what fits below maxBound" $ do
            -- Destination one unit below maxBound. The surface step
            -- makes gravity request 10 units; exactly one fits, and the
            -- other nine stay at the source. The drop also makes the
            -- waterfall branch eligible, and it correctly moves nothing
            -- into the now-full destination.
            let before = oneChunk
                    [ (idxOf 8 8, 5), (idxOf 9 8, 0) ]
                    [ (idxOf 8 8, cell Lake maxBound)
                    , (idxOf 9 8, cell Lake (maxBound - 1)) ]
                after  = simulateActiveTick before
            homeAt 8 8 after `shouldBe` cell Lake (maxBound - 1)
            homeAt 9 8 after `shouldBe` cell Lake maxBound
            activeVolume after `shouldBe` activeVolume before

        it "a waterfall moves only what fits below maxBound" $ do
            -- Gravity is suppressed (the destination's surface towers
            -- over the source's), so this is the waterfall branch alone:
            -- it requests 3 and only 1 fits.
            let before = oneChunk
                    [ (idxOf 8 8, 3), (idxOf 9 8, 0) ]
                    [ (idxOf 8 8, cell Lake 10)
                    , (idxOf 9 8, cell Lake (maxBound - 1)) ]
                after  = simulateActiveTick before
            -- One unit really fell, so the source carries the east
            -- outflow bit ('cardinalNeighbors' order: N, E, S, W).
            homeAt 8 8 after
                `shouldBe` Just (ActiveFluidCell Lake 9 2)
            homeAt 9 8 after `shouldBe` cell Lake maxBound
            activeVolume after `shouldBe` activeVolume before

        it "the seam moves only what fits below maxBound" $ do
            let a = ChunkCoord 0 0
                b = ChunkCoord 1 0
                before = seamWorld SimFlatTopology
                    a 5 (cell Lake maxBound) b 0 (cell Lake (maxBound - 1))
                after  = simulateActiveTick before
            cellAt a seamIdxA after `shouldBe` cell Lake (maxBound - 1)
            cellAt b seamIdxB after `shouldBe` cell Lake maxBound
            activeVolume after `shouldBe` activeVolume before

    -- * Requirement 8: events accumulate in the pure tick result
    describe "event accumulation across chained ticks" $ do
        let reacting = oneChunk
                [ (idxOf 8 8, 1), (idxOf 9 8, 0) ]
                [ (idxOf 8 8, cell Lava 3), (idxOf 9 8, cell Lake 5) ]
            once = simulateActiveTick reacting

        it "a following NONREACTING tick keeps the event, once" $ do
            let twice = simulateActiveTick once
            events twice `shouldBe` events once
            length (events twice) `shouldBe` 1

        it "an INACTIVE tick keeps the event, once" $ do
            let idle = simulateActiveTick (setChunkActive False once)
            events idle `shouldBe` events once

        it "a DEACTIVATING tick keeps the event and bakes the grid" $ do
            -- One tick from 199 equilibrium ticks crosses
            -- 'equilThreshold' (200) because the settled grid does not
            -- change, so this drives the real deactivation path.
            let closing = simulateActiveTick (setEquilTicks 199 once)
            isActiveAt homeChunk closing `shouldBe` False
            homeAt 9 8 closing `shouldBe` Nothing
            -- The rounded passive representation, NOT an exact
            -- active-volume equality: 'deactivateInPlace' discards the
            -- volume grid on purpose.
            passiveAt homeChunk (idxOf 9 8) closing
                `shouldBe` Just (FluidCell Lake (volumeToSurface 0 2))
            events closing `shouldBe` events once

        it "the real fast-settle loop keeps the event, exactly once" $ do
            -- 'fastSettleWorld' is the dump path's own synchronous
            -- settle; it runs until every chunk has quiesced AND
            -- deactivated, which is ~200 further ticks here.
            let settled = fastSettleWorld 500 reacting
            isActiveAt homeChunk settled `shouldBe` False
            map (\e → (sevChunk e, sevIndex e)) (events settled)
                `shouldBe` [(homeChunk, idxOf 8 8)]
