{-# LANGUAGE Strict #-}
-- | Pure tests for the cross-chunk fluid seam exchange
--   ('Sim.Fluid.Active.reconcileSeams', exercised via 'simulateActiveTick')
--   and for the edit-time neighbour activation behind
--   'Sim.Command.Types.SimChunkEdited' ('Sim.Chunk.applyChunkEdit').
--
--   Setup: chunk A=(0,0) is full of water (5 levels), chunk B=(1,0) is
--   dry; both flat at z=0, both active. Before the seam fix, A's edge
--   cells had no in-chunk neighbour past the boundary, so water could
--   never cross into B (the 1-tile lip). These tests pin the two
--   properties that matter: total volume is conserved every tick, and
--   water actually crosses the seam and levels the step down.
--
--   The wrap-boundary group (#2044) pins the same properties for a pair
--   that is physically adjacent across the cylindrical U seam, where the
--   far-side chunk is STORED under a canonical key with BOTH components
--   changed. Those cases compare cell-for-cell against the ordinary pair
--   above, which is what proves the seam pair runs the same rules, the
--   same rate, the same edge orientation, and runs them exactly once.
module Test.Headless.Sim.Seam (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize, wrapChunkCoordU)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import Sim.State.Types (SimWorldState(..), SimChunkState(..))
import Sim.Chunk (applyChunkEdit, loadedChunkState)
import Sim.Fluid.Types (ActiveFluidCell(..), volumePerLevel, volumeToSurface)
import Sim.Fluid.Active (simulateActiveTick)
import Sim.Topology
    (SimTopology(..), simTopologyForParams, simCanonChunk, simSeamNeighbor)

n ∷ Int
n = chunkSize * chunkSize

-- | An active chunk: flat terrain at z=0, given active-fluid grid.
mkChunk ∷ V.Vector (Maybe ActiveFluidCell) → SimChunkState
mkChunk active = SimChunkState
    { scsFluid       = V.replicate n Nothing
    , scsTerrain     = VU.replicate n 0
    , scsSettleTicks = 0
    , scsActive      = True
    , scsActiveFluid = active
    , scsEquilTicks  = 0
    , scsSideDeco    = VU.replicate n 0
    -- Never edited: the seam algorithm is pure and the freshness fence
    -- (#1596) lives on the world thread, so the baseline generation is
    -- the right value here.
    , scsEditGen     = 0
    }

mkState ∷ SimTopology → [(ChunkCoord, SimChunkState)] → SimWorldState
mkState topo chunks = SimWorldState
    { swsChunks      = HM.fromList chunks
    , swsDirtyChunks = HS.empty
    , swsActive      = True
    , swsTopology    = topo
    }

-- * Page topologies, always through the production derivation

-- | A cylindrical page of this world size. Seed 42 and the default
--   (empty) timeline: not an arena, so this really is the wrapping case.
cylTopo ∷ Int → SimTopology
cylTopo worldSize =
    simTopologyForParams defaultWorldGenParams { wgpWorldSize = worldSize }

-- | An arena page, built exactly the way
--   'World.Thread.Command.Init.handleWorldInitArenaCommand' builds one:
--   seed 0 plus the 100000 world-size SENTINEL, which is not an extent.
arenaTopo ∷ SimTopology
arenaTopo = simTopologyForParams defaultWorldGenParams
    { wgpSeed = 0, wgpWorldSize = arenaSentinelSize }

arenaSentinelSize ∷ Int
arenaSentinelSize = 100000

-- | A page with a non-positive world size — no seam at all. Seed 7 keeps
--   it out of the arena branch, so this exercises the size guard itself.
sizedTopo ∷ Int → SimTopology
sizedTopo worldSize = simTopologyForParams defaultWorldGenParams
    { wgpSeed = 7, wgpWorldSize = worldSize }

-- * Observations

-- | Sum of active-fluid volume across all chunks.
activeVolume ∷ SimWorldState → Int
activeVolume ss = sum
    [ fromIntegral (afcVolume afc)
    | scs ← HM.elems (swsChunks ss)
    , Just afc ← V.toList (scsActiveFluid scs) ]

allActive ∷ SimWorldState → Bool
allActive ss = all scsActive (HM.elems (swsChunks ss))

tick ∷ Int → SimWorldState → SimWorldState
tick k ss = iterate simulateActiveTick ss !! k

-- | Volume in one chunk (active grid).
chunkVolume ∷ ChunkCoord → SimWorldState → Int
chunkVolume cc ss = case HM.lookup cc (swsChunks ss) of
    Nothing  → 0
    Just scs → sum [ fromIntegral (afcVolume afc)
                   | Just afc ← V.toList (scsActiveFluid scs) ]

-- | One chunk's whole active-fluid grid, cell for cell.
activeGrid ∷ ChunkCoord → SimWorldState → V.Vector (Maybe ActiveFluidCell)
activeGrid cc ss = maybe V.empty scsActiveFluid (HM.lookup cc (swsChunks ss))

isActiveAt ∷ ChunkCoord → SimWorldState → Bool
isActiveAt cc ss = maybe False scsActive (HM.lookup cc (swsChunks ss))

-- | Water surface of the cell at (lx,ly) in a chunk (terrain z=0).
surfAt ∷ ChunkCoord → Int → Int → SimWorldState → Int
surfAt cc lx ly ss = case HM.lookup cc (swsChunks ss) of
    Nothing  → 0
    Just scs → case scsActiveFluid scs V.! (ly * chunkSize + lx) of
        Nothing  → 0
        Just afc → volumeToSurface 0 (afcVolume afc)

-- * Wrap-boundary fixtures (#2044)
--
-- A worldSize-64 page wraps u = cx - cy into [-32, 31]. @seamXA@ sits at
-- the maximum u, so its raw +X neighbour is past the seam and is STORED
-- as @seamXB@; @seamYA@ sits at the minimum u, so its raw +Y neighbour
-- wraps the other way and is stored as @seamXA@ itself. Both spellings
-- are what 'wrapChunkCoordU' 64 produces, asserted below.

seamWorldSize ∷ Int
seamWorldSize = 64

seamXA, seamXB, seamYA ∷ ChunkCoord
seamXA = ChunkCoord 16 (-15)
seamXB = ChunkCoord (-15) 17
seamYA = ChunkCoord (-16) 16

-- | A flat terrain map and the two fluid maps an edit can carry.
flatTerrain ∷ VU.Vector Int
flatTerrain = VU.replicate n 0

dryFluid, wetFluid ∷ V.Vector (Maybe FluidCell)
dryFluid = V.replicate n Nothing
wetFluid = V.replicate n (Just (FluidCell Lake 5))

spec ∷ Spec
spec = do
    let full = V.replicate n (Just (ActiveFluidCell Lake
                                       (fromIntegral (5 * volumePerLevel)) 0))
        dry  = V.replicate n Nothing
        topo = cylTopo seamWorldSize
        st0  = mkState topo [ (ChunkCoord 0 0, mkChunk full)
                            , (ChunkCoord 1 0, mkChunk dry) ]
        total0 = activeVolume st0
        a = ChunkCoord 0 0
        b = ChunkCoord 1 0

    describe "seam exchange" $ do
        it "conserves total volume every tick (no water created/lost)" $
            map (\k → activeVolume (tick k st0)) [0, 1, 2, 5, 20, 100]
                `shouldBe` replicate 6 total0

        it "keeps both chunks active while equalising (< equilThreshold)" $
            allActive (tick 100 st0) `shouldBe` True

        it "spills water across the seam into the dry neighbour" $
            chunkVolume b (tick 30 st0) `shouldSatisfy` (> 0)

        it "drains the source chunk as water crosses" $
            chunkVolume a (tick 30 st0) `shouldSatisfy` (< chunkVolume a st0)

        it "shrinks the seam surface step over time (lip levels out)" $ do
            let step k = surfAt a (chunkSize - 1) 8 (tick k st0)  -- A east edge
                       - surfAt b 0               8 (tick k st0)  -- B west edge
            step 80 `shouldSatisfy` (< step 2)

        it "two identical chunks exchange nothing (no spurious flow)" $ do
            let eq = mkState topo [ (a, mkChunk full), (b, mkChunk full) ]
                v0 = activeVolume eq
                stN = tick 5 eq
            -- volume conserved AND each chunk unchanged at the seam
            activeVolume stN `shouldBe` v0
            surfAt a (chunkSize - 1) 8 stN `shouldBe` surfAt b 0 8 stN

        -- #2044: the same seam, but at the cylindrical U wrap.
        describe "across the cylindrical U wrap boundary" $ do
            let seamXState = mkState topo [ (seamXA, mkChunk full)
                                          , (seamXB, mkChunk dry) ]
                seamYState = mkState topo [ (seamYA, mkChunk full)
                                          , (seamXA, mkChunk dry) ]
                plainX     = mkState topo [ (a, mkChunk full)
                                          , (b, mkChunk dry) ]
                plainY     = mkState topo [ (ChunkCoord 0 0, mkChunk full)
                                          , (ChunkCoord 0 1, mkChunk dry) ]

            it "pins the fixture: the far side really is a wrapped key" $ do
                -- Both anchors are already canonical, and each one's raw
                -- neighbour is not — which is exactly why a raw probe
                -- missed.
                wrapChunkCoordU seamWorldSize seamXA `shouldBe` seamXA
                wrapChunkCoordU seamWorldSize seamYA `shouldBe` seamYA
                wrapChunkCoordU seamWorldSize (ChunkCoord 17 (-15))
                    `shouldBe` seamXB
                wrapChunkCoordU seamWorldSize (ChunkCoord (-16) 17)
                    `shouldBe` seamXA

            it "moves water across the wrapped +X seam" $
                chunkVolume seamXB (tick 30 seamXState) `shouldSatisfy` (> 0)

            it "moves water across the wrapped +Y seam" $
                chunkVolume seamXA (tick 30 seamYState) `shouldSatisfy` (> 0)

            it "runs the wrapped +X pair exactly like an ordinary pair" $
                forM_ [1, 2, 30 ∷ Int] $ \k → do
                    activeGrid seamXA (tick k seamXState)
                        `shouldBe` activeGrid a (tick k plainX)
                    activeGrid seamXB (tick k seamXState)
                        `shouldBe` activeGrid b (tick k plainX)

            it "runs the wrapped +Y pair exactly like an ordinary pair" $
                forM_ [1, 2, 30 ∷ Int] $ \k → do
                    activeGrid seamYA (tick k seamYState)
                        `shouldBe` activeGrid (ChunkCoord 0 0) (tick k plainY)
                    activeGrid seamXA (tick k seamYState)
                        `shouldBe` activeGrid (ChunkCoord 0 1) (tick k plainY)

            it "conserves total volume across the wrapped seams" $ do
                let vX = activeVolume seamXState
                    vY = activeVolume seamYState
                map (\k → activeVolume (tick k seamXState)) [0, 1, 2, 5, 20, 100]
                    `shouldBe` replicate 6 vX
                map (\k → activeVolume (tick k seamYState)) [0, 1, 2, 5, 20, 100]
                    `shouldBe` replicate 6 vY

            it "exchanges nothing when the page has no seam (the defect)" $ do
                -- The identical fixture on a flat page: the two chunks are
                -- not raw-adjacent, so nothing crosses. This is what every
                -- cylindrical page used to do (#2044).
                let flatX = mkState SimFlatTopology
                                [ (seamXA, mkChunk full), (seamXB, mkChunk dry) ]
                chunkVolume seamXB (tick 30 flatX) `shouldBe` 0
                chunkVolume seamXA (tick 30 flatX)
                    `shouldBe` chunkVolume seamXA flatX

        -- #2044: the SimChunkEdited activation half. reconcileSeams can
        -- only move fluid between two ACTIVE chunks, so the edit path has
        -- to wake the far-side chunk under its stored key.
        describe "wakes the neighbour an edit is adjacent to" $ do
            let loaded coords t = mkState t
                    [ (cc, loadedChunkState dryFluid flatTerrain) | cc ← coords ]
                edit cc = applyChunkEdit cc 1 wetFluid flatTerrain

            it "wakes the chunk stored across the +X wrap" $
                isActiveAt seamXB (edit seamXA (loaded [seamXA, seamXB] topo))
                    `shouldBe` True

            it "wakes the chunk stored across the -X wrap" $
                isActiveAt seamXA (edit seamXB (loaded [seamXA, seamXB] topo))
                    `shouldBe` True

            it "wakes the chunk stored across the +Y wrap" $
                isActiveAt seamXA (edit seamYA (loaded [seamYA, seamXA] topo))
                    `shouldBe` True

            it "wakes the chunk stored across the -Y wrap" $
                isActiveAt seamYA (edit seamXA (loaded [seamYA, seamXA] topo))
                    `shouldBe` True

            it "leaves that neighbour asleep on a page with no seam" $
                isActiveAt seamXB
                    (edit seamXA (loaded [seamXA, seamXB] SimFlatTopology))
                    `shouldBe` False

            it "still wakes the raw neighbours of an interior edit" $ do
                let st = loaded [a, b, ChunkCoord 0 1] topo
                    ed = edit a st
                isActiveAt a ed `shouldBe` True
                isActiveAt b ed `shouldBe` True
                isActiveAt (ChunkCoord 0 1) ed `shouldBe` True

        -- #2044 / #2001: the two non-wrapping pages, selected separately.
        describe "neighbour resolution on a non-wrapping page" $ do
            -- An arena's world size is the 100000 SENTINEL, and this coord
            -- sits right at the boundary that sentinel would wrap at, so an
            -- accidental `wrapChunkCoordU 100000` is directly observable.
            let arenaEdge  = ChunkCoord 24999 (-25000)
                arenaPastU = ChunkCoord 25000 (-25000)

            it "pins the fixture: the sentinel really would wrap this coord" $
                wrapChunkCoordU arenaSentinelSize arenaPastU
                    `shouldBe` ChunkCoord (-25000) 25000

            it "never wraps an arena" $ do
                arenaTopo `shouldBe` SimFlatTopology
                simCanonChunk arenaTopo arenaPastU `shouldBe` arenaPastU
                simSeamNeighbor arenaTopo 1 0 arenaEdge `shouldBe` arenaPastU

            it "wakes an arena edit's raw neighbour, not a wrapped one" $ do
                let st = mkState arenaTopo
                        [ (cc, loadedChunkState dryFluid flatTerrain)
                        | cc ← [arenaEdge, arenaPastU, ChunkCoord (-25000) 25000] ]
                    ed = applyChunkEdit arenaEdge 1 wetFluid flatTerrain st
                isActiveAt arenaPastU ed `shouldBe` True
                isActiveAt (ChunkCoord (-25000) 25000) ed `shouldBe` False

            it "never wraps a zero or negative world size" $
                forM_ [0, -1, -64] $ \worldSize → do
                    sizedTopo worldSize `shouldBe` SimFlatTopology
                    simSeamNeighbor (sizedTopo worldSize) 1 0 seamXA
                        `shouldBe` ChunkCoord 17 (-15)

            it "is the identity for an interior coord on a wrapping page" $ do
                simCanonChunk topo a `shouldBe` a
                simSeamNeighbor topo 1 0 a `shouldBe` b
                simSeamNeighbor topo 0 1 a `shouldBe` ChunkCoord 0 1
