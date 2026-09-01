{-# LANGUAGE Strict #-}
-- | Pure tests pinning fluid-volume conservation through
--   'Sim.Fluid.Active.simulateActiveTick', with the lateral pressure
--   equalization phase ('phaseLateral') as the subject (#2042).
--
--   That phase sizes every neighbour transfer from a frozen snapshot of
--   the grid, then applies the transfers sequentially against the live
--   mutable cells. Before the fix nothing bounded the payments by what
--   the source actually held, so a low-volume cell with several
--   thirstier neighbours over-drained and its 'Word16' volume wrapped to
--   ~65535 — fluid manufactured inside authoritative simulation state.
--
--   Every fixture here is ONE active chunk with a flat basin, which
--   makes gravity and waterfall inert (both need a terrain drop) and
--   isolates lateral equalization. Cells outside the basin sit on
--   raised "wall" terrain: unequal terrain fails 'phaseLateral''s
--   @nTerrZ ≡ terrZ@ test, and being HIGHER than the source it drives
--   neither gravity nor a waterfall, so a wall neighbour is inert in
--   all three phases.
module Test.Headless.Sim.Conservation (spec) where

import UPrelude
import Test.Hspec
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Random (StdGen, mkStdGen, randomR)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidType(..))
import Sim.State.Types (SimWorldState(..), SimChunkState(..))
import Sim.Fluid.Types (ActiveFluidCell(..))
import Sim.Fluid.Active (simulateActiveTick)

n ∷ Int
n = chunkSize * chunkSize

-- | Terrain height of a cell outside the flat basin. High enough that
--   no phase can act across the step in either direction.
wallZ ∷ Int
wallZ = 10

idxOf ∷ Int → Int → Int
idxOf lx ly = ly * chunkSize + lx

water ∷ Word16 → Maybe ActiveFluidCell
water vol = Just (ActiveFluidCell Lake vol 0)

-- | An active chunk from an explicit terrain vector and fluid grid.
mkChunk ∷ VU.Vector Int → V.Vector (Maybe ActiveFluidCell) → SimChunkState
mkChunk terrain active = SimChunkState
    { scsFluid       = V.replicate n Nothing
    , scsTerrain     = terrain
    , scsSettleTicks = 0
    , scsActive      = True
    , scsActiveFluid = active
    , scsEquilTicks  = 0
    , scsSideDeco    = VU.replicate n 0
    -- Never edited: these fixtures drive the pure phases directly, and
    -- the freshness fence (#1596) lives on the world thread.
    , scsEditGen     = 0
    }

mkState ∷ SimChunkState → SimWorldState
mkState scs = SimWorldState
    { swsChunks      = HM.singleton (ChunkCoord 0 0) scs
    , swsDirtyChunks = HS.empty
    , swsActive      = True
    }

-- | Terrain that is flat at z=0 only on the listed cells, walled off
--   everywhere else.
basinTerrain ∷ [Int] → VU.Vector Int
basinTerrain basin = VU.replicate n wallZ VU.// [ (i, 0) | i ← basin ]

-- | Every active-fluid volume in the world, in grid order.
volumes ∷ SimWorldState → [Int]
volumes ss = [ fromIntegral (afcVolume afc)
             | scs ← HM.elems (swsChunks ss)
             , Just afc ← V.toList (scsActiveFluid scs) ]

totalVolume ∷ SimWorldState → Int
totalVolume = sum . volumes

-- | The active-fluid cell at (lx,ly) of the single fixture chunk.
cellAt ∷ Int → Int → SimWorldState → Maybe ActiveFluidCell
cellAt lx ly ss = case HM.lookup (ChunkCoord 0 0) (swsChunks ss) of
    Nothing  → Nothing
    Just scs → scsActiveFluid scs V.! idxOf lx ly

volumeAt ∷ Int → Int → SimWorldState → Int
volumeAt lx ly = maybe 0 (fromIntegral . afcVolume) . cellAt lx ly

-- * The reproduction fixture (#2042)

-- | Flat terrain, centre volume 3, four cardinal neighbours volume 1 —
--   total 7. Each neighbour's snapshot difference is 2, so each requests
--   @max 1 (2 `div` 4) = 1@ and the centre is asked for 4 from a volume
--   of 3.
fourNeighbourState ∷ SimWorldState
fourNeighbourState =
    let cx = 8
        cy = 8
        centre = idxOf cx cy
        nbrs = [ idxOf cx (cy - 1), idxOf (cx + 1) cy
               , idxOf cx (cy + 1), idxOf (cx - 1) cy ]
        fluid = V.replicate n Nothing
            V.// ((centre, water 3) : [ (i, water 1) | i ← nbrs ])
    in mkState (mkChunk (VU.replicate n 0) fluid)

-- | The four cardinal neighbours of the reproduction fixture's centre.
fourNeighbourVolumes ∷ SimWorldState → [Int]
fourNeighbourVolumes ss = sort
    [ volumeAt 8 7 ss, volumeAt 9 8 ss, volumeAt 8 9 ss, volumeAt 7 8 ss ]

-- * Randomized conservation sweep

-- | Fixed seeds, reported here so a CI failure is reproducible verbatim.
conservationSeeds ∷ [Int]
conservationSeeds = [1 .. 60]

-- | The randomized domain, exactly as scoped for this fix: ONE active,
--   non-deactivating chunk with correctly sized vectors, flat terrain,
--   and cells that are either 'Nothing' or a positive low volume. The
--   volume ceiling keeps the aggregate (at most @256 * 40 = 10240@) far
--   below @maxBound ∷ Word16@, so any total-volume change this sweep
--   reports is a transfer defect and never a representable-range one.
randomFlatState ∷ Int → SimWorldState
randomFlatState seed =
    let go ∷ Int → StdGen → [Maybe ActiveFluidCell]
        go 0 _ = []
        go k g =
            let (wet, g')  = randomR (0 ∷ Int, 3) g
                (vol, g'') = randomR (1 ∷ Int, 40) g'
                cell = if wet ≡ 0 then Nothing else water (fromIntegral vol)
            in cell : go (k - 1) g''
        fluid = V.fromListN n (go n (mkStdGen seed))
    in mkState (mkChunk (VU.replicate n 0) fluid)

spec ∷ Spec
spec = do
    describe "lateral equalization from a source that cannot pay" $ do
        let before = fourNeighbourState
            after  = simulateActiveTick before

        it "conserves total volume instead of wrapping the source" $ do
            totalVolume before `shouldBe` 7
            totalVolume after `shouldBe` 7

        it "leaves no cell above the world's total volume" $
            volumes after `shouldSatisfy` all (≤ 7)

        it "dries the source out after three affordable one-unit payments" $
            cellAt 8 8 after `shouldBe` Nothing

        it "still makes minimum-one-unit progress on what it can afford" $
            fourNeighbourVolumes after `shouldBe` [1, 2, 2, 2]

    describe "lateral equalization the source can afford" $ do
        it "moves exactly a quarter of the snapshot difference" $ do
            -- Source 10, neighbour 2: difference 8, so `8 div 4` = 2.
            let basin = [idxOf 8 8, idxOf 9 8]
                fluid = V.replicate n Nothing
                    V.// [(idxOf 8 8, water 10), (idxOf 9 8, water 2)]
                before = mkState (mkChunk (basinTerrain basin) fluid)
                after  = simulateActiveTick before
            totalVolume before `shouldBe` 12
            volumeAt 8 8 after `shouldBe` 8
            volumeAt 9 8 after `shouldBe` 4
            totalVolume after `shouldBe` 12

        it "spills a quarter of its volume into an empty neighbour" $ do
            -- Source 8 (> volumePerLevel), empty neighbour: `8 div 4` = 2.
            let basin = [idxOf 8 8, idxOf 9 8]
                fluid = V.replicate n Nothing V.// [(idxOf 8 8, water 8)]
                before = mkState (mkChunk (basinTerrain basin) fluid)
                after  = simulateActiveTick before
            totalVolume before `shouldBe` 8
            volumeAt 8 8 after `shouldBe` 6
            volumeAt 9 8 after `shouldBe` 2
            totalVolume after `shouldBe` 8

    describe "randomized total-volume conservation" $
        it "conserves volume across one tick for every fixed seed" $ do
            let offenders =
                    [ (seed, v0, v1)
                    | seed ← conservationSeeds
                    , let st = randomFlatState seed
                          v0 = totalVolume st
                          v1 = totalVolume (simulateActiveTick st)
                    , v0 ≢ v1 ]
            offenders `shouldBe` []
