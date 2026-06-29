{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Wrap-seam regression tests.
--
--   The world is a cylinder along the isometric u-axis (gx − gy);
--   the seam meridian at u = ±worldTiles/2 is the least-exercised
--   geometry in worldgen. These tests pin the seam behaviors that
--   have regressed silently before:
--
--   1. EVENT APPLICATION ACROSS THE SEAM. 'applyTimelineChunk'
--      prefilters each row's events by a Y band before the exact
--      per-tile 'tileInBBoxWrapped' check. Wrapping u shifts gy by
--      ±worldTiles/2 (gy = (v − u) / 2), so a row filter on the
--      pre-wrap gy alone drops events whose bbox sits across the
--      seam from the chunk — half a crater simply missing on one
--      side of the meridian. We build a one-event timeline on a
--      flat synthetic base (zero erosion) and require the chunk
--      path to reproduce the direct 'applyGeoEvent' result exactly,
--      for a chunk local to the event AND for the chunk physically
--      adjacent to it across the seam.
--
--   2. HYDROLOGY GRID COVERAGE. 'buildInitialElevGrid' samples the
--      world in (u, v) space on a gridW × gridW lattice. Its x-torus
--      wrap is only sound if gridW * spacing equals the world's
--      u-period exactly; floor-division spacing left a 128-tile
--      uncovered stripe at worldSize 128 (256 at 256) where no
--      per-age rivers, lakes, or valley carving could originate,
--      and stitched flow across a phantom discontinuity.
--
--   3. RIVER COMPONENT CONNECTIVITY (post-fix contract). Width-wing
--      tiles must stay in their river's component even when their
--      own descent direction points away from the channel, so the
--      length cull can't strip a river's bank tiles.
module Test.Headless.WorldGen.WrapSeam (spec) where

import UPrelude
import Test.Hspec
import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import World.Base (GeoCoord(..))
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Generate.Constants (chunkBorder)
import World.Generate.Timeline (applyTimelineChunk)
import World.Geology.Event (applyGeoEvent)
import World.Geology.Types (GeoModification(..))
import World.Geology.Timeline.Types
    ( GeoTimeline(..), GeoEvent(..), GeoScale(..), CraterParams(..)
    , ErosionParams(..), emptyTimeline )
import World.Geology.Timeline.Helpers (mkGeoPeriod)
import World.Material
    ( MaterialId(..), MaterialProps(..), emptyMaterialRegistry
    , getMaterialProps )
import World.Scale (computeWorldScale)
import World.Plate (generatePlates)
import World.Hydrology.Simulation (buildInitialElevGrid, ElevGrid(..))
import World.Fluid.River.Identify (labelRiverComponents)
-- Issue #316: the seam wrap must be one shared definition. We reach it
-- through BOTH re-export paths and require them to agree, so a future
-- divergent copy (the bug this guards) fails here.
import qualified World.Slope as Slope
import qualified World.Fluid.Internal as Fluid

-- * Shared geometry
--
-- worldSize 8 → worldTiles w = 128, halfW = 64; canonical u ∈ [−64, 64).
-- The crater sits at (gx, gy) = (30, −30), i.e. u = 60, v = 0 — six
-- tiles shy of the +u seam. Chunk (1, −2) contains the crater centre;
-- chunk (−2, 1) holds tiles down to u = −63, which are 1–5 tiles from
-- the crater centre PHYSICALLY (across the wrap) while their pre-wrap
-- gy band ([2, 45] with border) is disjoint from the event bbox's gy
-- band ([−54, −6]).

seamWorldSize ∷ Int
seamWorldSize = 8

baseZ ∷ Int
baseZ = 50

crater ∷ CraterParams
crater = CraterParams
    { cpCenter       = GeoCoord 30 (-30)
    , cpRadius       = 16
    , cpDepth        = 20
    , cpRimHeight    = 5
    , cpEjectaRadius = 24
    , cpMeteorite    = Nothing
    , cpCenterElev   = baseZ
    }

-- | Erosion disabled (zero rates AND zero duration) so the only
--   elevation change in the pipeline is the crater event itself —
--   the chunk path's output is then exactly @base + applyGeoEvent@.
zeroErosion ∷ ErosionParams
zeroErosion = ErosionParams 0 0 0 0 0 0 0 0 0 0 False

seamTimeline ∷ GeoTimeline
seamTimeline = emptyTimeline
    { gtSeed      = 1
    , gtWorldSize = seamWorldSize
    , gtPeriods   = [ mkGeoPeriod seamWorldSize "seam crater" Age 0 0.0
                          [CraterEvent crater] zeroErosion HM.empty ]
    }

borderSize ∷ Int
borderSize = chunkSize + 2 * chunkBorder

-- | Run the authoritative chunk pipeline over a flat synthetic base.
runChunk ∷ ChunkCoord → VU.Vector Int
runChunk cc =
    let base = ( VU.replicate (borderSize * borderSize) baseZ
               , VU.replicate (borderSize * borderSize) (MaterialId 1) )
    in fst (applyTimelineChunk seamTimeline seamWorldSize
                               emptyMaterialRegistry
                               (computeWorldScale seamWorldSize) cc base)

elevAt ∷ VU.Vector Int → Int → Int → Int
elevAt out lx ly = out VU.! ((ly + chunkBorder) * borderSize
                             + (lx + chunkBorder))

-- | Ground truth: the event applied directly at a tile (wrapped delta
--   inside 'applyGeoEvent'), with the registry's default hardness —
--   the same call 'applyTimelineChunk' makes per tile.
expectedAt ∷ Int → Int → Int
expectedAt gx gy =
    let hardness = mpHardness (getMaterialProps emptyMaterialRegistry
                                                (MaterialId 1))
    in baseZ + gmElevDelta (applyGeoEvent (CraterEvent crater)
                                          seamWorldSize gx gy baseZ hardness)

-- | Compare a chunk's full 16×16 interior against ground truth.
interiorMatches ∷ ChunkCoord → [((Int, Int), (Int, Int))]
interiorMatches cc@(ChunkCoord cx cy) =
    let out = runChunk cc
    in [ ((lx, ly), (got, want))
       | lx ← [0 .. chunkSize - 1]
       , ly ← [0 .. chunkSize - 1]
       , let gx   = cx * chunkSize + lx
             gy   = cy * chunkSize + ly
             got  = elevAt out lx ly
             want = expectedAt gx gy
       , got ≠ want
       ]

spec ∷ Spec
spec = do
    describe "timeline events across the wrap seam" $ do

        it "premise: the crater modifies tiles on BOTH sides of the seam" $ do
            -- Local side (sanity that the event math fires at all)…
            expectedAt 30 (-30) `shouldSatisfy` (≠ baseZ)
            -- …and across the seam: tile (−32, 31) is u = −63, v = −1 —
            -- wrapped distance ≈ 3.6 tiles from the crater centre, well
            -- inside the bowl. If this fails the test geometry is wrong,
            -- not the engine.
            expectedAt (-32) 31 `shouldSatisfy` (≠ baseZ)

        it "chunk containing the event reproduces applyGeoEvent exactly" $
            interiorMatches (ChunkCoord 1 (-2)) `shouldBe` []

        it "chunk across the seam reproduces applyGeoEvent exactly" $
            -- Chunk (−2, 1): pre-wrap rows gy ∈ [2, 45] never intersect
            -- the event bbox's gy band [−54, −6]; only a wrap-aware row
            -- filter (or none) lets tileInBBoxWrapped see the event.
            interiorMatches (ChunkCoord (-2) 1) `shouldBe` []

    describe "hydrology elevation grid coverage" $
        forM_ [32, 64, 96, 128, 192, 256] $ \ws →
            it ("gridW * spacing covers worldTiles exactly at worldSize "
                ⧺ show ws) $ do
                let plates = generatePlates 42 ws 4
                    grid   = buildInitialElevGrid 42 ws plates
                -- Exact coverage ⇒ no unsampled stripe at the seam AND
                -- the grid's x-torus wrap aligns with the world's
                -- u-period (the wrap is only valid when these divide).
                egGridW grid * egSpacing grid `shouldBe` ws * chunkSize

    describe "wrapChunkCoordU single source of truth (issue #316)" $ do
        -- A grid of coords spanning interior, the ±u seam, and well past
        -- it (multi-period wrap), in (cx, cy) space.
        let coords = [ ChunkCoord cx cy
                     | cx ← [-20 .. 20], cy ← [-20 .. 20] ]
            uOf (ChunkCoord cx cy) = cx - cy

        it "slope and fluid re-exports are the same function" $
            -- Both modules re-export World.Chunk.Types.wrapChunkCoordU; if
            -- anyone re-introduces a divergent local copy this breaks.
            forM_ [0, 1, 2, 8, 32, 64, 128, 256] $ \ws →
                forM_ coords $ \cc →
                    Slope.wrapChunkCoordU ws cc
                        `shouldBe` Fluid.wrapChunkCoordU ws cc

        it "is identity for non-wrapping (arena / zero-size) worlds" $
            -- worldSize ≤ 1 ⇒ w = (ws `div` 2) * 2 = 0; the old un-guarded
            -- fluid copy did `mod 0` here and crashed.
            forM_ [-3, 0, 1] $ \ws →
                forM_ coords $ \cc →
                    Fluid.wrapChunkCoordU ws cc `shouldBe` cc

        it "folds u into the canonical [-halfW, halfW) range and is idempotent" $
            forM_ [2, 8, 32, 64, 128, 256] $ \ws → do
                let w     = (ws `div` 2) * 2
                    halfW = w `div` 2
                forM_ coords $ \cc → do
                    let wrapped = Slope.wrapChunkCoordU ws cc
                        u       = uOf wrapped
                    -- canonical range
                    (u ≥ -halfW ∧ u < halfW) `shouldBe` True
                    -- already-canonical ⇒ unchanged (idempotent)
                    Slope.wrapChunkCoordU ws wrapped `shouldBe` wrapped

    describe "river component connectivity" $ do
        let wt = 16
            n  = wt * wt
            at x y = y * wt + x

        it "4-adjacent river tiles share a component (wing contract)" $ do
            -- Width-expansion wings often have steepest-descent dirs
            -- pointing away from their channel; labelling must be
            -- pure adjacency so the length cull can't strip them.
            let mask = VU.generate n (\i → i ≡ at 5 5 ∨ i ≡ at 6 5)
                (comps, nComps) = labelRiverComponents wt mask
            nComps `shouldBe` 1
            comps VU.! at 5 5 `shouldBe` comps VU.! at 6 5

        it "diagonal-only river tiles stay separate components" $ do
            let mask = VU.generate n (\i → i ≡ at 5 5 ∨ i ≡ at 6 6)
                (_, nComps) = labelRiverComponents wt mask
            nComps `shouldBe` 2

        it "a centre line with detached-dir wings labels as one river" $ do
            -- A 6-tile horizontal centre line with wings above and
            -- below — 18 tiles, one component.
            let mask = VU.generate n (\i →
                    let x = i `mod` wt
                        y = i `div` wt
                    in x ≥ 4 ∧ x ≤ 9 ∧ y ≥ 7 ∧ y ≤ 9)
                (_, nComps) = labelRiverComponents wt mask
            nComps `shouldBe` 1
