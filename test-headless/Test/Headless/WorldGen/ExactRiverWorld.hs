{-# LANGUAGE Strict #-}
-- | Check the FINAL indexed table against the historical footprint and
-- the independent geometric constraints. Reuses the shared seed-42 world;
-- a pure timeline reconstruction recovers the co-evolved climate/grid which
-- Init discards when it refines the page climate. It creates no second page.
module Test.Headless.WorldGen.ExactRiverWorld (spec) where
import UPrelude
import Test.Hspec
import Data.IORef (readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Core.State (EngineEnv(..))
import Test.Headless.Harness (sharedWorld, getWorldGenParams)
import World.Generate.Types (WorldGenParams(..))
import World.Geology.Timeline.Types (GeoTimeline(..))
import World.Geology.Timeline (buildTimeline)
import World.Geology.Timeline.Stitch (stitchWorldTerrain)
import World.Generate.Chunk (generateChunk, generateZoomTerrain)
import World.Fluid.Lake.Identify (computeWorldEdgeOcean)
import World.Fluid.River.Identify.Flow
import World.Fluid.River.Identify.Components
import World.Fluid.River.Identify.Breakthrough
import World.Fluid.River.Identify.ChunkIndex (buildRiverChunkIndex)
import World.Fluid.River.Identify.Surface (riverSurfaceBaseline)
import World.Fluid.River.Identify.SurfaceFlow (resolveSurfaceFlow)
import World.Fluid.River.Identify.Common
import World.Fluid.River.Types (WorldRivers(..), RiverChunkEntry(..))
import World.Fluid.Types (FluidCell(..), FluidType(Ocean), fluidVolumeOverTerrain)
import World.Fluid.Exact (exactSurfaceOfZ, fluidUnitsPerZ)
import World.Constants (seaLevel)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Flora.Types (emptyFloraCatalog)
import World.Page.Types (WorldPageId(..))

spec ∷ SpecWith EngineEnv
spec = describe "WorldGen.ExactRiver generated geometry" $
  forM_ [(42,32,4),(42,64,3),(4567,32,4),(13579,32,4)] $ \(seed,size,plates) →
    it ("preserves footprint, bounded cuts and composed depth for " <> show (seed,size,plates)) $ \env → do
        -- The smaller world supplies coastal mouths; the canonical w64
        -- world supplies inland bends and large overlapping cross-sections.
        world ← sharedWorld env seed size plates
        Just p ← getWorldGenParams world
        registry ← readIORef (materialRegistryRef env)
        let ws = wgpWorldSize p
            n = ws * chunkSize
            half = n `div` 2
            timeline = wgpGeoTimeline p
            (rebuilt, timelineClimate, cache, _, _) = buildTimeline registry
                (wgpSeed p) ws (wgpPlateCount p) (wgpErosionIntensity p)
                (wgpVolcanicActivity p) (wgpLavaPoolDepth p) (wgpLavaPoolRadius p)
                (wgpWaterfallQuantum p) (wgpOreLevers p) (wgpTimelineParams p)
            terrain = stitchWorldTerrain ws cache
            lakes = gtWorldLakes timeline
            lakeIds = buildLakeIdAt ws lakes
            (spillways, owners, dirs) = resolveSpillways ws lakes terrain lakeIds
            order = bucketSortAscending terrain
            (flow, _) = computeFlowAccumulation n terrain lakeIds dirs spillways
                (computePrecipUnits ws timelineClimate terrain)
                (computeEvapUnits ws timelineClimate terrain) order
            primary = VU.generate (n*n) $ \i → terrain VU.! i > seaLevel
                ∧ lakeIds VU.! i < 0 ∧ flow VU.! i ≥ 100
            centres = extendRiverChains n terrain lakeIds dirs primary order
            q = wgpWaterfallQuantum p
            cs = clampCentreSurfaces n terrain dirs centres order q
            (mask, widths, surf, perp, claims) =
                expandWidthWithSections n terrain dirs flow centres cs
            (cid,nc) = labelRiverComponents n mask
            (maskF,cidF,ncF) = cullByLength ws cid nc mask
            (maskB,cidB,widthB,surfB,_,paths) = addBreakthroughsWithPaths
                n maskF cidF dirs terrain (computeWorldEdgeOcean terrain n)
                widths surf perp
            old = clampLateralSurfaces n maskB surfB q
            historical = buildRiverChunkIndex ws half maskB cidB old widthB
            actual = gtWorldRivers timeline
            strip e = e {rcePerTileSurfZ = VU.empty}
            entries = wrByChunk actual
            indexed = VU.replicate (n*n) minBound VU.//
                [ ((cy*chunkSize + i `div` chunkSize + half)*n
                    + cx*chunkSize + i `mod` chunkSize + half, rcePerTileSurfZ e VU.! i)
                | (ChunkCoord cx cy,es) ← HM.toList entries, e ← V.toList es,
                  i ← [0..255], rceBitmask e VU.! i]
            wet = [i | i ← [0..n*n-1], maskB VU.! i]
            rawEdges = [(i,j) | i ← wet, centres VU.! i,
                Just j ← [stepDir n i (dirs VU.! i)], maskB VU.! j, centres VU.! j]
                <> concatMap (\path → zip path (drop 1 path)) paths
            baseline = riverSurfaceBaseline n maskB old paths q
            flowEdges = resolveSurfaceFlow n maskB baseline rawEdges
        -- The frozen selection path is independent of the new surface pass.
        gtWorldLakes rebuilt `shouldBe` gtWorldLakes timeline
        [cc | cc ← HM.keys entries <> HM.keys historical,
            fmap (V.map strip) (HM.lookup cc entries)
                ≢ fmap (V.map strip) (HM.lookup cc historical)] `shouldBe` []
        wrRivers actual `shouldBe` buildRivers ws terrain lakeIds owners
            spillways dirs flow maskB cidB ncF
        wet `shouldSatisfy` (not . null)
        [i | i ← wet, indexed VU.! i ≡ minBound] `shouldBe` []
        [(a,b) | (a,b) ← claims, maskB VU.! a, maskB VU.! b,
                  baseline VU.! a ≡ baseline VU.! b,
                  indexed VU.! a ≢ indexed VU.! b] `shouldBe` []
        [(a,b) | (a,b) ← flowEdges, indexed VU.! a < indexed VU.! b] `shouldBe` []
        [(a,b) | a ← wet, d ← [dirNorth,dirEast,dirSouth,dirWest],
                  Just b ← [stepDir n a d], maskB VU.! b,
                  abs (indexed VU.! a - indexed VU.! b) > exactSurfaceOfZ q]
            `shouldBe` []
        [(a,b) | a ← wet, d ← [dirNorth,dirEast,dirSouth,dirWest],
                  Just b ← [stepDir n a d], maskB VU.! b,
                  abs (indexed VU.! a - indexed VU.! b)
                    > max 1 (exactSurfaceOfZ (abs (old VU.! a - old VU.! b)))]
            `shouldBe` []
        [end | path ← paths, not (null path), let end = last path,
               indexed VU.! end ≢ exactSurfaceOfZ seaLevel] `shouldBe` []
        [(a,b) | (a,b) ← flowEdges,
            (indexed VU.! a - indexed VU.! b) `mod` fluidUnitsPerZ ≢ 0]
            `shouldSatisfy` (not . null)
        [(i,baseline VU.! i-indexed VU.! i) | i ← wet,
            baseline VU.! i-indexed VU.! i > fluidUnitsPerZ-1] `shouldBe` []
        [(i,exactSurfaceOfZ (old VU.! i)-indexed VU.! i) | i ← wet,
            exactSurfaceOfZ (old VU.! i)-indexed VU.! i > 2*fluidUnitsPerZ-1]
            `shouldBe` []
        -- Actual generated terrain includes post-carve despike and magma caps;
        -- Ocean may supersede a river mask, but must also contain real water.
        coastalCounts ← forM (HM.toList entries) $ \(cc,es) → do
            let (_,_,bed,cells,_,_,_,_) = generateChunk registry emptyFloraCatalog
                    (WorldPageId "exact-river-check") p cc
                (_,_,_,zoomCells) = generateZoomTerrain registry p (Just cache) cc
                bad = [(i,rcePerTileSurfZ e VU.! i,bed VU.! i,cells V.! i)
                      | e ← V.toList es, i ← [0..255], rceBitmask e VU.! i,
                    exactSurfaceOfZ (bed VU.! i) ≥ rcePerTileSurfZ e VU.! i
                    ∨ maybe True ((≤ 0) . fluidVolumeOverTerrain (bed VU.! i))
                        (cells V.! i)]
            (cc,bad) `shouldBe` (cc,[])
            zoomCells `shouldBe` cells
            pure $ length [() | e ← V.toList es, i ← [0..255],
                rceBitmask e VU.! i, Just (FluidCell Ocean _) ← [cells V.! i]]
        when (seed ≡ 42 ∧ size ≡ 32) $ sum coastalCounts `shouldSatisfy` (> 0)
