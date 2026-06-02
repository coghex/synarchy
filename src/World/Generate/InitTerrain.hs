{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | World-init terrain compute, shared between 'World.Geology.Timeline'
--   (which uses it to build the world-resolution terrain that the
--   global priority flood runs against) and any future caller that
--   wants chunk-pipeline-accurate terrain without paying for all the
--   other 'generateChunk' work (materials, strata, fluid, etc.).
--
--   Lives in its own module to dodge the import cycle:
--   'World.Geology.Timeline' needs this function, but
--   'World.Generate.Chunk' transitively depends on 'World.Geology', so
--   keeping the function here lets both 'Timeline' and 'Chunk' import
--   it without going through each other.
module World.Generate.InitTerrain
    ( computeChunkInteriorTerrain
    , computeChunkBorderedPipeline
    , BorderedTerrainCache
    , borderedToInterior
    ) where

import UPrelude
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)
import World.Generate.Timeline (applyTimelineChunk, removeElevationSpikes)
import World.Geology.Coastal (applyCoastalErosion)
import World.Geology.Timeline.Types (GeoTimeline)
import World.Material (MaterialId, MaterialRegistry, matGlacier)
import World.Ocean.Types (OceanMap)
import World.Plate
    (TectonicPlate, elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Scale (computeWorldScale)

-- | Per-chunk cached output of the init-time bordered pipeline
--   (plate-base → 'applyTimelineChunk' → 'applyCoastalErosion' →
--   'removeElevationSpikes'). Both 'buildWorldTerrain' and
--   'generateZoomTerrain' want this; computing it once and looking up
--   per chunk saves ~50% of the per-chunk pipeline work during init.
--
--   The pair is @(borderedElev, borderedMat)@, both length
--   @(chunkSize + 2 * chunkBorder)^2@.
type BorderedTerrainCache =
    HM.HashMap ChunkCoord (VU.Vector Int, VU.Vector MaterialId)

-- | Compute the 16×16 interior terrain for one chunk using the SAME
--   pipeline 'World.Generate.Chunk.generateChunk' runs (plate-base →
--   'applyTimelineChunk' → 'applyCoastalErosion' → 'removeElevationSpikes')
--   minus the downstream-only work (materials, strata, fluid,
--   vegetation). Output is indexed @ly * chunkSize + lx@; beyond-
--   glacier tiles get 'minBound'.
--
--   The 'World.Geology.Timeline.buildTimeline' caller stitches one
--   of these per chunk into a world-resolution terrain grid that
--   'World.Fluid.Lake.Identify' runs the priority flood against —
--   so basin bitmasks match what the chunk renderer will produce,
--   eliminating the fast-vs-chunk divergence that produced grass-
--   topped columns in the middle of lakes.
computeChunkInteriorTerrain
    ∷ Word64
    → [TectonicPlate]
    → Int                  -- ^ worldSize
    → MaterialRegistry
    → OceanMap
    → GeoTimeline
    → ChunkCoord
    → VU.Vector Int
computeChunkInteriorTerrain seed plates worldSize registry oceanMap
                            timeline coord =
    let (borderedElev, _) =
            computeChunkBorderedPipeline seed plates worldSize registry
                                          oceanMap timeline coord
    in borderedToInterior worldSize coord borderedElev

-- | The bordered version of the init pipeline. Runs the same four
--   passes as 'computeChunkInteriorTerrain' but returns the full
--   @(chunkSize + 2*chunkBorder)^2@ tile region — both elevation and
--   the post-coastal material vectors.  Consumers that only need the
--   16×16 interior can call 'borderedToInterior'; the zoom-cache
--   build wants the bordered region directly so its strata-cache
--   neighbour reads stay consistent with the detail world.
computeChunkBorderedPipeline
    ∷ Word64
    → [TectonicPlate]
    → Int                  -- ^ worldSize
    → MaterialRegistry
    → OceanMap
    → GeoTimeline
    → ChunkCoord
    → (VU.Vector Int, VU.Vector MaterialId)
computeChunkBorderedPipeline seed plates worldSize registry oceanMap
                             timeline coord =
    let wsc        = computeWorldScale worldSize
        borderSize = chunkSize + 2 * chunkBorder
        borderArea = borderSize * borderSize
        fromIndex idx =
            let (by, bx) = idx `divMod` borderSize
            in (bx - chunkBorder, by - chunkBorder)

        (baseElev, baseMat) = runST $ do
            elevM ← VUM.new borderArea
            matM  ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \idx → do
                let (lx, ly) = fromIndex idx
                    (gx, gy) = chunkToGlobal coord lx ly
                    (gx', gy') = wrapGlobalU worldSize gx gy
                if isBeyondGlacier worldSize gx' gy'
                    then do
                        VUM.write elevM idx (seaLevel + 100)
                        VUM.write matM  idx matGlacier
                    else do
                        let (elev, mat) =
                                elevationAtGlobal seed plates worldSize gx' gy'
                        VUM.write elevM idx elev
                        VUM.write matM  idx mat
            elevF ← VU.unsafeFreeze elevM
            matF  ← VU.unsafeFreeze matM
            pure (elevF, matF)

        (timelineElev, timelineMat) =
            applyTimelineChunk timeline worldSize registry wsc coord
                (baseElev, baseMat)

        (postCoastElev, finalMat) =
            applyCoastalErosion seed worldSize plates registry timeline
                                oceanMap coord
                (timelineElev, timelineMat)

        (finalElev, _) = removeElevationSpikes 12 4 borderSize
                                               (postCoastElev, finalMat)

    in (finalElev, finalMat)

-- | Slice a bordered elevation vector down to the @chunkSize^2@
--   interior used by 'buildWorldTerrain' for the global priority
--   flood. Returns @minBound@ for beyond-glacier tiles.
borderedToInterior
    ∷ Int                  -- ^ worldSize
    → ChunkCoord
    → VU.Vector Int        -- ^ bordered (chunkSize + 2*chunkBorder)^2
    → VU.Vector Int
borderedToInterior worldSize coord borderedElev =
    let borderSize = chunkSize + 2 * chunkBorder
        toIndex lx ly =
            let bx = lx + chunkBorder
                by = ly + chunkBorder
            in by * borderSize + bx
    in VU.generate (chunkSize * chunkSize) $ \i →
        let lx = i `mod` chunkSize
            ly = i `div` chunkSize
            (gx, gy) = chunkToGlobal coord lx ly
            (gx', gy') = wrapGlobalU worldSize gx gy
        in if isBeyondGlacier worldSize gx' gy'
           then minBound
           else borderedElev VU.! toIndex lx ly
