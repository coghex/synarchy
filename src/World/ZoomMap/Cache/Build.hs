{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Build the zoom cache entries (no per-tile pixel data), split out
--   of "World.ZoomMap.Cache" (issue #573).
module World.ZoomMap.Cache.Build
    ( buildZoomCache
    ) where

import UPrelude
import Control.Parallel.Strategies (parListChunk, using, rdeepseq)
import qualified Data.Set as Set
import qualified Data.Vector as V
import World.Types
import World.Material (MaterialId(..), matGlacier, MaterialRegistry)
import World.Plate (elevationAtGlobal, isBeyondGlacier, isGlacierZone, wrapGlobalU)
import World.Fluids (isOceanChunk, hasAnyOceanFluid)
import World.Fluid.Lake.Types (wlByChunk)
import qualified Data.HashMap.Strict as HM
import World.Fluid.Lava (chunkHasLavaQuick)
import World.Generate (applyTimelineFast)
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))
import World.ZoomMap.Cache.Classify (majorityMaterial, vegCategoryFromClimate)
import World.ZoomMap.Cache.Noise (zoomIceNoise)

-- * Sampling Configuration

sampleGridSize ∷ Int
sampleGridSize = 5

sampleOffsets ∷ [(Int, Int)]
sampleOffsets =
    let step = max 1 (chunkSize `div` (sampleGridSize + 1))
    in [ (sx * step, sy * step)
       | sx ← [1 .. sampleGridSize]
       , sy ← [1 .. sampleGridSize]
       ]

-- * Build Zoom Cache (called once at world init)

buildZoomCache ∷ WorldGenParams → MaterialRegistry → V.Vector ZoomChunkEntry
buildZoomCache params registry =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        plates = wgpPlates params
        halfSize = worldSize `div` 2
        timeline = wgpGeoTimeline params
        oceanMap = wgpOceanMap params
        climate = wgpClimateState params

        w = halfSize * 2

        buildOne (ccx, ccy) =
            let baseGX = ccx * chunkSize
                baseGY = ccy * chunkSize

                samples = [ let gx = baseGX + ox
                                gy = baseGY + oy
                                (gx', gy') = wrapGlobalU worldSize gx gy
                                (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gx' gy'
                            in if baseMat ≡ matGlacier
                               then (baseElev, unMaterialId baseMat)
                               else if baseElev < -100 then (baseElev, 0)
                               else let (e, m) = applyTimelineFast timeline plates
                                                     worldSize gx' gy'
                                                     registry
                                                     (baseElev, baseMat)
                                    in (e, unMaterialId m)
                          | (ox, oy) ← sampleOffsets
                          ]
                winnerMat = majorityMaterial samples
                avgElev = let s = sum (map fst samples)
                          in s `div` length samples

                coord = ChunkCoord ccx ccy
                chunkLava = chunkHasLavaQuick (wgpVolcanoCtx params) coord avgElev
                          -- Pool lava can spread into chunks with no
                          -- breach of their own — the global pool
                          -- table is authoritative for those.
                          ∨ HM.member coord
                              (wlByChunk (gtWorldLavaPools timeline))
                chunkOcean = isOceanChunk oceanMap coord
                          ∨ hasAnyOceanFluid worldSize oceanMap coord

                -- Vegetation category from climate
                vegCat = if chunkOcean ∨ winnerMat ≡ 250
                         then 0
                         else vegCategoryFromClimate climate worldSize
                                  baseGX baseGY winnerMat

                -- Ice check at chunk center
                centerGX = baseGX + chunkSize `div` 2
                centerGY = baseGY + chunkSize `div` 2
                (cgx, cgy) = wrapGlobalU worldSize centerGX centerGY
                LocalClimate{lcTemp=cmt, lcSummerTemp=cst
                            , lcWinterTemp=cwt} =
                    lookupLocalClimate climate worldSize cgx cgy
                -- Use plate elevation at chunk center (globally deterministic)
                -- instead of avgElev (which includes erosion and varies per-chunk).
                (centerElev, _) = elevationAtGlobal seed plates worldSize cgx cgy
                altAboveSea = max 0 (centerElev - seaLevel)
                altCool = fromIntegral altAboveSea * (0.065 ∷ Float)
                ocnPen = if centerElev < seaLevel then 5.0 else 0.0 ∷ Float
                iceNoise = zoomIceNoise seed cgx cgy
                effT = cmt + ocnPen - altCool + iceNoise
                chunkIce = not (isBeyondGlacier worldSize cgx cgy)
                         ∧ (isGlacierZone worldSize cgx cgy
                            ∨ effT < -2.0
                            ∨ (cwt - altCool < -10.0 ∧ cst - altCool < 5.0))
            in ZoomChunkEntry
                { zceChunkX = ccx
                , zceChunkY = ccy
                , zceBaseGX = baseGX
                , zceBaseGY = baseGY
                , zceTexIndex = winnerMat
                , zceElev     = avgElev
                , zceIsOcean  = chunkOcean
                , zceHasLava  = chunkLava
                , zceVegCategory = vegCat
                , zceHasIce  = chunkIce
                }

        allCoords = [ let wrappedU = ((u + halfSize) `mod` w + w) `mod` w - halfSize
                          ccx = (wrappedU + v) `div` 2
                          ccy = (v - wrappedU) `div` 2
                      in (ccx, ccy)
                    | v ← [-halfSize .. halfSize - 1]
                    , u ← [-halfSize .. halfSize - 1]
                    , even (u + v)
                    ]

        -- Deduplicate via Set.  The reordering is harmless: every
        -- downstream consumer (entry vector, pixel vector, atlas
        -- packing, bakeEntriesAtlas) iterates the same list in
        -- the same order, so vector indices stay consistent.
        uniqueCoords = Set.toList $ Set.fromList allCoords

        chunkBatchSize = max 1 (length uniqueCoords `div` 128)
        results = map buildOne uniqueCoords `using` parListChunk chunkBatchSize rdeepseq
    in V.fromList results
