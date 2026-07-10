{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Build the zoom cache entries AND per-chunk pixel data in one pass,
--   split out of "World.ZoomMap.Cache" (issue #573).
module World.ZoomMap.Cache.BuildPixels
    ( buildZoomCacheWithPixels
    ) where

import UPrelude
import Control.Parallel.Strategies (parListChunk, using, rdeepseq)
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import World.Types
import World.Material (MaterialRegistry)
import World.Plate (elevationAtGlobal, isBeyondGlacier, isGlacierZone, wrapGlobalU)
import qualified Data.Vector.Unboxed as VU
import World.Fluids (isOceanChunk, hasAnyOceanFluid)
import World.Fluid.Lake.Types (wlByChunk)
import qualified Data.HashMap.Strict as HM
import World.Fluid.Lava (chunkHasLavaQuick)
import World.Fluid.IceLevel (lookupIceLevel)
import World.Generate.Chunk (generateZoomTerrain)
import World.Generate.InitTerrain (BorderedTerrainCache)
import World.Vegetation (vegHash, vegSnow)
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))
import World.ZoomMap.ColorPalette (ZoomColorPalette)
import World.ZoomMap.Cache.Classify (majorityMaterial, vegCategoryFromClimate)
import World.ZoomMap.Cache.Noise (zoomIceNoise)
import World.ZoomMap.Cache.Pixels (generateChunkPixels)
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST (runST)

-- * Build Zoom Cache + Per-Chunk Pixel Data

-- | Build the zoom cache and per-chunk pixel data in one pass.
--   For each chunk, computes material and vegetation at every
--   tile position (16×16) and generates RGBA pixel data using
--   the color palette.
--
--   The 'Maybe BorderedTerrainCache' is the optional init-time
--   per-chunk pipeline cache. When present (only on fresh world
--   init, where 'buildTimeline' produced it alongside the timeline),
--   'generateZoomTerrain' reads the pre-computed bordered terrain
--   instead of re-running 'applyTimelineChunk' /
--   'applyCoastalErosion' / 'removeElevationSpikes'. Loaded-save
--   paths pass 'Nothing' and pay the recompute cost.
buildZoomCacheWithPixels ∷ WorldGenParams → MaterialRegistry
                         → ZoomColorPalette
                         → Maybe BorderedTerrainCache
                         → (V.Vector ZoomChunkEntry, V.Vector BS.ByteString)
buildZoomCacheWithPixels params registry palette mBorderedCache =
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
                coord = ChunkCoord ccx ccy
                chunkOcean = isOceanChunk oceanMap coord
                           ∨ hasAnyOceanFluid worldSize oceanMap coord

                -- Neighbor ocean chunks for edge seeding of ocean flood fill
                _wrapC = wrapChunkCoordU worldSize
                _chunkOceanN = isOceanChunk oceanMap (wrapC (ChunkCoord ccx (ccy - 1)))
                            ∨ hasAnyOceanFluid worldSize oceanMap (wrapC (ChunkCoord ccx (ccy - 1)))
                _chunkOceanS = isOceanChunk oceanMap (wrapC (ChunkCoord ccx (ccy + 1)))
                            ∨ hasAnyOceanFluid worldSize oceanMap (wrapC (ChunkCoord ccx (ccy + 1)))
                _chunkOceanE = isOceanChunk oceanMap (wrapC (ChunkCoord (ccx + 1) ccy))
                            ∨ hasAnyOceanFluid worldSize oceanMap (wrapC (ChunkCoord (ccx + 1) ccy))
                _chunkOceanW = isOceanChunk oceanMap (wrapC (ChunkCoord (ccx - 1) ccy))
                            ∨ hasAnyOceanFluid worldSize oceanMap (wrapC (ChunkCoord (ccx - 1) ccy))

                -- Use the full detail-world pipeline (bordered region +
                -- timeline + coastal erosion + fluid + vegetation) for
                -- accurate terrain. Vegetation comes from the SAME
                -- per-tile function the detail world runs — the old
                -- chunk-level ocean gate here stripped veg from whole
                -- dry below-sea-level chunks (solid brown diamonds on
                -- the zoom map; see the note on 'generateZoomTerrain').
                (zoomElev, zoomMat, zoomVeg, chunkFluidMap) =
                    generateZoomTerrain registry params mBorderedCache coord

                tileData = [ let gx = baseGX + lx
                                 gy = baseGY + ly
                                 idx = ly * chunkSize + lx
                             in ( zoomElev VU.! idx
                                , zoomMat  VU.! idx
                                , zoomVeg  VU.! idx
                                , gx, gy )
                           | ly ← [0 .. chunkSize - 1]
                           , lx ← [0 .. chunkSize - 1]
                           ]

                -- (The per-tile ocean flood fill that lived here is
                -- gone — its output was threaded into the pixel
                -- generator but never read; ocean pixels come from
                -- the compose fluid map, which IS the detail rule.)

                -- Coastal erosion already applied by generateZoomTerrain.
                -- No separate coastal material pass needed.

                -- Summary stats from all tiles (for ZoomChunkEntry)
                -- Filter out beyond-glacier tiles (minBound elevation)
                -- to prevent overflow in avgElev and material contamination.
                allMats = [ (e, m) | (e, m, _, _, _) ← tileData, e > minBound ]
                winnerMat = majorityMaterial allMats
                avgElev = if null allMats then 0
                          else let s = sum (map fst allMats)
                               in s `div` length allMats
                chunkLava = chunkHasLavaQuick (wgpVolcanoCtx params) coord avgElev
                          -- Pool lava can spread into chunks with no
                          -- breach of their own — the global pool
                          -- table is authoritative for those.
                          ∨ HM.member coord
                              (wlByChunk (gtWorldLavaPools timeline))
                vegCat = if chunkOcean ∨ winnerMat ≡ 250
                         then 0
                         else vegCategoryFromClimate climate worldSize
                                  baseGX baseGY winnerMat

                -- Ice check at chunk center
                centerGX' = baseGX + chunkSize `div` 2
                centerGY' = baseGY + chunkSize `div` 2
                (cgx', cgy') = wrapGlobalU worldSize centerGX' centerGY'
                LocalClimate{lcTemp=cmt', lcSummerTemp=cst'
                            , lcWinterTemp=cwt'} =
                    lookupLocalClimate climate worldSize cgx' cgy'
                -- Use plate elevation at chunk center (globally deterministic)
                -- instead of avgElev (which includes erosion and varies per-chunk).
                (centerElev', _) = elevationAtGlobal seed plates worldSize cgx' cgy'
                altAboveSea' = max 0 (centerElev' - seaLevel)
                altCool' = fromIntegral altAboveSea' * (0.065 ∷ Float)
                ocnPen' = if centerElev' < seaLevel then 5.0 else 0.0 ∷ Float
                iceNoise' = zoomIceNoise seed cgx' cgy'
                effT' = cmt' + ocnPen' - altCool' + iceNoise'
                chunkIce' = not (isBeyondGlacier worldSize cgx' cgy')
                          ∧ (isGlacierZone worldSize cgx' cgy'
                             ∨ effT' < -2.0
                             ∨ (cwt' - altCool' < -10.0 ∧ cst' - altCool' < 5.0))

                entry = ZoomChunkEntry
                    { zceChunkX = ccx
                    , zceChunkY = ccy
                    , zceBaseGX = baseGX
                    , zceBaseGY = baseGY
                    , zceTexIndex = winnerMat
                    , zceElev     = avgElev
                    , zceIsOcean  = chunkOcean
                    , zceHasLava  = chunkLava
                    , zceVegCategory = vegCat
                    , zceHasIce  = chunkIce'
                    }

                -- Fluid map already computed by generateZoomTerrain
                -- (same pipeline as detail world: ocean + river + lake +
                -- equilibration + coastal gap fill).

                -- Ice overlay: per-tile decision using continuous noise
                -- that doesn't break at chunk boundaries.
                -- Uses global ice level grid for basin/drape decision.
                ilGrid = gtIceLevel timeline
                chunkIceMap = V.fromList
                    [ let (e, _, _, gx, gy) = td
                          (gx', gy') = wrapGlobalU worldSize gx gy
                          LocalClimate{lcTemp=mt, lcSummerTemp=st
                                      , lcWinterTemp=wt} =
                              lookupLocalClimate climate worldSize gx' gy'
                          (globalElev, _) = elevationAtGlobal seed plates worldSize gx' gy'
                          altAboveSea = max 0 (globalElev - seaLevel)
                          altCool = fromIntegral altAboveSea * (0.065 ∷ Float)
                          ocnPen = if globalElev < seaLevel
                                    then 5.0 else 0.0 ∷ Float
                          n = zoomIceNoise seed gx' gy'
                          effT = mt + ocnPen - altCool + n
                          ice = not (isBeyondGlacier worldSize gx' gy')
                              ∧ (isGlacierZone worldSize gx' gy'
                                 ∨ effT < -2.0
                                 ∨ (wt - altCool < -10.0
                                    ∧ st - altCool < 5.0))
                          mIceLevel = lookupIceLevel ilGrid worldSize gx' gy'
                      in if ice ∧ e > minBound
                         then case mIceLevel of
                            Just iceLevel | e < iceLevel →
                                Just (IceCell (min iceLevel (e + 20)) BasinIce)
                            _ → Just (IceCell (e + 1) DrapeIce)
                         else Nothing
                    | (td, _idx') ← zip tileData [0 ∷ Int ..]
                    ]

                -- Inject snow veg on ice-covered tiles
                tileDataWithIce = zipWith (\idx' td →
                    case chunkIceMap V.! idx' of
                        Just _ →
                            let (e, m, _, gx, gy) = td
                                h = vegHash seed gx gy
                                var = fromIntegral ((h `shiftR` 8) .&. 0x03) ∷ Word8
                            in (e, m, vegSnow + var, gx, gy)
                        Nothing → td
                    ) [0 ∷ Int ..] tileData

                -- Return intermediate data for pass 2 (cross-chunk extension)
                elevVec = zoomElev
            in (entry, tileData, chunkFluidMap
               , (chunkLava, chunkIceMap, tileDataWithIce, elevVec))

        allCoords = [ let wrappedU = ((u + halfSize) `mod` w + w) `mod` w - halfSize
                          ccx = (wrappedU + v) `div` 2
                          ccy = (v - wrappedU) `div` 2
                      in (ccx, ccy)
                    | v ← [-halfSize .. halfSize - 1]
                    , u ← [-halfSize .. halfSize - 1]
                    , even (u + v)
                    ]

        -- Deduplicate via Set.  Same order used for entries, pixels,
        -- and atlas packing — see comment in buildZoomCache.
        uniqueCoords = Set.toList $ Set.fromList allCoords

        chunkBatchSize = max 1 (length uniqueCoords `div` 128)

        -- Pass 1: compute terrain + fluid for all chunks (parallelized)
        pass1Results = map buildOne uniqueCoords
                         `using` parListChunk chunkBatchSize rdeepseq

        -- Build lookup map: chunk coord → fluid map for cross-chunk extension
        wrapC = wrapChunkCoordU worldSize
        fluidLookup = Map.fromList
            [ (ChunkCoord ccx ccy, fm)
            | ((ccx, ccy), (_, _, fm, _)) ← zip uniqueCoords pass1Results
            ]

        -- Pass 2: extend ocean at chunk boundaries using neighbor fluid data,
        -- then regenerate pixels.  For each chunk, check if edge tiles should
        -- be ocean by looking at the adjacent chunk's fluid map.
        extendAndRender ((ccx, ccy), (entry, _tileData0, rawFluid, extras)) =
            let _coord = ChunkCoord ccx ccy
                -- Check if neighbor chunk has ocean at a specific tile
                neighborHasOcean nx ny =
                    let ncx = if ny < 0 then ccx else if ny ≥ chunkSize then ccx else ccx + (if nx < 0 then -1 else if nx ≥ chunkSize then 1 else 0)
                        ncy = if nx < 0 ∨ nx ≥ chunkSize then ccy else if ny < 0 then ccy - 1 else if ny ≥ chunkSize then ccy + 1 else ccy
                        nlx = if nx < 0 then chunkSize - 1 else if nx ≥ chunkSize then 0 else nx
                        nly = if ny < 0 then chunkSize - 1 else if ny ≥ chunkSize then 0 else ny
                        ncoord = wrapC (ChunkCoord ncx ncy)
                    in case Map.lookup ncoord fluidLookup of
                        Just nfm → case nfm V.! (nly * chunkSize + nlx) of
                            Just (FluidCell Ocean _) → True
                            _ → False
                        Nothing → False

                -- Extend ocean: empty tiles at seaLevel+1/+2 adjacent to
                -- ocean (including in neighbor chunks) get ocean fluid
                extendedFluid = runST $ do
                    mv ← V.thaw rawFluid
                    let area = chunkSize * chunkSize
                    forM_ [0 .. area - 1] $ \idx → do
                        val ← MV.read mv idx
                        when (isNothing val) $ do
                            let (_, _, _, elevV) = extras
                                surfZ = elevV VU.! idx
                            when (surfZ ≤ seaLevel + 2 ∧ surfZ > minBound) $ do
                                let lx = idx `mod` chunkSize
                                    ly = idx `div` chunkSize
                                    checkAdj x y
                                      | x ≥ 0 ∧ x < chunkSize
                                        ∧ y ≥ 0 ∧ y < chunkSize =
                                          isJust ⊚ MV.read mv (y * chunkSize + x)
                                      | otherwise =
                                          return (neighborHasOcean x y)
                                n ← checkAdj lx (ly-1)
                                s ← checkAdj lx (ly+1)
                                w ← checkAdj (lx-1) ly
                                e ← checkAdj (lx+1) ly
                                when (n ∨ s ∨ w ∨ e) $
                                    MV.write mv idx (Just (FluidCell Ocean seaLevel))
                    V.freeze mv

                (chunkLava0, chunkIceMap0, tileDataWithIce0, _) = extras
                tileVec = V.fromList tileDataWithIce0
                pixels = generateChunkPixels palette chunkLava0
                             worldSize extendedFluid chunkIceMap0 tileVec
            in (entry, pixels)

        results = map extendAndRender (zip uniqueCoords pass1Results)
        (entries, pixelDatas) = unzip results
    in (V.fromList entries, V.fromList pixelDatas)
