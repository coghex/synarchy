{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Build the zoom cache at world init time.
--   Pure world-generation logic – no rendering imports.
module World.ZoomMap.Cache
    ( buildZoomCache
    , buildZoomCacheWithPixels
    , majorityMaterial
    , wrapChunkX
    , wrapChunkY
    ) where

import UPrelude
import Control.Parallel.Strategies (parListChunk, using, rdeepseq)
import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import World.Types
import World.Constants (seaLevel)
import World.Material (MaterialId(..), matGlacier, MaterialRegistry(..)
                      , MaterialProps(..), getMaterialProps)
import World.Plate (TectonicPlate(..), elevationAtGlobal
                   , isBeyondGlacier, isGlacierZone, wrapGlobalU
                   , twoNearestPlates, BoundaryType(..), classifyBoundary)
import qualified Data.Vector.Unboxed as VU
import World.Fluids (isOceanChunk, hasAnyLavaQuick, hasAnyOceanFluid
                    , hasAnyRiverQuick, hasAnyLakeQuick
                    , computeChunkFluid, computeChunkRivers
                    , computeChunkLakes, unionFluidMap
                    , equilibrateFluidMap)
import World.Fluid.Types (FluidCell(..), FluidType(..), IceCell(..), IceMode(..), IceMap)
import World.Fluid.IceLevel (lookupIceLevel)
import World.Fluid.Internal (FluidMap, wrapChunkCoordU)
import World.Geology.Timeline.Types (GeoEvent(..))
import World.Hydrology.Types (HydroFeature(..), RiverParams(..))
import World.Generate (applyTimelineFast)
import World.Generate.Chunk (generateZoomTerrain)
import Data.Bits ((.&.), shiftR, xor)
import Data.Word (Word64)
import World.Vegetation (isBarrenMaterial, isWetlandSoil
                        , selectVegetation, vegHash, vegNone, vegSnow, vegVariants)
import World.Weather.Types (ClimateState(..))
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))
import World.ZoomMap.ColorPalette (ZoomColorPalette, lookupMatColor
                                  , lookupVegColorById
                                  , defaultOceanColor, defaultLavaColor)
import World.Base (GeoCoord(..))
import World.Geology.Hash (valueNoise2D)
import World.Geology.Coastal (CoastType(..), classifyCoast, isDepositional
                             , beachMaterial, wetlandMaterial, deltaMaterial
                             , coastHash, sandProfile, outcroppHardness
                             , maxCoastalDist, filterNearbyMouths
                             , isNearRiverMouth, shorelineOffset)
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST (runST)
import Control.Monad (when, forM_)
import World.Render.Zoom.Types (zoomTileSize)

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
        features = gtFeatures timeline
        climate = wgpClimateState params

        w = halfSize * 2

        buildOne (ccx, ccy) =
            let baseGX = ccx * chunkSize
                baseGY = ccy * chunkSize

                samples = [ let gx = baseGX + ox
                                gy = baseGY + oy
                                (gx', gy') = wrapGlobalU worldSize gx gy
                                (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gx' gy'
                                hardness = mpHardness (getMaterialProps registry baseMat)
                            in if baseMat ≡ matGlacier
                               then (baseElev, unMaterialId baseMat)
                               else if baseElev < -100 then (baseElev, 0)
                               else let (e, m) = applyTimelineFast timeline
                                                     worldSize gx' gy'
                                                     hardness
                                                     (baseElev, baseMat)
                                    in (e, unMaterialId m)
                          | (ox, oy) ← sampleOffsets
                          ]
                winnerMat = majorityMaterial samples
                avgElev = let s = sum (map fst samples)
                          in s `div` length samples

                coord = ChunkCoord ccx ccy
                chunkLava = hasAnyLavaQuick features seed plates worldSize coord avgElev
                chunkOcean = isOceanChunk oceanMap coord
                          ∨ hasAnyOceanFluid worldSize oceanMap coord
                eventRivers = concatMap extractEventRivers' (gtPeriods timeline)
                chunkRiver = hasAnyRiverQuick eventRivers worldSize coord
                chunkLake  = hasAnyLakeQuick features worldSize coord

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
                , zceHasRiver = chunkRiver
                , zceHasLake  = chunkLake
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

-- * Build Zoom Cache + Per-Chunk Pixel Data

-- | Build the zoom cache and per-chunk pixel data in one pass.
--   For each chunk, computes material and vegetation at every
--   tile position (16×16) and generates RGBA pixel data using
--   the color palette.
buildZoomCacheWithPixels ∷ WorldGenParams → MaterialRegistry
                         → ZoomColorPalette
                         → (V.Vector ZoomChunkEntry, V.Vector BS.ByteString)
buildZoomCacheWithPixels params registry palette =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        plates = wgpPlates params
        halfSize = worldSize `div` 2
        timeline = wgpGeoTimeline params
        oceanMap = wgpOceanMap params
        features = gtFeatures timeline
        climate = wgpClimateState params

        w = halfSize * 2

        buildOne (ccx, ccy) =
            let baseGX = ccx * chunkSize
                baseGY = ccy * chunkSize
                coord = ChunkCoord ccx ccy
                chunkOcean = isOceanChunk oceanMap coord
                           ∨ hasAnyOceanFluid worldSize oceanMap coord

                -- Neighbor ocean chunks for edge seeding of ocean flood fill
                wrapC = wrapChunkCoordU worldSize
                chunkOceanN = isOceanChunk oceanMap (wrapC (ChunkCoord ccx (ccy - 1)))
                            ∨ hasAnyOceanFluid worldSize oceanMap (wrapC (ChunkCoord ccx (ccy - 1)))
                chunkOceanS = isOceanChunk oceanMap (wrapC (ChunkCoord ccx (ccy + 1)))
                            ∨ hasAnyOceanFluid worldSize oceanMap (wrapC (ChunkCoord ccx (ccy + 1)))
                chunkOceanE = isOceanChunk oceanMap (wrapC (ChunkCoord (ccx + 1) ccy))
                            ∨ hasAnyOceanFluid worldSize oceanMap (wrapC (ChunkCoord (ccx + 1) ccy))
                chunkOceanW = isOceanChunk oceanMap (wrapC (ChunkCoord (ccx - 1) ccy))
                            ∨ hasAnyOceanFluid worldSize oceanMap (wrapC (ChunkCoord (ccx - 1) ccy))

                -- Use the full detail-world pipeline (bordered region +
                -- timeline + coastal erosion + fluid) for accurate terrain.
                (zoomElev, zoomMat, chunkFluidMap) =
                    generateZoomTerrain registry params coord

                -- Build tile data from accurate terrain + compute vegetation
                tileData = [ let gx = baseGX + lx
                                 gy = baseGY + ly
                                 idx = ly * chunkSize + lx
                                 elev  = zoomElev VU.! idx
                                 matId = zoomMat  VU.! idx
                                 LocalClimate{lcTemp=temp, lcPrecip=precip
                                             , lcHumidity=humid, lcSnow=snow} =
                                     lookupLocalClimate climate worldSize gx gy
                                 h = vegHash seed gx gy
                                 gxF'    = fromIntegral gx ∷ Float
                                 gyF'    = fromIntegral gy ∷ Float
                                 wx'     = valueNoise2D seed 101 gxF' gyF' 15.0 * 0.6
                                         + valueNoise2D seed 103 gxF' gyF' 6.0  * 0.4
                                 wy'     = valueNoise2D seed 102 gxF' gyF' 15.0 * 0.6
                                         + valueNoise2D seed 104 gxF' gyF' 6.0  * 0.4
                                 roll    = valueNoise2D seed 47 (gxF' + wx' * 3.0) (gyF' + wy' * 3.0) 10.0 + 0.5
                                 variant = fromIntegral ((h `shiftR` 8) .&. 0x03) ∷ Word8
                                 isOceanTile = elev ≤ seaLevel ∧ chunkOcean
                                 vegId = if isOceanTile ∨ isBarrenMaterial matId
                                         then vegNone
                                         else selectVegetation matId 0 False elev
                                                  temp precip humid snow roll variant
                             in (elev, matId, vegId, gx, gy)
                           | ly ← [0 .. chunkSize - 1]
                           , lx ← [0 .. chunkSize - 1]
                           ]

                -- Elevation vector for ocean flood fill and coastal pass
                -- Accurate post-erosion elevation from generateZoomTerrain
                elevRaw = zoomElev

                -- Per-tile ocean flags via flood fill from the ocean BFS.
                -- Seed from: (a) all ≤seaLevel tiles in ocean-range chunks,
                -- (b) edge tiles facing ocean-range neighbor chunks.
                -- Then flood-fill to connected ≤seaLevel tiles.
                -- This gives per-tile ocean data (no chunk boundary) while
                -- filtering inland noise dips (not connected to ocean).
                --
                -- Edge seeds use a relaxed threshold (seaLevel+3) to
                -- compensate for the detail world's contour smoothing
                -- which lowers near-coast tiles below seaLevel.  Without
                -- this, coastal chunks where the zoom's raw elevation sits
                -- slightly above seaLevel show as entirely land, creating
                -- blocky chunk-aligned coastlines.
                oceanFlags = runST $ do
                    let csA = chunkSize * chunkSize
                    mask ← VUM.replicate csA (0 ∷ Int)
                    changed ← VUM.new 1
                    let seed idx = do
                            old ← VUM.read mask idx
                            when (old ≡ 0 ∧ elevRaw VU.! idx ≤ seaLevel) $ do
                                VUM.write mask idx 1
                                VUM.write changed 0 (1 ∷ Int)
                    -- Seed from ocean + near-ocean chunks (distance 2).
                    -- Lakes are protected by hasLakeOrRiver in the renderer.
                    when chunkOcean $
                        forM_ [0 .. csA - 1] $ \idx → seed idx
                    -- Seed edges facing ocean-range neighbor chunks.
                    -- Strict ≤seaLevel only — no relaxation needed since
                    -- generateZoomTerrain uses the full detail pipeline.
                    when chunkOceanN $ forM_ [0 .. chunkSize - 1] $ \x → seed x
                    when chunkOceanS $ forM_ [0 .. chunkSize - 1] $ \x →
                        seed ((chunkSize - 1) * chunkSize + x)
                    when chunkOceanW $ forM_ [0 .. chunkSize - 1] $ \y → seed (y * chunkSize)
                    when chunkOceanE $ forM_ [0 .. chunkSize - 1] $ \y →
                        seed (y * chunkSize + chunkSize - 1)
                    -- Flood fill: extend to connected ≤seaLevel tiles
                    -- (strict threshold — only truly below-seaLevel tiles
                    -- propagate, preventing blue speckles on land).
                    let fillPass = do
                            VUM.write changed 0 (0 ∷ Int)
                            forM_ [0 .. csA - 1] $ \idx → do
                                m ← VUM.read mask idx
                                when (m ≡ 0 ∧ elevRaw VU.! idx ≤ seaLevel) $ do
                                    let lx = idx `mod` chunkSize
                                        ly = idx `div` chunkSize
                                        chk nx ny
                                          | nx < 0 ∨ nx ≥ chunkSize
                                            ∨ ny < 0 ∨ ny ≥ chunkSize = return False
                                          | otherwise = (≡ 1) ⊚ VUM.read mask
                                                            (ny * chunkSize + nx)
                                    hasN ← chk lx (ly-1)
                                    hasS ← chk lx (ly+1)
                                    hasW ← chk (lx-1) ly
                                    hasE ← chk (lx+1) ly
                                    when (hasN ∨ hasS ∨ hasW ∨ hasE) $ do
                                        VUM.write mask idx 1
                                        VUM.write changed 0 (1 ∷ Int)
                        loop 0 = return ()
                        loop n = do
                            fillPass
                            c ← VUM.read changed 0
                            when (c ≡ 1) $ loop (n - 1)
                    loop (32 ∷ Int)
                    -- Extend ocean 1 tile into coast: mark tiles at
                    -- seaLevel+1/+2 as ocean if adjacent to an existing
                    -- ocean tile.  Matches the detail world's Pass 2
                    -- (which extends ocean for side-face rendering).
                    -- Unlike the old seedEdge relaxation, this only
                    -- extends from CONFIRMED ocean tiles, not chunk edges.
                    forM_ [0 .. csA - 1] $ \idx → do
                        m ← VUM.read mask idx
                        when (m ≡ 0 ∧ elevRaw VU.! idx ≤ seaLevel + 2) $ do
                            let lx = idx `mod` chunkSize
                                ly = idx `div` chunkSize
                                chk nx ny
                                  | nx < 0 ∨ nx ≥ chunkSize
                                    ∨ ny < 0 ∨ ny ≥ chunkSize = return False
                                  | otherwise = (≡ 1) ⊚ VUM.read mask
                                                    (ny * chunkSize + nx)
                            hasN ← chk lx (ly-1)
                            hasS ← chk lx (ly+1)
                            hasW ← chk (lx-1) ly
                            hasE ← chk (lx+1) ly
                            when (hasN ∨ hasS ∨ hasW ∨ hasE) $
                                VUM.write mask idx 1
                    VU.unsafeFreeze mask

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
                chunkLava = hasAnyLavaQuick features seed plates worldSize coord avgElev
                eventRivers = concatMap extractEventRivers' (gtPeriods timeline)
                chunkRiver = hasAnyRiverQuick eventRivers worldSize coord
                chunkLake  = hasAnyLakeQuick features worldSize coord
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
                    , zceHasRiver = chunkRiver
                    , zceHasLake  = chunkLake
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
                    | (td, idx') ← zip tileData [0 ∷ Int ..]
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
               , (chunkLava, chunkIceMap, tileDataWithIce, oceanFlags, elevVec))

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
        extendAndRender ((ccx, ccy), (entry, tileData0, rawFluid, extras)) =
            let coord = ChunkCoord ccx ccy
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
                            let (_, _, _, _, elevV) = extras
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

                (chunkLava0, chunkIceMap0, tileDataWithIce0, oceanFlags0, _) = extras
                tileVec = V.fromList tileDataWithIce0
                pixels = generateChunkPixels palette chunkLava0
                             worldSize extendedFluid chunkIceMap0 oceanFlags0 tileVec
            in (entry, pixels)

        results = map extendAndRender (zip uniqueCoords pass1Results)
        (entries, pixelDatas) = unzip results
    in (V.fromList entries, V.fromList pixelDatas)

-- | Generate zoomTileSize×zoomTileSize RGBA pixel data for a single chunk.
--   Uses inverse isometric transform to map screen-space pixels
--   back to grid tiles, producing a diamond-shaped image.
generateChunkPixels ∷ ZoomColorPalette → Bool
                    → Int → FluidMap → IceMap → VU.Vector Int
                    → V.Vector (Int, Word8, Word8, Int, Int)
                    → BS.ByteString
generateChunkPixels palette hasLava worldSize fluidMap iceMap oceanFlags tileVec =
    BL.toStrict $ BB.toLazyByteString $ mconcat
        [ pixelAt px py
        | py ← [0 .. zoomTileSize - 1]
        , px ← [0 .. zoomTileSize - 1]
        ]
  where
    cs ∷ Float
    cs = fromIntegral chunkSize
    ts ∷ Float
    ts = fromIntegral zoomTileSize

    pixelAt ∷ Int → Int → BB.Builder
    pixelAt px py =
        let u = (fromIntegral px + 0.5) / ts
            v = (fromIntegral py + 0.5) / ts
            -- Inverse isometric: UV bounding box → grid-local coords
            gxF = cs * (u + v - 0.5)
            gyF = cs * (v - u + 0.5)
            lx = floor gxF ∷ Int
            ly = floor gyF ∷ Int
        in if lx < 0 ∨ lx ≥ chunkSize ∨ ly < 0 ∨ ly ≥ chunkSize
           then BB.word8 0 <> BB.word8 0 <> BB.word8 0 <> BB.word8 0
           else let idx = ly * chunkSize + lx
                    (elev, matId, vegId, gx, gy) = tileVec V.! idx
               -- Beyond-glacier tiles (minBound elevation) are outside the
               -- world diamond — render them transparent so ocean fluid
               -- doesn't bleed through at the south pole boundary.
               in if elev ≡ minBound
               -- Beyond-glacier tiles are outside the world diamond —
               -- render as glacier ice (snow veg color) so the boundary
               -- is solid and matches adjacent glacier zone tiles.
               then let (gr, gg, gb, ga) = case lookupVegColorById palette 65 of
                            Just c  → c
                            Nothing → (169, 189, 199, 255)
                    in BB.word8 gr <> BB.word8 gg <> BB.word8 gb <> BB.word8 ga
               else
                let tileIsOcean = case fluidMap V.! idx of
                        Just (FluidCell Ocean surf) → elev ≤ surf
                        _ → False
                    hasIce = isJust (iceMap V.! idx)
                    baseColor
                      | tileIsOcean ∧ not hasIce = defaultOceanColor
                      | otherwise = tileColor palette hasLava
                                        matId vegId elev gx gy
                    (r, g, b, a)
                        | hasIce = baseColor
                        | otherwise = case fluidMap V.! idx of
                            Just fc | fcType fc ≢ Ocean →
                                let blend = 0.7 ∷ Float
                                    (lr, lg, lb, la) = baseColor
                                in ( round (fromIntegral lr * (1.0 - blend)
                                          + 50.0 * blend ∷ Float)
                                   , round (fromIntegral lg * (1.0 - blend)
                                          + 90.0 * blend ∷ Float)
                                   , round (fromIntegral lb * (1.0 - blend)
                                          + 170.0 * blend ∷ Float)
                                   , la )
                            _ → baseColor
                in BB.word8 r <> BB.word8 g <> BB.word8 b <> BB.word8 a

-- | Determine the color for a single non-ocean tile.
--   Ocean rendering is handled by the per-tile oceanFlags flood fill
--   in generateChunkPixels, not here. This avoids per-chunk gates.
tileColor ∷ ZoomColorPalette → Bool → Word8 → Word8 → Int → Int → Int
          → (Word8, Word8, Word8, Word8)
tileColor palette hasLava matId vegId elev _gx _gy
    -- Snow-covered tiles (including frozen ocean) use snow color
    | isSnowVeg vegId =
        case lookupVegColorById palette vegId of
            Just vegColor → vegColor
            Nothing       → (220, 225, 235, 255)
    | otherwise =
        case lookupVegColorById palette vegId of
            Just vegColor → blendVegMat vegId vegColor (lookupMatColor palette matId)
            Nothing       → lookupMatColor palette matId

-- | Blend vegetation color with material color based on veg type density.
blendVegMat ∷ Word8 → (Word8,Word8,Word8,Word8) → (Word8,Word8,Word8,Word8)
            → (Word8,Word8,Word8,Word8)
blendVegMat vegId (vr,vg,vb,va) (mr,mg,mb,ma) =
    let vegWeight = vegDensityWeight vegId
        matWeight = 1.0 - vegWeight
        blend v m = round (fromIntegral v * vegWeight + fromIntegral m * matWeight)
    in (blend vr mr, blend vg mg, blend vb mb, max va ma)

-- | Determine vegetation blend weight from the veg type.
vegDensityWeight ∷ Word8 → Float
vegDensityWeight 0 = 0.0
vegDensityWeight vegId =
    let base = ((vegId - 1) `div` vegVariants) * vegVariants + 1
    in case base of
        1  → 0.3   -- sparse grass
        5  → 0.5   -- medium grass
        9  → 0.8   -- dense grass
        13 → 0.6   -- tall grass
        17 → 0.3   -- thin moss
        21 → 0.6   -- thick moss
        25 → 0.3   -- light ivy
        29 → 0.6   -- heavy ivy
        33 → 0.2   -- lichen
        37 → 0.2   -- desert scrub
        41 → 0.6   -- marsh grass
        45 → 0.3   -- dead grass
        49 → 0.4   -- fallen leaves
        53 → 0.5   -- pine needles
        57 → 0.3   -- mushroom patch
        61 → 0.4   -- wildflowers
        65 → 0.9   -- snow
        69 → 0.8   -- desert sand
        73 → 0.7   -- gravel tundra
        _  → 0.3

-- | Check if a vegetation ID is snow (IDs 65-68).
isSnowVeg ∷ Word8 → Bool
isSnowVeg v = v ≥ 65 ∧ v ≤ 68

-- * Per-Pixel River Rendering


-- * Ice Noise (zoom-level, continuous across chunk boundaries)

-- | Smooth noise for zoom-level ice boundaries.
--   Uses larger scales than tile-level noise so the ice edge
--   is smooth at the zoomed-out view. Returns ±2°C.
zoomIceNoise ∷ Word64 → Int → Int → Float
zoomIceNoise seed gx gy =
    let -- Must match iceNoise (Ice.hs) and iceLevelNoise (IceLevel.hs)
        -- so frozen classification is consistent across zoom/chunk/grid.
        h1 = zoomIceHash seed gx gy 12
        h2 = zoomIceHash seed gx gy 5
        n1 = (zoomHashToFloat h1 - 0.5) * 3.0
        n2 = (zoomHashToFloat h2 - 0.5) * 1.0
    in n1 + n2

zoomIceHash ∷ Word64 → Int → Int → Int → Word64
zoomIceHash seed gx gy scale =
    let fx = fromIntegral gx / fromIntegral scale ∷ Float
        fy = fromIntegral gy / fromIntegral scale ∷ Float
        ix = floor fx ∷ Int
        iy = floor fy ∷ Int
        tx = fx - fromIntegral ix
        ty = fy - fromIntegral iy
        sx = zoomSmoothstep tx
        sy = zoomSmoothstep ty
        v00 = zoomTileHash seed ix       iy
        v10 = zoomTileHash seed (ix + 1) iy
        v01 = zoomTileHash seed ix       (iy + 1)
        v11 = zoomTileHash seed (ix + 1) (iy + 1)
        f00 = zoomHashToFloat v00
        f10 = zoomHashToFloat v10
        f01 = zoomHashToFloat v01
        f11 = zoomHashToFloat v11
        top    = f00 + sx * (f10 - f00)
        bottom = f01 + sx * (f11 - f01)
        result = top + sy * (bottom - top)
    in round (result * fromIntegral (0xFFFFFF ∷ Int)) ∷ Word64

zoomTileHash ∷ Word64 → Int → Int → Word64
zoomTileHash seed x y =
    let h0 = seed `xor` 0x1CE1CE1CE
        h1 = h0 `xor` (fromIntegral x * 0x517cc1b727220a95)
        h2 = h1 `xor` (fromIntegral y * 0x6c62272e07bb0142)
        h3 = h2 `xor` (h2 `shiftR` 33)
        h4 = h3 * 0xff51afd7ed558ccd
        h5 = h4 `xor` (h4 `shiftR` 33)
    in h5

zoomHashToFloat ∷ Word64 → Float
zoomHashToFloat h = fromIntegral (h .&. 0x00FFFFFF) / fromIntegral (0x00FFFFFF ∷ Word64)

zoomSmoothstep ∷ Float → Float
zoomSmoothstep t = t * t * (3.0 - 2.0 * t)

-- * Coastal Contour Smoothing (zoom map)

-- | Lightweight coastal terrain flattening for the zoom map.
--   Simplified version of smoothCoastalContour (Coastal.hs) without
--   border overlap or hardness awareness. Flattens terrain near
--   seaLevel by pulling toward minimum neighbor, creating gradual
--   coastal slopes for the minimap.
smoothZoomContour ∷ Int → Int → VU.Vector Int → VU.Vector Int
smoothZoomContour 0 _ elev = elev
smoothZoomContour iters cs elev =
    let csA = cs * cs
        bandWidth = 60 ∷ Int
        fadeStart = 45 ∷ Int
        smoothed = runST $ do
            em ← VUM.new csA
            forM_ [0 .. csA - 1] $ \i →
                VUM.write em i (elev VU.! i)
            forM_ [0 .. csA - 1] $ \idx → do
                let e  = elev VU.! idx
                    bx = idx `mod` cs
                    by = idx `div` cs
                    absD = abs (e - seaLevel)
                when (absD < bandWidth
                     ∧ bx > 0 ∧ bx < cs - 1
                     ∧ by > 0 ∧ by < cs - 1) $ do
                    let elevFade = if absD < fadeStart then 1.0
                                   else 1.0 - fromIntegral (absD - fadeStart)
                                            / fromIntegral (bandWidth - fadeStart) ∷ Float
                        readN nx ny
                            | nx ≥ 0 ∧ nx < cs ∧ ny ≥ 0 ∧ ny < cs
                                = elev VU.! (ny * cs + nx)
                            | otherwise = e
                        n  = readN bx       (by - 1)
                        s  = readN bx       (by + 1)
                        w  = readN (bx - 1) by
                        eN = readN (bx + 1) by
                        minN = min n (min s (min w eN))
                        avg  = (n + s + w + eN + e) `div` 5
                        target
                          | e > seaLevel = min avg (minN + 2)
                          | otherwise    = avg
                        delta = fromIntegral (target - e) ∷ Float
                        newE = e + round (elevFade * delta)
                    VUM.write em idx newE
            VU.unsafeFreeze em
    in smoothZoomContour (iters - 1) cs smoothed

-- * Helpers

wrapChunkX ∷ Int → Int → Int
wrapChunkX halfSize cx =
    let w = halfSize * 2
    in ((cx + halfSize) `mod` w + w) `mod` w - halfSize

wrapChunkY ∷ Int → Int → Int
wrapChunkY halfSize cy = max (-halfSize) (min (halfSize - 1) cy)

majorityMaterial ∷ [(Int, Word8)] → Word8
majorityMaterial samples =
    let counts = foldl' (\m (_, mat) → Map.insertWith (+) mat (1 ∷ Int) m)
                        Map.empty samples
        -- Prefer land materials over ocean/air (matId=0).
        -- A coastal chunk with 40% land should show as land in the
        -- preview, since the per-chunk zoom pixel texture already
        -- renders the per-tile detail correctly.
        landCounts = Map.delete 0 counts
        pickBest m = Map.foldlWithKey' (\(bestMat, bestCount) mat count →
            if count > bestCount ∨ (count ≡ bestCount ∧ mat > bestMat)
            then (mat, count)
            else (bestMat, bestCount)
            ) (0, 0) m
        (landWinner, landCount) = pickBest landCounts
        (anyWinner, _) = pickBest counts
    in if landCount > 0 then landWinner else anyWinner

-- * Vegetation Category

-- | Climate-based vegetation density:
--   0 = none (barren rock, ocean, glacier),
--   1 = sparse (tundra, arid scrub),
--   2 = medium (temperate grassland),
--   3 = dense (lush grass, tropical),
--   4 = marsh/wetland.

vegCategoryFromClimate ∷ ClimateState → Int → Int → Int → Word8 → Word8
vegCategoryFromClimate climate worldSize baseGX baseGY matId
    | isBarrenMaterial matId = 0
    | isWetlandSoil matId   = 4
    | otherwise =
        let -- Sample at chunk center
            gx = baseGX + chunkSize `div` 2
            gy = baseGY + chunkSize `div` 2
            LocalClimate{lcTemp=temp, lcPrecip=precip, lcSnow=snow} =
                lookupLocalClimate climate worldSize gx gy
        in if snow > 0.7           then 0
           else if temp < -5.0     then 1  -- tundra
           else if precip < 0.15   then 0  -- desert
           else if precip < 0.25   then 1  -- arid
           else if temp > 20.0 ∧ precip > 0.5 then 3  -- tropical
           else if temp > 10.0 ∧ precip > 0.4 then 3  -- warm wet
           else if temp > 5.0  ∧ precip > 0.3 then 2  -- temperate
           else 1  -- cool / sparse

-- | Extract RiverParams from all HydroEvents in a period's events.
extractEventRivers' ∷ GeoPeriod → [RiverParams]
extractEventRivers' period = concatMap go (gpEvents period)
  where
    go (HydroEvent (RiverFeature rp)) = [rp]
    go _ = []
