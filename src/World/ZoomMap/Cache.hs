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
                   , isBeyondGlacier, wrapGlobalU
                   , twoNearestPlates, BoundaryType(..), classifyBoundary)
import qualified Data.Vector.Unboxed as VU
import World.Fluids (isOceanChunk, hasAnyLavaQuick, hasAnyOceanFluid
                    , hasAnyRiverQuick, hasAnyLakeQuick
                    , computeChunkFluid, computeChunkRivers
                    , computeChunkLakes, unionFluidMap)
import World.Fluid.Types (FluidCell(..), FluidType(..), IceCell(..), IceMap)
import World.Fluid.Internal (FluidMap)
import World.Geology.Timeline.Types (GeoEvent(..))
import World.Hydrology.Types (HydroFeature(..), RiverParams(..))
import World.Generate (applyTimelineFast)
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
import World.Geology.Coastal (CoastType(..), classifyCoast, isDepositional
                             , beachMaterial, wetlandMaterial, deltaMaterial
                             , coastHash, sandProfile, outcroppHardness
                             , maxCoastalDist, filterNearbyMouths
                             , isNearRiverMouth)
import qualified Data.Vector.Unboxed.Mutable as VUM
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
                          ∨ hasAnyOceanFluid oceanMap coord
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
                altAboveSea = max 0 (avgElev - seaLevel)
                altCool = fromIntegral altAboveSea * (0.065 ∷ Float)
                ocnPen = if chunkOcean then 5.0 else 0.0 ∷ Float
                iceNoise = zoomIceNoise seed cgx cgy
                effT = cmt + ocnPen - altCool + iceNoise
                chunkIce = effT < -2.0
                         ∨ (cwt - altCool < -10.0 ∧ cst - altCool < 5.0)
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
                           ∨ hasAnyOceanFluid oceanMap coord

                -- Compute material + veg at every tile position
                tileData = [ let gx = baseGX + lx
                                 gy = baseGY + ly
                                 (gx', gy') = wrapGlobalU worldSize gx gy
                                 (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gx' gy'
                                 hardness = mpHardness (getMaterialProps registry baseMat)
                                 (elev, matId) =
                                     if baseMat ≡ matGlacier
                                     then (baseElev, unMaterialId baseMat)
                                     else if baseElev < -100 then (baseElev, 0)
                                     else let (e, m) = applyTimelineFast timeline
                                                            worldSize gx' gy'
                                                            hardness
                                                            (baseElev, baseMat)
                                          in (e, unMaterialId m)
                                 -- Per-tile vegetation using actual selectVegetation
                                 LocalClimate{lcTemp=temp, lcPrecip=precip
                                             , lcHumidity=humid, lcSnow=snow} =
                                     lookupLocalClimate climate worldSize gx gy
                                 h = vegHash seed gx gy
                                 roll    = fromIntegral (h .&. 0xFF) / 255.0 ∷ Float
                                 variant = fromIntegral ((h `shiftR` 8) .&. 0x03) ∷ Word8
                                 isOceanTile = elev < seaLevel
                                                   ∧ isOceanChunk oceanMap coord
                                 vegId = if isOceanTile ∨ isBarrenMaterial matId
                                         then vegNone
                                         else selectVegetation matId 0 False elev
                                                  temp precip humid snow roll variant
                             in (elev, matId, vegId, gx, gy)
                           | ly ← [0 .. chunkSize - 1]
                           , lx ← [0 .. chunkSize - 1]
                           ]

                -- Coastal material pass: mimic applyCoastalErosion
                -- for the zoom map by building a BFS distance field
                -- and applying the same material selection.
                tileData' = if not chunkOcean then tileData
                    else let cs = chunkSize
                             csA = cs * cs
                             elevArr = VU.fromList [ e | (e, _, _, _, _) ← tileData ]
                             -- BFS distance from ocean tiles
                             distField = runST $ do
                                 distM ← VUM.replicate csA (maxCoastalDist + 1)
                                 seedsM ← VUM.new csA
                                 scRef ← VUM.new 1
                                 VUM.write scRef 0 (0 ∷ Int)
                                 forM_ [0 .. csA - 1] $ \idx → do
                                     let e = elevArr VU.! idx
                                     when (e ≤ seaLevel) $ do
                                         VUM.write distM idx 0
                                         sc ← VUM.read scRef 0
                                         VUM.write seedsM sc idx
                                         VUM.write scRef 0 (sc + 1)
                                 sc0 ← VUM.read scRef 0
                                 let bfs curSeeds curCount level =
                                         when (level ≤ maxCoastalDist ∧ curCount > 0) $ do
                                             nextM ← VUM.new csA
                                             ncRef ← VUM.new 1
                                             VUM.write ncRef 0 (0 ∷ Int)
                                             forM_ [0 .. curCount - 1] $ \si → do
                                                 idx ← VUM.read curSeeds si
                                                 let bx = idx `mod` cs
                                                     by = idx `div` cs
                                                     tryN nx ny =
                                                         when (nx ≥ 0 ∧ nx < cs
                                                              ∧ ny ≥ 0 ∧ ny < cs) $ do
                                                             let nIdx = ny * cs + nx
                                                             old ← VUM.read distM nIdx
                                                             when (old > level + 1) $ do
                                                                 VUM.write distM nIdx (level + 1)
                                                                 nc ← VUM.read ncRef 0
                                                                 VUM.write nextM nc nIdx
                                                                 VUM.write ncRef 0 (nc + 1)
                                                 tryN (bx - 1) by
                                                 tryN (bx + 1) by
                                                 tryN bx       (by - 1)
                                                 tryN bx       (by + 1)
                                             nc ← VUM.read ncRef 0
                                             bfs nextM nc (level + 1)
                                 bfs seedsM sc0 0
                                 VU.unsafeFreeze distM
                             -- Plate classification at chunk center
                             ChunkCoord cx' cy' = coord
                             cGX = cx' * cs + cs `div` 2
                             cGY = cy' * cs + cs `div` 2
                             (cGX', cGY') = wrapGlobalU worldSize cGX cGY
                             ((pA, dA), (pB, dB)) =
                                 twoNearestPlates seed worldSize plates cGX' cGY'
                             cBoundary = classifyBoundary worldSize pA pB
                             cBDist = (dB - dA) / 2.0
                             maxBD = fromIntegral worldSize * 4.0 ∷ Float
                             cBFade = min 1.0 (abs cBDist / maxBD)
                             -- River mouths near this chunk
                             allMouths = concatMap (\p →
                                 [ (mx, my)
                                 | HydroEvent (RiverFeature rp) ← gpEvents p
                                 , let GeoCoord mx my = rpMouthRegion rp
                                 ]) (gtPeriods timeline)
                             nearMouths = filterNearbyMouths worldSize coord allMouths
                         in zipWith (\idx (e, m, v, gx, gy) →
                             let dist = distField VU.! idx
                             in if dist ≤ 0 ∨ dist > maxCoastalDist
                                then (e, m, v, gx, gy)
                                else let hardness = mpHardness
                                            (getMaterialProps registry (MaterialId m))
                                         nearRiver = isNearRiverMouth worldSize
                                            nearMouths gx gy
                                         coastType = classifyCoast cBoundary
                                            (plateIsLand pA) (plateIsLand pB)
                                            hardness 0.0 cBFade nearRiver
                                         lh = coastHash seed gx gy
                                         roll = fromIntegral (lh .&. 0xFF)
                                                / 255.0 ∷ Float
                                         sandLevel = sandProfile dist
                                         isOutcrop = hardness ≥ outcroppHardness
                                                   ∧ e > sandLevel
                                         newMat
                                           | isDepositional coastType ∧ not isOutcrop
                                             = case coastType of
                                                 _ | isDepositional coastType →
                                                     case coastType of
                                                       DepositionalWetland →
                                                           wetlandMaterial dist roll
                                                       DeltaicCoast →
                                                           deltaMaterial dist roll
                                                       _ → beachMaterial dist m roll
                                                 _ → m
                                           | otherwise = m
                                     in if newMat ≡ m
                                        then (e, m, v, gx, gy)
                                        else let h' = vegHash seed gx gy
                                                 r' = fromIntegral (h' .&. 0xFF)
                                                      / 255.0 ∷ Float
                                                 var' = fromIntegral
                                                    ((h' `shiftR` 8) .&. 0x03) ∷ Word8
                                                 LocalClimate{lcTemp=t, lcPrecip=p
                                                             , lcHumidity=hu, lcSnow=sn} =
                                                     lookupLocalClimate climate
                                                         worldSize gx gy
                                                 v' = selectVegetation newMat 0 False e
                                                          t p hu sn r' var'
                                             in (e, newMat, v', gx, gy)
                            ) [0 ∷ Int ..] tileData

                -- Summary stats from all tiles (for ZoomChunkEntry)
                allMats = [ (e, m) | (e, m, _, _, _) ← tileData' ]
                winnerMat = majorityMaterial allMats
                avgElev = let s = sum (map fst allMats)
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
                altAboveSea' = max 0 (avgElev - seaLevel)
                altCool' = fromIntegral altAboveSea' * (0.065 ∷ Float)
                ocnPen' = if chunkOcean then 5.0 else 0.0 ∷ Float
                iceNoise' = zoomIceNoise seed cgx' cgy'
                effT' = cmt' + ocnPen' - altCool' + iceNoise'
                chunkIce' = effT' < -2.0
                          ∨ (cwt' - altCool' < -10.0 ∧ cst' - altCool' < 5.0)

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

                -- Compute per-tile fluid: build a terrain surface map from
                -- the already-computed tile elevations, then run the fluid
                -- pipeline (ocean + river + lake) to get actual water data.
                terrainSurfMap = VU.fromList
                    [ e | (e, _, _, _, _) ← tileData' ]
                oceanFluid = computeChunkFluid worldSize oceanMap
                                 coord terrainSurfMap
                riverFluid = computeChunkRivers eventRivers worldSize
                                 coord terrainSurfMap
                lakeFluid = computeChunkLakes features seed plates worldSize
                                 coord terrainSurfMap
                chunkFluidMap = unionFluidMap riverFluid
                              $ unionFluidMap lakeFluid oceanFluid

                -- Ice overlay: per-tile decision using continuous noise
                -- that doesn't break at chunk boundaries.
                chunkIceMap = V.fromList
                    [ let (e, _, _, gx, gy) = td
                          (gx', gy') = wrapGlobalU worldSize gx gy
                          LocalClimate{lcTemp=mt, lcSummerTemp=st
                                      , lcWinterTemp=wt} =
                              lookupLocalClimate climate worldSize gx' gy'
                          altAboveSea = max 0 (e - seaLevel)
                          altCool = fromIntegral altAboveSea * (0.065 ∷ Float)
                          isOcn = case chunkFluidMap V.! idx' of
                              Just fc → fcType fc ≡ Ocean
                              Nothing → False
                          ocnPen = if isOcn then 5.0 else 0.0 ∷ Float
                          -- Smooth noise at large scale for zoom view
                          n = zoomIceNoise seed gx' gy'
                          effT = mt + ocnPen - altCool + n
                          ice = effT < -2.0
                              ∨ (wt - altCool < -10.0
                                 ∧ st - altCool < 5.0)
                      in if ice ∧ e > minBound
                         then Just (IceCell (e + 1))
                         else Nothing
                    | (td, idx') ← zip tileData' [0 ∷ Int ..]
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
                    ) [0 ∷ Int ..] tileData'

                -- Generate zoomTileSize×zoomTileSize RGBA pixel data (isometric)
                tileVec = V.fromList tileDataWithIce
                pixels = generateChunkPixels palette chunkOcean chunkLava
                             worldSize chunkFluidMap chunkIceMap tileVec

            in (entry, pixels)

        allCoords = [ let wrappedU = ((u + halfSize) `mod` w + w) `mod` w - halfSize
                          ccx = (wrappedU + v) `div` 2
                          ccy = (v - wrappedU) `div` 2
                      in (ccx, ccy)
                    | v ← [-halfSize .. halfSize - 1]
                    , u ← [-halfSize .. halfSize - 1]
                    , even (u + v)
                    ]

        uniqueCoords = Set.toList $ Set.fromList allCoords

        chunkBatchSize = max 1 (length uniqueCoords `div` 128)
        results = map buildOne uniqueCoords
                    `using` parListChunk chunkBatchSize rdeepseq
        (entries, pixelDatas) = unzip results
    in (V.fromList entries, V.fromList pixelDatas)

-- | Generate zoomTileSize×zoomTileSize RGBA pixel data for a single chunk.
--   Uses inverse isometric transform to map screen-space pixels
--   back to grid tiles, producing a diamond-shaped image.
generateChunkPixels ∷ ZoomColorPalette → Bool → Bool
                    → Int → FluidMap → IceMap
                    → V.Vector (Int, Word8, Word8, Int, Int)
                    → BS.ByteString
generateChunkPixels palette isOcean hasLava worldSize fluidMap iceMap tileVec =
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
                    baseColor = tileColor palette isOcean hasLava
                                    matId vegId elev gx gy
                    hasIce = isJust (iceMap V.! idx)
                    -- Check actual fluid map for this tile
                    (r, g, b, a)
                        -- Ice-covered: use the tile color which already
                        -- has snow veg injected from tileDataWithIce
                        | hasIce = baseColor
                        | otherwise = case fluidMap V.! idx of
                            Just fc | fcType fc ≢ Ocean →
                                -- River/Lake water: tint blue
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

-- | Determine the color for a single tile.
tileColor ∷ ZoomColorPalette → Bool → Bool → Word8 → Word8 → Int → Int → Int
          → (Word8, Word8, Word8, Word8)
tileColor palette isOcean hasLava matId vegId elev _gx _gy
    -- Snow-covered tiles (including frozen ocean) use snow color
    | isSnowVeg vegId =
        case lookupVegColorById palette vegId of
            Just vegColor → vegColor
            Nothing       → (220, 225, 235, 255)
    -- Any tile below sea level renders as water
    | elev < seaLevel ∧ (isOcean ∨ matId ≡ 0) = defaultOceanColor
    -- Lava only in actual depressions (below sea level)
    | hasLava ∧ matId ≡ 0 ∧ elev < seaLevel = defaultLavaColor
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
    let -- Large-scale noise for smooth continental-scale variation
        h1 = zoomIceHash seed gx gy 48
        h2 = zoomIceHash seed gx gy 20
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
