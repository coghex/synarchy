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
                   , isBeyondGlacier, wrapGlobalU)
import qualified Data.HashMap.Strict as HM
import World.Fluids (isOceanChunk, hasAnyLavaQuick, hasAnyOceanFluid
                    , hasAnyRiverQuick, hasAnyLakeQuick)
import World.Generate (applyTimelineFast)
import Data.Bits ((.&.), shiftR)
import World.Vegetation (isBarrenMaterial, isWetlandSoil
                        , selectVegetation, vegHash, vegNone, vegVariants)
import World.Weather.Types (ClimateState(..), ClimateGrid(..)
                           , RegionClimate(..), SeasonalClimate(..)
                           , ClimateCoord(..), climateRegionSize)
import World.ZoomMap.ColorPalette (ZoomColorPalette, lookupMatColor
                                  , lookupVegColorById
                                  , defaultOceanColor, defaultLavaColor)

-----------------------------------------------------------
-- Sampling Configuration
-----------------------------------------------------------

sampleGridSize ∷ Int
sampleGridSize = 5

sampleOffsets ∷ [(Int, Int)]
sampleOffsets =
    let step = max 1 (chunkSize `div` (sampleGridSize + 1))
    in [ (sx * step, sy * step)
       | sx ← [1 .. sampleGridSize]
       , sy ← [1 .. sampleGridSize]
       ]

-----------------------------------------------------------
-- Build Zoom Cache (called once at world init)
-----------------------------------------------------------

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
                chunkRiver = hasAnyRiverQuick features worldSize coord
                chunkLake  = hasAnyLakeQuick features worldSize coord

                -- Vegetation category from climate
                vegCat = if chunkOcean ∨ winnerMat ≡ 250
                         then 0
                         else vegCategoryFromClimate climate worldSize
                                  baseGX baseGY winnerMat
            in ZoomChunkEntry
                { zceChunkX = ccx
                , zceChunkY = ccy
                , zceBaseGX = baseGX
                , zceBaseGY = baseGY
                , zceTexIndex = if isOceanChunk oceanMap coord
                                then 0
                                else winnerMat
                , zceElev     = if isOceanChunk oceanMap coord
                                then seaLevel else avgElev
                , zceIsOcean  = chunkOcean
                , zceHasLava  = chunkLava
                , zceHasRiver = chunkRiver
                , zceHasLake  = chunkLake
                , zceVegCategory = vegCat
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

-----------------------------------------------------------
-- Build Zoom Cache + Per-Chunk Pixel Data
-----------------------------------------------------------

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
                                 (temp, precip, humid, snow) =
                                     lookupClimateQuick climate worldSize gx gy
                                 h = vegHash seed gx gy
                                 roll    = fromIntegral (h .&. 0xFF) / 255.0 ∷ Float
                                 variant = fromIntegral ((h `shiftR` 8) .&. 0x03) ∷ Word8
                                 vegId = if chunkOcean ∨ isBarrenMaterial matId
                                         then vegNone
                                         else selectVegetation matId 0 False elev
                                                  temp precip humid snow roll variant
                             in (elev, matId, vegId, gx, gy)
                           | ly ← [0 .. chunkSize - 1]
                           , lx ← [0 .. chunkSize - 1]
                           ]

                -- Summary stats from all tiles (for ZoomChunkEntry)
                allMats = [ (e, m) | (e, m, _, _, _) ← tileData ]
                winnerMat = majorityMaterial allMats
                avgElev = let s = sum (map fst allMats)
                          in s `div` length allMats
                chunkLava = hasAnyLavaQuick features seed plates worldSize coord avgElev
                chunkRiver = hasAnyRiverQuick features worldSize coord
                chunkLake  = hasAnyLakeQuick features worldSize coord
                vegCat = if chunkOcean ∨ winnerMat ≡ 250
                         then 0
                         else vegCategoryFromClimate climate worldSize
                                  baseGX baseGY winnerMat

                entry = ZoomChunkEntry
                    { zceChunkX = ccx
                    , zceChunkY = ccy
                    , zceBaseGX = baseGX
                    , zceBaseGY = baseGY
                    , zceTexIndex = if isOceanChunk oceanMap coord
                                    then 0
                                    else winnerMat
                    , zceElev     = if isOceanChunk oceanMap coord
                                    then seaLevel else avgElev
                    , zceIsOcean  = chunkOcean
                    , zceHasLava  = chunkLava
                    , zceHasRiver = chunkRiver
                    , zceHasLake  = chunkLake
                    , zceVegCategory = vegCat
                    }

                -- Generate zoomTileSize×zoomTileSize RGBA pixel data (isometric)
                tileVec = V.fromList tileData
                pixels = generateChunkPixels palette chunkOcean chunkLava tileVec

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
                    → V.Vector (Int, Word8, Word8, Int, Int)
                    → BS.ByteString
generateChunkPixels palette isOcean hasLava tileVec =
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
                    (r, g, b, a) = tileColor palette isOcean hasLava
                                       matId vegId elev gx gy
                in BB.word8 r <> BB.word8 g <> BB.word8 b <> BB.word8 a

-- | Determine the color for a single tile.
tileColor ∷ ZoomColorPalette → Bool → Bool → Word8 → Word8 → Int → Int → Int
          → (Word8, Word8, Word8, Word8)
tileColor palette isOcean hasLava matId vegId elev gx gy
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
        _  → 0.3

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

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
        (winner, _) = Map.foldlWithKey' (\(bestMat, bestCount) mat count →
            if count > bestCount ∨ (count ≡ bestCount ∧ mat > bestMat)
            then (mat, count)
            else (bestMat, bestCount)
            ) (0, 0) counts
    in winner

-----------------------------------------------------------
-- Vegetation Category (simplified climate-based)
--
-- 0 = none (barren rock, ocean, glacier)
-- 1 = sparse (tundra, arid scrub)
-- 2 = medium (temperate grassland)
-- 3 = dense (lush grass, tropical)
-- 4 = marsh/wetland
-----------------------------------------------------------

vegCategoryFromClimate ∷ ClimateState → Int → Int → Int → Word8 → Word8
vegCategoryFromClimate climate worldSize baseGX baseGY matId
    | isBarrenMaterial matId = 0
    | isWetlandSoil matId   = 4
    | otherwise =
        let -- Sample at chunk center
            gx = baseGX + chunkSize `div` 2
            gy = baseGY + chunkSize `div` 2
            (temp, precip, _, snow) = lookupClimateQuick climate worldSize gx gy
        in if snow > 0.7           then 0
           else if temp < -5.0     then 1  -- tundra
           else if precip < 0.15   then 0  -- desert
           else if precip < 0.25   then 1  -- arid
           else if temp > 20.0 ∧ precip > 0.5 then 3  -- tropical
           else if temp > 10.0 ∧ precip > 0.4 then 3  -- warm wet
           else if temp > 5.0  ∧ precip > 0.3 then 2  -- temperate
           else 1  -- cool / sparse

-- | Climate lookup with bilinear interpolation between region centers.
--   Eliminates hard 4×4 chunk grid edges in vegetation/climate transitions.
lookupClimateQuick ∷ ClimateState → Int → Int → Int
                   → (Float, Float, Float, Float)
lookupClimateQuick climate worldSize gx gy =
    let regions = cgRegions (csClimate climate)
        halfChunks = worldSize `div` 2
        w = worldSize * chunkSize
        halfW = w `div` 2
        u = gx - gy
        v = gx + gy
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW

        -- Continuous region coordinates (tile → chunk → region)
        fCS = fromIntegral chunkSize ∷ Float
        fCRS = fromIntegral climateRegionSize ∷ Float
        hcF = fromIntegral halfChunks ∷ Float
        ruF = (fromIntegral wrappedU / fCS + hcF) / fCRS
        rvF = (fromIntegral v / fCS + hcF) / fCRS

        -- Center-based: region center is at integer + 0.5
        regionsPerSide = worldSize `div` climateRegionSize
        ruC = ruF - 0.5
        rvC = rvF - 0.5
        ru0raw = floor ruC ∷ Int
        rv0 = floor rvC ∷ Int
        ru0 = ((ru0raw `mod` regionsPerSide) + regionsPerSide) `mod` regionsPerSide
        ru1 = (ru0 + 1) `mod` regionsPerSide
        rv1 = rv0 + 1
        tu = ruC - fromIntegral ru0raw
        tv = rvC - fromIntegral rv0

        lookupRC ru rv =
            case HM.lookup (ClimateCoord ru rv) regions of
                Just rc →
                    let SeasonalClimate st wt = rcAirTemp rc
                        SeasonalClimate sp wp = rcPrecipitation rc
                    in ((st + wt) / 2.0, (sp + wp) / 2.0
                       , rcHumidity rc, rcPrecipType rc)
                Nothing →
                    (csGlobalTemp climate, 0.5, 0.5, 0.0)

        (t00, p00, h00, s00) = lookupRC ru0 rv0
        (t10, p10, h10, s10) = lookupRC ru1 rv0
        (t01, p01, h01, s01) = lookupRC ru0 rv1
        (t11, p11, h11, s11) = lookupRC ru1 rv1

        lerpF a b t = a + t * (b - a)
        temp   = lerpF (lerpF t00 t10 tu) (lerpF t01 t11 tu) tv
        precip = lerpF (lerpF p00 p10 tu) (lerpF p01 p11 tu) tv
        humid  = lerpF (lerpF h00 h10 tu) (lerpF h01 h11 tu) tv
        snow   = lerpF (lerpF s00 s10 tu) (lerpF s01 s11 tu) tv
    in (temp, precip, humid, snow)
