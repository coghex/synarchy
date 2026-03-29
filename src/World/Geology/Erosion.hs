{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Erosion
    ( applyErosion
    , erosionSediment
    , lookupRegionalErosion
    ) where

import UPrelude
import World.Types
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..))
import World.Geology.Types
import qualified Data.HashMap.Strict as HM
import World.Weather.Types (ClimateCoord(..))
import World.Weather.Lookup (RegionGridCoords(..), regionGridCoords)

-- * Erosion Application

-- | Apply time-based erosion to a tile position.
--   Smooths the tile's elevation toward the average of its
--   4 cardinal neighbors, gated by material hardness.
--
--   Four erosion modes combine to produce the final rate:
--
--   * Hydraulic (rainfall/runoff): dominant mode. Scales with
--     elevation difference (steeper = faster). Drives most
--     valley carving.
--
--   * Wind (aeolian): weak but steady. Less sensitive to slope,
--     flattens exposed surfaces. Strongest in arid regions.
--
--   * Thermal (freeze-thaw): shatters steep rock faces.
--     Scales with slope squared — very aggressive on cliffs
--     but negligible on gentle terrain.
--
--   * Chemical (dissolution): slow, uniform. Increases effective
--     erodability of softer rocks (limestone, shale). Barely
--     touches granite.
--
--   All four modes produce a delta toward the neighbor average.
--   They differ in how strongly they respond to slope and
--   material, which creates distinct erosion signatures:
--     - Wet climate:  deep V-valleys from hydraulic dominance
--     - Arid climate: gentle wind-smoothed plateaus
--     - Cold climate: jagged peaks from thermal shattering
--     - Warm/wet:     rounded limestone karst from chemical
applyErosion ∷ ErosionParams
             → Int       -- ^ worldSize
             → Int       -- ^ period duration (millions of years)
             → Float     -- ^ world scale factor (worldSize / 512)
             → Word8     -- ^ surface material ID at this tile
             → Float     -- ^ material hardness (0.0-1.0)
             → Int       -- ^ this tile's post-event elevation
             → (Int, Int, Int, Int)
                         -- ^ neighbor post-event elevations (N, S, E, W)
             → GeoModification
applyErosion params _worldSize duration worldScale matId hardness elev (nN, nS, nE, nW) = if hardness ≥ 1.0
       then noModification  -- indestructible (glacier, mantle)
       else
       let -- Average of 4 cardinal neighbors
           avgNeighbor = fromIntegral (nN + nS + nE + nW) / 4.0 ∷ Float
           diff = avgNeighbor - fromIntegral elev

           -- Common scaling factors
           erodability = 1.0 - hardness
           durationScale = fromIntegral duration / 5.0 ∷ Float
           scaleFactor = sqrt (max 0.1 worldScale)

           -- Slope magnitude: max absolute difference to any neighbor
           -- Used to modulate slope-sensitive erosion modes
           absDiff = abs diff
           slopeNorm = min 1.0 (absDiff / 30.0)  -- normalize: 30 tiles = max slope

           ---------------------------------------------------------
           -- Hydraulic erosion (rainfall/runoff)
           --   The dominant carver. Proportional to slope —
           --   water flows faster on steep terrain, carries more
           --   sediment. Flat areas barely erode hydraulically.
           --   epHydraulic is high when climate is wet (lots of rain).
           ---------------------------------------------------------
           hydraulicSlopeBoost = 0.4 + 0.6 * slopeNorm
               -- even flat terrain erodes a little (sheet wash),
               -- but steep terrain erodes 2.5× faster
           hydraulicRate = epHydraulic params
                         * erodability
                         * hydraulicSlopeBoost

           ---------------------------------------------------------
           -- Wind erosion (aeolian)
           --   Weak but nearly slope-independent. Grinds down
           --   exposed surfaces. Strongest in arid regions
           --   (high epWind). Barely affected by slope — wind
           --   hits everything equally.
           ---------------------------------------------------------
           windSlopeBoost = 0.8 + 0.2 * slopeNorm
               -- almost flat response, slight boost on ridges
           windRate = epWind params
                    * erodability
                    * windSlopeBoost

           ---------------------------------------------------------
           -- Thermal erosion (freeze-thaw)
           --   Shatters steep cliffs. Proportional to slope²
           --   so it's negligible on gentle terrain but very
           --   aggressive on cliffs. epThermal peaks in climates
           --   that cycle around 0°C.
           ---------------------------------------------------------
           thermalSlopeBoost = slopeNorm * slopeNorm
               -- squared: only bites on steep terrain
           thermalRate = epThermal params
                       * erodability
                       * thermalSlopeBoost

           ---------------------------------------------------------
           -- Chemical erosion (dissolution)
           --   Slow, uniform weathering. Dissolves soft rocks
           --   faster than hard ones. Makes soft rocks even
           --   softer (increases effective erodability beyond
           --   the material's base value). Not slope-dependent.
           --   epChemical is high when CO2 is high (acidic rain).
           ---------------------------------------------------------
           chemicalErodability = min 1.0 (erodability + epChemical params * 0.3)
               -- chemical weathering softens rock beyond base hardness
               -- e.g. limestone (hardness 0.4, erodability 0.6) at
               -- epChemical 0.5: effective erodability = 0.6 + 0.15 = 0.75
           chemicalRate = epChemical params
                        * chemicalErodability
                        * 0.5  -- intrinsically slower than hydraulic

           ---------------------------------------------------------
           -- Combined rate
           --   Sum of all modes, then scale by duration and world.
           --   Each mode contributes independently so a wet + cold
           --   world gets both hydraulic valleys AND thermal peaks.
           ---------------------------------------------------------
           combinedRate = (hydraulicRate + windRate + thermalRate + chemicalRate)
                        * durationScale
                        * scaleFactor
                        * epIntensity params

           -- Clamp rate to prevent over-smoothing past the average
           clampedRate = min 1.0 combinedRate

           -- Raw delta: fraction of the difference we close
           rawDelta = diff * clampedRate

           -- Round toward zero to avoid jitter on small differences
           delta = if abs rawDelta < 0.5 then 0
                   else truncateTowardZero rawDelta
           -- Soil depth for last-age: continuous function of slope
           -- instead of discrete thresholds (avoids visible contour lines).
           -- Steep slopes (>0.8) get no soil; flat terrain gets full depth.
           soilDepth
               | not (epIsLastAge params) = 0
               | slopeNorm > 0.8          = 0
               | otherwise                = max 1 (round
                   (4.0 * erodability * (1.0 - slopeNorm) ∷ Float))

           -- Strata thickness bonus: longer ages deposit thicker layers.
           -- A 15-MY age adds 5 bonus tiles, a 1-MY age adds 0.
           -- This only applies to non-last-age periods (geological rock strata).
           durationBonus
               | epIsLastAge params = 0
               | otherwise          = max 0 (truncateTowardZero
                                        (fromIntegral duration / 3.0 ∷ Float))

       in if delta ≡ 0
          then if epIsLastAge params ∧ soilDepth > 0
               then GeoModification
                   { gmElevDelta        = 0
                   , gmMaterialOverride = Just (erosionSediment params matId elev False)
                   , gmIntrusionDepth   = soilDepth
                   }
               else if epIsLastAge params
               then GeoModification
                   { gmElevDelta        = 0
                   , gmMaterialOverride = Just (erosionSediment params matId elev False)
                   , gmIntrusionDepth   = 0
                   }
               else noModification
          else if delta < 0
               then if epIsLastAge params ∧ soilDepth > 0
                    then GeoModification
                        { gmElevDelta        = delta
                        , gmMaterialOverride = Just (erosionSediment params matId elev False)
                        , gmIntrusionDepth   = soilDepth
                        }
                    else GeoModification
                        { gmElevDelta        = delta
                        , gmMaterialOverride = Just (erosionSediment params matId elev False)
                        , gmIntrusionDepth   = durationBonus  -- ← erosion strata thickness
                        }
               -- Deposition: tile is lower than neighbors, receive sediment
               else GeoModification
                   { gmElevDelta        = delta
                   , gmMaterialOverride = Just (erosionSediment params matId elev True)
                   , gmIntrusionDepth   = if epIsLastAge params
                                          then max delta soilDepth
                                          else delta + durationBonus  -- ← deposition strata thickness
                   }

-- | Truncate a float toward zero (not toward negative infinity).
truncateTowardZero ∷ Float → Int
truncateTowardZero x
    | x > 0     = floor x
    | x < 0     = ceiling x
    | otherwise = 0

-- | Determine what material results from erosion/deposition.
--   Climate-aware: uses temperature, precipitation, humidity
--   baked into ErosionParams at world-gen time.
--
--   For non-final ages: produces geological sedimentary rocks.
--   For the final age (epIsLastAge): produces soils (0-5 tile veneer).
--
--   The seed field in ErosionParams provides deterministic variation
--   so adjacent tiles don't all produce identical materials.
erosionSediment ∷ ErosionParams → Word8 → Int → Bool → Word8
erosionSediment params matId elev isDeposition =
    let temp  = epTemperature params
        precip = epPrecipitation params
        humid = epHumidity params
        snow  = epSnowFraction params
        seed  = epSeed params
        lastAge = epIsLastAge params

        -- Hash for local variation (cheap: xor + shift).
        -- Interpolate between adjacent elevation buckets to prevent
        -- both fine-grained checkerboard (from per-tile hashing) and
        -- coarse 8-tile banding (from hard quantization).
        bucket = elev `div` 8
        bucketFrac = fromIntegral (elev `mod` 8) / 8.0 ∷ Float
        hashBucket b = fromIntegral (seed `xor` fromIntegral matId
                                          `xor` (fromIntegral b * 0x9E3779B9)) ∷ Word64
        roll0 = fromIntegral (hashBucket bucket .&. 0xFF) / 255.0 ∷ Float
        roll1 = fromIntegral (hashBucket (bucket + 1) .&. 0xFF) / 255.0 ∷ Float
        roll = roll0 + bucketFrac * (roll1 - roll0)

    in if lastAge
       then soilFromClimate temp precip humid snow matId roll
       else rockFromSource matId temp precip roll

-- | Final-age soil selection based on climate gradients.
--   Materials follow temperature/precipitation smoothly.
--   The roll value shifts each threshold by ±0.02, creating a
--   natural transition zone instead of a hard line at each boundary.
soilFromClimate ∷ Float → Float → Float → Float → Word8 → Float → Word8
soilFromClimate temp precip humid snow _srcMat roll
    -- Glacial: till/moraine/glacial clay
    | snow > 0.6 ∧ temp < -5.0 =
        if snow > blur 0.85 then 112          -- glacial clay
        else if snow > blur 0.7 then 111      -- moraine
        else 110                               -- till

    -- Cold + wet periglacial
    | temp < blur 0.0 ∧ precip > blur 0.3 =
        if temp < blur (-3.0) then 65          -- heavy gravel
        else if precip > blur 0.5 then 61     -- silt
        else 60                                -- silt loam

    -- Cold + dry
    | temp < blur 5.0 ∧ precip < blur 0.2 =
        if precip < blur 0.08 then 67          -- salt flat
        else 66                                -- light gravel

    -- Hot + wet tropical
    | temp > blur 25.0 ∧ precip > blur 0.5 =
        if precip > blur 0.8 then 64           -- muck
        else if precip > blur 0.65 then 50    -- clay
        else if precip > blur 0.55 then 58    -- silty clay
        else 56                                -- loam

    -- Hot + dry desert → transitional
    | temp > blur 25.0 ∧ precip < blur 0.3 =
        if precip < blur 0.08 then 55          -- sand (hyper-arid)
        else if precip < blur 0.15 then 55    -- sand (arid)
        else if precip < blur 0.2  then 54    -- loamy sand
        else if precip < blur 0.25 then 53    -- sandy loam
        else 52                                -- sandy clay loam

    -- Warm + moderate
    | temp > blur 15.0 ∧ precip > blur 0.3 =
        if precip > blur 0.6 then if humid > 0.6 then 62 else 57
        else if precip > blur 0.45 then 57    -- clay loam
        else if precip > blur 0.35 then 56    -- loam
        else 52                                -- sandy clay loam

    -- Temperate + wet
    | temp > blur 5.0 ∧ precip > blur 0.4 =
        if precip > blur 0.7 then 62           -- peat
        else if precip > blur 0.55 then 59    -- silty clay loam
        else if precip > blur 0.45 then 56    -- loam
        else 60                                -- silt loam

    -- Temperate + dry
    | temp > blur 5.0 ∧ precip < blur 0.3 =
        if precip < blur 0.1 then 55           -- sand
        else if precip < blur 0.2 then 54     -- loamy sand
        else 53                                -- sandy loam

    -- Default temperate
    | otherwise =
        if precip > blur 0.5 then 60           -- silt loam
        else if precip > blur 0.3 then 56     -- loam
        else 53                                -- sandy loam
  where
    -- Shift threshold by ±0.02 based on roll to blur material
    -- boundaries.  Creates a natural transition zone instead of
    -- a hard line where the climate gradient crosses each threshold.
    blur threshold = threshold + (roll - 0.5) * 0.04

-- | Non-final-age sedimentary rock from source material.
--   Climate modulates which sedimentary rock forms.
rockFromSource ∷ Word8 → Float → Float → Float → Word8
rockFromSource matId temp precip roll = case matId of
    -- Quartz-rich igneous → sandstone family
    1  → if precip > 0.5 ∧ roll < 0.3 then 25 else 20  -- granite → claystone or sandstone
    2  → 20   -- diorite → sandstone
    8  → 20   -- pegmatite → sandstone
    83 → 20   -- feldspar → sandstone

    -- Mafic igneous → fine-grained sediments
    3  → if precip > 0.4 then 24 else 22  -- gabbro → mudstone or shale
    7  → 24   -- peridotite → mudstone
    10 → if temp > 20.0 ∧ precip > 0.5 then 25 else 22  -- basalt → claystone or shale
    100 → 22  -- lava → shale
    101 → 22  -- magma → shale

    -- Calcium-bearing → limestone/chalk
    6  → 30   -- anorthosite → limestone (calcium feldspar)
    30 → 30   -- limestone → limestone (re-deposited)
    35 → 30   -- dolomite → limestone
    40 → if precip > 0.3 then 31 else 30  -- marble → chalk or limestone

    -- Volcanic → tuff products
    14 → 21   -- tuff → siltstone
    15 → 21   -- pumice → siltstone
    16 → 21   -- scoria → siltstone
    102 → 21  -- volcanic ash → siltstone
    103 → 21  -- tephra → siltstone

    -- Obsidian/rhyolite → fine silica
    11 → if roll < 0.5 then 22 else 32   -- obsidian → shale or chert
    12 → 20   -- rhyolite → sandstone
    13 → if precip > 0.4 then 24 else 20  -- andesite → mudstone or sandstone

    -- Metamorphic weathering products
    41 → 20   -- quartzite → sandstone
    42 → 22   -- slate → shale
    43 → if precip > 0.5 then 24 else 22  -- schist → mudstone or shale
    44 → if roll < 0.4 then 20 else 22    -- gneiss → sandstone or shale
    45 → 22   -- phyllite → shale

    -- Sedimentary → stays or coarsens
    20 → 20   -- sandstone → sandstone
    21 → 21   -- siltstone → siltstone
    22 → 22   -- shale → shale
    23 → if roll < 0.5 then 20 else 23  -- conglomerate → sandstone or conglomerate
    24 → 24   -- mudstone → mudstone
    25 → 25   -- claystone → claystone

    -- Chemical/organic sedimentary
    31 → 31   -- chalk → chalk
    32 → 32   -- chert → chert
    33 → if precip < 0.2 then 33 else 34  -- rock salt → rock salt or gypsum
    34 → 34   -- gypsum → gypsum

    -- Impact materials → mixed debris
    90 → if roll < 0.5 then 20 else 23  -- impactite → sandstone or conglomerate
    91 → 20   -- tektite → sandstone

    -- Ores stay (they're deep/hard, erosion rarely reaches them)
    m | m ≥ 80 ∧ m ≤ 86 → 20  -- ore → sandstone (surface weathering)

    -- Glacial deposits
    110 → 23  -- till → conglomerate
    111 → 23  -- moraine → conglomerate
    112 → 25  -- glacial clay → claystone
    113 → 23  -- outwash gravel → conglomerate

    -- Soils from previous ages get lithified
    50 → 25   -- clay → claystone
    51 → 25   -- sandy clay → claystone
    52 → 24   -- sandy clay loam → mudstone
    53 → 20   -- sandy loam → sandstone
    54 → 20   -- loamy sand → sandstone
    55 → 20   -- sand → sandstone
    56 → 24   -- loam → mudstone
    57 → 25   -- clay loam → claystone
    58 → 25   -- silty clay → claystone
    59 → 24   -- silty clay loam → mudstone
    60 → 21   -- silt loam → siltstone
    61 → 21   -- silt → siltstone
    62 → 70   -- peat → lignite
    63 → 71   -- mucky peat → bituminous coal
    64 → 72   -- muck → anthracite
    65 → 23   -- heavy gravel → conglomerate
    66 → 23   -- light gravel → conglomerate
    67 → 33   -- salt flat → rock salt

    -- Default
    _  → 20   -- sandstone (general weathering product)

-- | Look up erosion params for a tile, using regional climate
--   data when available, falling back to the global params.
--
--   Uses bilinear interpolation between the 4 nearest region
--   centers to eliminate hard grid boundaries.
--
--   Climate regions are keyed by (ru, rv) in the rotated
--   (u, v) = (gx - gy, gx + gy) coordinate system, divided
--   into climateRegionSize chunks per side.
{-# INLINE lookupRegionalErosion #-}
lookupRegionalErosion ∷ ErosionParams
                      → HM.HashMap ClimateCoord ErosionParams
                      → Int → Int → Int
                      → ErosionParams
lookupRegionalErosion fallback regMap worldSize gx gy =
    if HM.null regMap
    then fallback
    else let RegionGridCoords ru0 ru1 rv0 rv1 tu tv =
                 regionGridCoords 16 worldSize gx gy

             lookupEP ru rv =
                 HM.lookupDefault fallback (ClimateCoord ru rv) regMap

             ep00 = lookupEP ru0 rv0
             ep10 = lookupEP ru1 rv0
             ep01 = lookupEP ru0 rv1
             ep11 = lookupEP ru1 rv1

             lerpF a b t = a + t * (b - a)
             lerpField f = lerpF (lerpF (f ep00) (f ep10) tu)
                                 (lerpF (f ep01) (f ep11) tu) tv

         in ErosionParams
            { epIntensity     = lerpField epIntensity
            , epHydraulic     = lerpField epHydraulic
            , epThermal       = lerpField epThermal
            , epWind          = lerpField epWind
            , epChemical      = lerpField epChemical
            , epSeed          = epSeed ep00
            , epTemperature   = lerpField epTemperature
            , epPrecipitation = lerpField epPrecipitation
            , epHumidity      = lerpField epHumidity
            , epSnowFraction  = lerpField epSnowFraction
            , epIsLastAge     = epIsLastAge ep00
            }
