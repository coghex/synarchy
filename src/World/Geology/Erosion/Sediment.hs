{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Erosion.Sediment
    ( erosionSediment
    , erosionSedimentLerp4
    ) where

import UPrelude
import World.Types

-- | Determine what material results from erosion/deposition.
--   Climate-aware: uses temperature, precipitation, humidity baked
--   into ErosionParams at world-gen time.
--
--   For non-final ages: produces geological sedimentary rocks.
--   For the final age (epIsLastAge): produces soils (0-5 tile veneer).
--
--   A Phase 2 attempt added slope + water-table-depth gating here
--   (only place muck on flat tiles near groundwater, etc.) but the
--   underlying wt depth was miscalibrated and the result was muck
--   becoming vanishingly rare. Reverted; soil classification is back
--   to climate-only. The `mpDrainage` field on `MaterialProps` is
--   still present and parsed from YAML for future use.
{-# INLINE erosionSediment #-}
erosionSediment ∷ ErosionParams → Word8 → Int → Bool → Word8
erosionSediment params matId elev isDeposition =
    erosionSedimentScalar
        (epTemperature params) (epPrecipitation params)
        (epHumidity params) (epSnowFraction params)
        (epSeed params) (epIsLastAge params)
        matId elev isDeposition

-- | Fast variant for the chunk-loop in 'World.Generate.Timeline.applyTimelineChunk':
--   takes the 4 climate-region-corner 'ErosionParams' plus @(tu, tv)@.
--   Lerps only the 4 sediment Float fields (temperature, precipitation,
--   humidity, snow) inline.  Seed + isLastAge are picked from @ep00@
--   (they aren't lerped — same convention as 'lerpErosionParams').
--   The callback in 'applyErosionLerp4' invokes this only on tile-periods
--   that actually need a material override, so most tile-periods skip
--   these 4 lerps entirely.
{-# INLINE erosionSedimentLerp4 #-}
erosionSedimentLerp4
    ∷ ErosionParams → ErosionParams → ErosionParams → ErosionParams
    → Float → Float → Word8 → Int → Bool → Word8
erosionSedimentLerp4 ep00 ep10 ep01 ep11 tu tv matId elev isDeposition =
    let lerpC f =
            let v0 = f ep00 + tu * (f ep10 - f ep00)
                v1 = f ep01 + tu * (f ep11 - f ep01)
            in v0 + tv * (v1 - v0)
    in erosionSedimentScalar
        (lerpC epTemperature) (lerpC epPrecipitation)
        (lerpC epHumidity) (lerpC epSnowFraction)
        (epSeed ep00) (epIsLastAge ep00)
        matId elev isDeposition

-- | The actual sediment computation, parameterised on scalar climate
--   values.  'erosionSediment' and 'erosionSedimentLerp4' are thin
--   wrappers.  NOT inlined — one shared copy of the work code.
erosionSedimentScalar
    ∷ Float       -- ^ temperature
    → Float       -- ^ precipitation
    → Float       -- ^ humidity
    → Float       -- ^ snow fraction
    → Word64      -- ^ seed
    → Bool        -- ^ isLastAge
    → Word8       -- ^ source material ID
    → Int         -- ^ tile elevation
    → Bool        -- ^ isDeposition
    → Word8
erosionSedimentScalar temp precip humid snow seed lastAge matId elev _isDeposition =
    let -- Hash for local variation (cheap: xor + shift).
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
        if precip < blur 0.15 then 55          -- sand (arid)
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
