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
import World.Weather.Types (ClimateCoord(..), climateRegionSize)

-----------------------------------------------------------
-- Erosion Application
-----------------------------------------------------------

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
             → Int       -- ^ this tile's post-event elevation
             → (Int, Int, Int, Int)
                         -- ^ neighbor post-event elevations (N, S, E, W)
             → GeoModification
applyErosion params _worldSize duration worldScale matId elev (nN, nS, nE, nW) =
    let hardness = matHardness (getMaterialProps (MaterialId matId))
    in if hardness ≥ 1.0
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

           -- Clamp rate to prevent over-smoothing past the average
           clampedRate = min 1.0 combinedRate

           -- Raw delta: fraction of the difference we close
           rawDelta = diff * clampedRate

           -- Round toward zero to avoid jitter on small differences
           delta = if abs rawDelta < 0.5
                   then 0
                   else truncateTowardZero rawDelta
           soilDepth
               | not (epIsLastAge params) = 0  -- no soil veneer in non-final ages
               | slopeNorm > 0.8          = 0
               | slopeNorm > 0.5          = 1
               | slopeNorm > 0.2          = 2
               | otherwise                = max 1 (round (4.0 * erodability))

       in if delta ≡ 0
          then
              if epIsLastAge params ∧ soilDepth > 0
              then GeoModification
                       { gmElevDelta        = 0
                       , gmMaterialOverride = Just (erosionSediment params matId elev False)
                       , gmIntrusionDepth   = soilDepth
                       }
              else noModification
          else if delta < 0
               -- Erosion: tile is higher than neighbors, remove material
               -- Surface becomes sedimentary rock (weathering product)
               then if epIsLastAge params ∧ soilDepth > 0
                    then GeoModification
                             { gmElevDelta        = delta
                             , gmMaterialOverride = Just (erosionSediment params matId elev False)
                             , gmIntrusionDepth   = soilDepth
                             }
                    else GeoModification
                             { gmElevDelta        = delta
                             , gmMaterialOverride = Just (erosionSediment params matId elev False)
                             , gmIntrusionDepth   = 0
                             }
               -- Deposition: tile is lower than neighbors, receive sediment
               -- Deposited material is sedimentary, with full intrusion
               -- so it shows in stratigraphy as a distinct layer
               else GeoModification
                   { gmElevDelta        = delta
                   , gmMaterialOverride = Just (erosionSediment params matId elev True)
                   , gmIntrusionDepth   = if epIsLastAge params
                                          then max delta soilDepth
                                          else delta
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

        -- Hash for local variation (cheap: xor + shift)
        localHash = fromIntegral (seed `xor` fromIntegral matId
                                       `xor` (fromIntegral elev * 0x9E3779B9)) ∷ Word64
        roll = fromIntegral (localHash .&. 0xFF) / 255.0 ∷ Float

    in if lastAge
       then soilFromClimate temp precip humid snow matId roll
       else rockFromSource matId temp precip roll

-- | Final-age soil selection based on climate.
--   Produces soil materials (50-67) as a thin surface veneer.
soilFromClimate ∷ Float → Float → Float → Float → Word8 → Float → Word8
soilFromClimate temp precip humid snow srcMat roll
    -- Glacial: till/moraine/glacial clay
    | snow > 0.6 ∧ temp < -5.0 =
        if roll < 0.5 then 110       -- till
        else if roll < 0.8 then 111  -- moraine
        else 112                      -- glacial clay

    -- Cold + wet periglacial: silt + gravel
    | temp < 0.0 ∧ precip > 0.3 =
        if roll < 0.4 then 61        -- silt
        else if roll < 0.7 then 65   -- heavy gravel
        else 60                       -- silt loam

    -- Cold + dry: salt flats, light gravel
    | temp < 5.0 ∧ precip < 0.2 =
        if roll < 0.5 then 67        -- salt flat
        else 66                       -- light gravel

    -- Hot + wet tropical: deep clay weathering
    | temp > 25.0 ∧ precip > 0.5 =
        if roll < 0.3 then 50        -- clay
        else if roll < 0.5 then 58   -- silty clay
        else if roll < 0.7 then 56   -- loam
        else 64                       -- muck (organic-rich)

    -- Hot + dry desert: sand
    | temp > 25.0 ∧ precip < 0.2 =
        if roll < 0.6 then 55        -- sand
        else if roll < 0.8 then 54   -- loamy sand
        else 67                       -- salt flat

    -- Warm + moderate: clay-loam spectrum
    | temp > 15.0 ∧ precip > 0.3 =
        if roll < 0.25 then 57       -- clay loam
        else if roll < 0.5 then 52   -- sandy clay loam
        else if roll < 0.75 then 56  -- loam
        else 62                       -- peat (if very wet + organic)

    -- Temperate + wet: rich soils
    | temp > 5.0 ∧ precip > 0.4 =
        if roll < 0.3 then 60        -- silt loam
        else if roll < 0.6 then 56   -- loam
        else if roll < 0.8 then 59   -- silty clay loam
        else 62                       -- peat

    -- Temperate + dry: sandy soils
    | temp > 5.0 ∧ precip < 0.3 =
        if roll < 0.4 then 53        -- sandy loam
        else if roll < 0.7 then 54   -- loamy sand
        else 55                       -- sand

    -- Default temperate
    | otherwise =
        if roll < 0.3 then 56        -- loam
        else if roll < 0.6 then 60   -- silt loam
        else 53                       -- sandy loam

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
    else let -- Convert (gx, gy) tile coords → (u, v) tile coords
             u = gx - gy
             v = gx + gy
             -- Convert tile coords → chunk coords
             halfChunks = worldSize `div` 2
             w = worldSize * chunkSizeTiles
             halfW = w `div` 2
             -- Wrap u-axis (cylindrical world), then to chunk space
             wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
             chunkU = floorDiv wrappedU chunkSizeTiles
             chunkV = floorDiv v chunkSizeTiles
             -- Chunk space → climate region index
             ru = (chunkU + halfChunks) `div` climateRegionSize
             rv = (chunkV + halfChunks) `div` climateRegionSize
         in HM.lookupDefault fallback (ClimateCoord ru rv) regMap
  where
    chunkSizeTiles = 16
    floorDiv a b
      | b > 0     = if a >= 0 then a `div` b else -(((-a) + b - 1) `div` b)
      | otherwise = error "floorDiv: non-positive divisor"
