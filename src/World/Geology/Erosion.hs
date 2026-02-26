{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Erosion
    ( applyErosion
    , erosionSediment
    ) where

import UPrelude
import World.Types
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..))
import World.Geology.Types

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

       in if delta ≡ 0
          then noModification
          else if delta < 0
               -- Erosion: tile is higher than neighbors, remove material
               -- Surface becomes sedimentary rock (weathering product)
               then GeoModification
                   { gmElevDelta        = delta
                   , gmMaterialOverride = Just (erosionSediment matId)
                   , gmIntrusionDepth   = 0
                   }
               -- Deposition: tile is lower than neighbors, receive sediment
               -- Deposited material is sedimentary, with full intrusion
               -- so it shows in stratigraphy as a distinct layer
               else GeoModification
                   { gmElevDelta        = delta
                   , gmMaterialOverride = Just (erosionSediment matId)
                   , gmIntrusionDepth   = delta
                   }

-- | Determine what sedimentary material results from eroding
--   a given source material. Different rocks weather into
--   different sedimentary products.
erosionSediment ∷ Word8 → Word8
erosionSediment matId = case matId of
    -- Quartz-rich rocks → sandstone
    1   → 10   -- granite → sandstone
    2   → 10   -- diorite → sandstone
    33  → 10   -- feldspar → sandstone

    -- Calcium/alkaline rocks → limestone
    11  → 11   -- limestone → limestone (re-deposited)
    31  → 11   -- olivine → limestone (calcium weathering)

    -- Fine-grained / dark rocks → shale
    3   → 12   -- gabbro → shale
    4   → 12   -- basalt → shale
    5   → 12   -- obsidian → shale
    12  → 12   -- shale → shale (re-deposited)
    32  → 12   -- pyroxene → shale
    100 → 12   -- lava → shale

    -- Impact materials → sandstone (mixed debris)
    20  → 10   -- impactite → sandstone
    30  → 10   -- iron meteorite → sandstone

    -- Sedimentary rocks stay as themselves
    10  → 10   -- sandstone → sandstone

    -- Default: sandstone (general weathering product)
    _   → 10

-- | Truncate a float toward zero (not toward negative infinity).
truncateTowardZero ∷ Float → Int
truncateTowardZero x
    | x > 0     = floor x
    | x < 0     = ceiling x
    | otherwise = 0
