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
--   Harder materials (granite 0.9, obsidian 0.95) barely erode.
--   Softer materials (shale 0.25, sandstone 0.4) smooth quickly.
--   Indestructible materials (glacier 1.0) don't erode at all.
--
--   The erosion delta is always toward the neighbor average:
--     - Tile higher than neighbors → negative delta (erosion)
--     - Tile lower than neighbors → positive delta (deposition)
--
--   Erosion deposits sedimentary material. Deposition also
--   deposits sedimentary material with full intrusion depth
--   so it appears in stratigraphy.
--
--   The rate formula uses sqrt(worldScale) instead of raw worldScale
--   so that small worlds (128 chunks, scale=0.25) still get meaningful
--   erosion (sqrt 0.25 = 0.5) while large worlds don't over-erode.
--   The base smoothing factor is raised to 1.5 to compensate for
--   the per-period application (each age is one pass, not many).
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

           -- Erosion rate: how much of the difference we close per age
           --   intensity:     0.0–1.0 from ErosionParams
           --   erodability:   (1 - hardness), e.g. shale=0.75, granite=0.1
           --   durationScale: duration / 5.0 (5 MY reference age, was 10)
           --   scaleFactor:   sqrt(worldScale) — flattens the curve so
           --                  small worlds still erode
           --   smoothing:     1.5 base factor (was 0.5)
           --
           -- Old formula at ws=128, granite, 5MY age:
           --   0.5 * 0.1 * 0.5 * 0.25 * 0.5 = 0.003 (rounds to 0)
           -- New formula at ws=128, granite, 5MY age:
           --   0.5 * 0.1 * 1.0 * 0.5 * 1.5 = 0.0375 (10-tile diff → delta 0.375, rounds to 0 still)
           -- But for shale (erodability 0.75) at 5MY:
           --   0.5 * 0.75 * 1.0 * 0.5 * 1.5 = 0.28 (10-tile diff → delta 2.8, rounds to 2!)
           -- And for sandstone (erodability 0.6):
           --   0.5 * 0.6 * 1.0 * 0.5 * 1.5 = 0.225 (10-tile diff → delta 2.25, rounds to 2)
           erodability = 1.0 - hardness
           durationScale = fromIntegral duration / 5.0 ∷ Float
           scaleFactor = sqrt (max 0.1 worldScale)
           rate = epIntensity params
                * erodability
                * durationScale
                * scaleFactor
                * 1.5  -- base smoothing factor

           -- Clamp rate to prevent over-smoothing past the average
           clampedRate = min 1.0 rate

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
    10  → 10   -- sandstone  sandstone

    -- Default: sandstone (general weathering product)
    _   → 10

-- | Truncate a float toward zero (not toward negative infinity).
truncateTowardZero ∷ Float → Int
truncateTowardZero x
    | x > 0     = floor x
    | x < 0     = ceiling x
    | otherwise = 0
