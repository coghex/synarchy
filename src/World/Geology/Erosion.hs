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
--   Scaled by:
--     - epIntensity: overall erosion strength for this period
--     - (1 - hardness): material resistance
--     - durationScale: longer ages erode more (duration / 10 MY reference)
--     - worldScale: larger worlds have proportionally larger features
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
           --   durationScale: duration / 10.0 (10 MY reference age)
           --   worldScale:    worldSize / 512 (larger worlds = larger features)
           --   smoothing:     0.5 base factor to prevent over-correction
           erodability = 1.0 - hardness
           durationScale = fromIntegral duration / 10.0 ∷ Float
           rate = epIntensity params
                * erodability
                * durationScale
                * worldScale
                * 0.5  -- base smoothing factor

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
    10  → 10   -- sandstone → sandstone

    -- Default: sandstone (general weathering product)
    _   → 10

-- | Truncate a float toward zero (not toward negative infinity).
truncateTowardZero ∷ Float → Int
truncateTowardZero x
    | x > 0     = floor x
    | x < 0     = ceiling x
    | otherwise = 0
