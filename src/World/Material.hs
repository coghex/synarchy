{-# LANGUAGE Strict #-}
module World.Material
    ( -- * Material IDs
      MaterialId(..)
    , matGranite
    , matDiorite
    , matGabbro
      -- * Material Properties
    , MaterialProps(..)
    , getMaterialProps
    ) where

import UPrelude

-----------------------------------------------------------
-- Material IDs
-----------------------------------------------------------

-- | Material identifier. Stored as the tileType in each Tile.
--   We use Word8 so it fits in the existing Tile structure.
--
--   Convention:
--     0       = air / empty (reserved)
--     1-49    = igneous intrusive
--     50-99   = igneous extrusive (future: basalt, obsidian)
--     100-149 = sedimentary (future: sandstone, limestone)
--     150-199 = metamorphic (future: marble, slate)
--     200-249 = soil/organic (future: dirt, sand, clay)
--     250-255 = special (future: water, lava)
newtype MaterialId = MaterialId { unMaterialId :: Word8 }
    deriving (Show, Eq, Ord)

-- Igneous intrusive
matGranite :: MaterialId
matGranite = MaterialId 1

matDiorite :: MaterialId
matDiorite = MaterialId 2

matGabbro :: MaterialId
matGabbro = MaterialId 3

-----------------------------------------------------------
-- Material Properties
-----------------------------------------------------------

-- | Properties that govern how a material behaves during
--   world generation and erosion.
data MaterialProps = MaterialProps
    { matName       :: !Text          -- ^ Human-readable name
    , matHardness   :: !Float         -- ^ 0.0 = soft (sand), 1.0 = hard (granite)
                                      --   Affects erosion: harder = more cliffs, fewer slopes
    , matDensity    :: !Float         -- ^ Relative density, affects layering
                                      --   Heavier materials sink during formation
    } deriving (Show)

-- | Look up material properties by ID.
--   Unknown materials default to granite-like properties.
getMaterialProps :: MaterialId -> MaterialProps
getMaterialProps (MaterialId mid) = case mid of
    1 -> MaterialProps  -- Granite
            { matName     = "granite"
            , matHardness = 0.9
            , matDensity  = 2.7
            }
    2 -> MaterialProps  -- Diorite
            { matName     = "diorite"
            , matHardness = 0.85
            , matDensity  = 2.8
            }
    3 -> MaterialProps  -- Gabbro
            { matName     = "gabbro"
            , matHardness = 0.8
            , matDensity  = 3.0
            }
    _ -> MaterialProps  -- Fallback
            { matName     = "unknown"
            , matHardness = 0.5
            , matDensity  = 2.5
            }
