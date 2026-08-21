{-# LANGUAGE Strict #-}
-- | Data the zoom CACHE produces: the per-chunk summary entries
--   "World.ZoomMap.Cache" builds at world-init time, and the atlas tile
--   size the cache's pixel generators lay out against.
--
--   This module owns those types so the cache tree imports its own
--   output honestly. It deliberately has no dependency on
--   @World.Render.*@: the render tree ("World.Render.Zoom.Types" and
--   siblings) reads this data, never the other way round.
module World.ZoomMap.Types
    ( ZoomChunkEntry(..)
    , zoomTileSize
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))

data ZoomChunkEntry = ZoomChunkEntry
    { zceChunkX   ∷ !Int       -- ^ Canonical chunk X
    , zceChunkY   ∷ !Int       -- ^ Canonical chunk Y
    , zceBaseGX   ∷ !Int
    , zceBaseGY   ∷ !Int
    , zceTexIndex ∷ !Word8     -- ^ Material ID (used to pick texture at render time)
    , zceElev     ∷ !Int       -- ^ Elevation (used to pick texture at render time)
    , zceIsOcean  ∷ !Bool      -- ^ Whether this chunk is ocean
    , zceHasLava  ∷ !Bool      -- ^ Whether this chunk has lava (for zoom rendering)
    , zceVegCategory ∷ !Word8  -- ^ Vegetation density category (0=none,1=sparse,2=medium,3=dense,4=marsh)
    , zceHasIce  ∷ !Bool      -- ^ Whether this chunk has ice cover
    } deriving (Show, Eq)
instance NFData ZoomChunkEntry where
    rnf (ZoomChunkEntry x y bgX bgY tex elev ocean lava veg ice) =
        rnf x `seq` rnf y `seq` rnf bgX `seq` rnf bgY `seq`
        rnf tex `seq` rnf elev `seq` rnf ocean `seq` rnf lava `seq`
        rnf veg `seq` rnf ice

-- | Pixel size of each chunk tile in the zoom atlas.
--   Larger than chunkSize (16) to accommodate the isometric
--   diamond shape within a square texture tile.
zoomTileSize ∷ Int
zoomTileSize = 32
