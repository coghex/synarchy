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
    , zoomTexelTile
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import World.Chunk.Types (chunkSize)

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

-- | Which LOCAL tile of a chunk one texel of its atlas block belongs to.
--
--   The inverse isometric transform the zoom pass draws through: a
--   @zoomTileSize@-square block holds a @chunkSize@ diamond, so a texel
--   maps back to a grid-local coordinate that may fall OUTSIDE the
--   chunk — those are the transparent corners of the square, and the
--   caller is expected to reject them.
--
--   Shared rather than inlined because two places need the same answer:
--   'World.ZoomMap.Cache.Pixels.generateChunkPixels' colours each texel
--   by it, and 'World.Render.Zoom.Project.zoomTileScreenRect' inverts it
--   to say where one tile's texels are drawn. Two copies of an isometric
--   transform is two chances to disagree about where a tile is.
zoomTexelTile ∷ Int → Int → (Int, Int)
zoomTexelTile px py =
    let cs = fromIntegral chunkSize ∷ Float
        ts = fromIntegral zoomTileSize ∷ Float
        u  = (fromIntegral px + 0.5) / ts
        v  = (fromIntegral py + 0.5) / ts
    in (floor (cs * (u + v - 0.5)), floor (cs * (v - u + 0.5)))
