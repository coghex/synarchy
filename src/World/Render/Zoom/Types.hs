{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Render.Zoom.Types
    ( ZoomChunkEntry(..)
    , ZoomCameraSnapshot(..)
    , ZoomQuadCache(..)
    , BakedZoomEntry(..)
    , ZoomMapMode(..)
    , ZoomAtlasInfo(..)
    , zoomTileSize
    , textToMapMode
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData(..))
import qualified Data.Vector as V
import Engine.Scene.Types.Batch (SortableQuad(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..))
import Engine.Asset.Handle (TextureHandle(..))

data ZoomMapMode = ZMDefault | ZMTemp | ZMSeaTemp | ZMPressure | ZMHumidity
                 | ZMPrecipitation | ZMPrecipType | ZMEvaporation
    deriving (Show, Eq, Generic, Serialize)

textToMapMode ∷ Text → ZoomMapMode
textToMapMode "map_temp" = ZMTemp
textToMapMode "map_pressure" = ZMPressure
textToMapMode "map_humidity" = ZMHumidity
textToMapMode "map_precipitation" = ZMPrecipitation
textToMapMode "map_preciptype" = ZMPrecipType
textToMapMode "map_evaporation" = ZMEvaporation
textToMapMode "map_seatemp" = ZMSeaTemp
textToMapMode _          = ZMDefault

data ZoomChunkEntry = ZoomChunkEntry
    { zceChunkX   ∷ !Int       -- ^ Canonical chunk X
    , zceChunkY   ∷ !Int       -- ^ Canonical chunk Y
    , zceBaseGX   ∷ !Int
    , zceBaseGY   ∷ !Int
    , zceTexIndex ∷ !Word8     -- ^ Material ID (used to pick texture at render time)
    , zceElev     ∷ !Int       -- ^ Elevation (used to pick texture at render time)
    , zceIsOcean  ∷ !Bool      -- ^ Whether this chunk is ocean
    , zceHasLava  ∷ !Bool      -- ^ Whether this chunk has lava (for zoom rendering)
    , zceHasRiver ∷ !Bool      -- ^ Whether this chunk has a river (for preview)
    , zceHasLake  ∷ !Bool      -- ^ Whether this chunk has a lake (for preview)
    , zceVegCategory ∷ !Word8  -- ^ Vegetation density category (0=none,1=sparse,2=medium,3=dense,4=marsh)
    } deriving (Show, Eq)
instance NFData ZoomChunkEntry where
    rnf (ZoomChunkEntry x y bgX bgY tex elev ocean lava river lake veg) =
        rnf x `seq` rnf y `seq` rnf bgX `seq` rnf bgY `seq`
        rnf tex `seq` rnf elev `seq` rnf ocean `seq` rnf lava `seq`
        rnf river `seq` rnf lake `seq` rnf veg

data ZoomCameraSnapshot = ZoomCameraSnapshot
    { zcsPosition ∷ !(Float, Float)
    , zcsZoom     ∷ !Float
    , zcsFbSize   ∷ !(Int, Int)
    } deriving (Show, Eq)

data ZoomQuadCache = ZoomQuadCache
    { zqcCamera ∷ !ZoomCameraSnapshot
    , zqcAlpha  ∷ !Float               -- ^ Alpha at time of caching
    , zqcQuads  ∷ !(V.Vector SortableQuad)
    } deriving (Show)

data BakedZoomEntry = BakedZoomEntry
    { bzeChunkX  ∷ !Int
    , bzeChunkY  ∷ !Int
    , bzeDrawX   ∷ !Float       -- ^ Canonical draw X (before wrap offset)
    , bzeDrawY   ∷ !Float
    , bzeWidth   ∷ !Float       -- ^ Quad width (for wrap-around rendering)
    , bzeHeight  ∷ !Float       -- ^ Quad height
    , bzeSortKey ∷ !Float
    , bzeV0      ∷ !Vertex      -- ^ Top-left
    , bzeV1      ∷ !Vertex      -- ^ Top-right
    , bzeV2      ∷ !Vertex      -- ^ Bottom-right
    , bzeV3      ∷ !Vertex      -- ^ Bottom-left
    , bzeTexture ∷ !TextureHandle
    , bzeIsOcean ∷ !Bool
    , bzeHasLava ∷ !Bool
    , bzeElev    ∷ !Int
    } deriving (Show)

-- | Information about the zoom atlas texture, used for
--   computing per-chunk UV coordinates during baking.
data ZoomAtlasInfo = ZoomAtlasInfo
    { zaiTexture     ∷ !TextureHandle   -- ^ Atlas texture handle
    , zaiWidth       ∷ !Int             -- ^ Atlas width in pixels
    , zaiHeight      ∷ !Int             -- ^ Atlas height in pixels
    , zaiChunksPerRow ∷ !Int            -- ^ Number of chunk tiles per atlas row
    } deriving (Show, Eq)

-- | Pixel size of each chunk tile in the zoom atlas.
--   Larger than chunkSize (16) to accommodate the isometric
--   diamond shape within a square texture tile.
zoomTileSize ∷ Int
zoomTileSize = 32
