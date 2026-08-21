{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | Types the zoom RENDERER owns: baked render-ready entries, the quad
--   cache and its camera snapshot, the atlas description, and the
--   save-persisted map mode.
--
--   "World.ZoomMap.Cache" and its siblings BUILD the zoom cache at
--   world-init time; this tree ("World.Render.Zoom.*") RENDERS from it.
--   The cache's own output types live in "World.ZoomMap.Types" and are
--   re-exported here unchanged for existing consumers.
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
import qualified Data.Vector as V
import Engine.Scene.Types.Batch (SortableQuad(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..))
import Engine.Asset.Handle (TextureHandle(..))
import World.ZoomMap.Types (ZoomChunkEntry(..), zoomTileSize)

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
