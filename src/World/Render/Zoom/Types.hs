{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Zoom.Types
    ( ZoomChunkEntry(..)
    , ZoomCameraSnapshot(..)
    , ZoomQuadCache(..)
    , BakedZoomEntry(..)
    , ZoomMapMode(..)
    , textToMapMode
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import qualified Data.Vector as V
import Engine.Scene.Types.Batch (SortableQuad(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..))
import Engine.Asset.Handle (TextureHandle(..))

data ZoomMapMode = ZMDefault | ZMTemp
    deriving (Show, Eq)

textToMapMode ∷ Text → ZoomMapMode
textToMapMode "map_temp" = ZMTemp
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
    } deriving (Show, Eq)
instance NFData ZoomChunkEntry where
    rnf (ZoomChunkEntry x y bgX bgY tex elev ocean lava) =
        rnf x `seq` rnf y `seq` rnf bgX `seq` rnf bgY `seq`
        rnf tex `seq` rnf elev `seq` rnf ocean `seq` rnf lava

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
