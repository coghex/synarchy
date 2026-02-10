{-# LANGUAGE Strict #-}
module World.Grid
    ( -- * Grid configuration
      GridConfig(..)
    , defaultGridConfig
      -- * Derived constants (from defaultGridConfig)
    , tileWidth
    , tileHeight
    , tileDiamondHeight
    , tileSideHeight
    , tileHalfWidth
    , tileHalfDiamondHeight
      -- * Layer constants
    , worldLayer
    , uiLayerThreshold
      -- * Coordinate conversions
    , gridToWorld
    , gridToScreen
    , worldToGrid
      -- * Camera constants
    , cameraPanSpeed
    ) where

import UPrelude
import Engine.Scene.Base (LayerId(..))

-----------------------------------------------------------
-- Grid Configuration
-----------------------------------------------------------

-- | All world grid constants in one place.
-- Changing the sprite size or proportions only requires
-- editing defaultGridConfig — everything else is derived.
data GridConfig = GridConfig
    { gcTilePixelWidth  :: !Int    -- ^ Sprite width in pixels (96)
    , gcTilePixelHeight :: !Int    -- ^ Sprite height in pixels (64)
    , gcSidePixels      :: !Int    -- ^ Side-face height in pixels (16)
    , gcWorldTileWidth  :: !Float  -- ^ Sprite width in world-space units
    , gcWorldLayer      :: !LayerId -- ^ Layer for world tiles/sprites
    , gcUILayerThreshold :: !LayerId -- ^ Layers >= this use UI pipeline
    , gcCameraPanSpeed   :: !Float  -- ^ Camera pan speed in world units per second
    } deriving (Show)

defaultGridConfig :: GridConfig
defaultGridConfig = GridConfig
    { gcTilePixelWidth  = 96
    , gcTilePixelHeight = 64
    , gcSidePixels      = 16
    , gcWorldTileWidth  = 0.15
    , gcWorldLayer      = LayerId 1
    , gcUILayerThreshold = LayerId 10
    , gcCameraPanSpeed   = 0.5
    }

-----------------------------------------------------------
-- Derived Constants
-----------------------------------------------------------

-- | All derived values computed from defaultGridConfig.
-- These are top-level CAFs — computed once, shared everywhere.

tileWidth :: Float
tileWidth = gcWorldTileWidth defaultGridConfig

-- | Full sprite height, maintaining pixel aspect ratio
tileHeight :: Float
tileHeight = tileWidth * fromIntegral ph / fromIntegral pw
  where
    pw = gcTilePixelWidth defaultGridConfig
    ph = gcTilePixelHeight defaultGridConfig

-- | Diamond (top face) height: the isometric footprint
tileDiamondHeight :: Float
tileDiamondHeight = tileWidth * fromIntegral diamondPx / fromIntegral pw
  where
    pw = gcTilePixelWidth defaultGridConfig
    diamondPx = gcTilePixelHeight defaultGridConfig - gcSidePixels defaultGridConfig

-- | Side face height: one elevation level
tileSideHeight :: Float
tileSideHeight = tileWidth * fromIntegral (gcSidePixels defaultGridConfig) 
              / fromIntegral (gcTilePixelWidth defaultGridConfig)

tileHalfWidth :: Float
tileHalfWidth = tileWidth / 2.0

tileHalfDiamondHeight :: Float
tileHalfDiamondHeight = tileDiamondHeight / 2.0

-- | The layer used for world tiles and world-space scene sprites
worldLayer :: LayerId
worldLayer = gcWorldLayer defaultGridConfig

-- | Layers >= this threshold use the UI pipeline
uiLayerThreshold :: LayerId
uiLayerThreshold = gcUILayerThreshold defaultGridConfig

-- | Camera pan speed in world-space units per second
cameraPanSpeed :: Float
cameraPanSpeed = gcCameraPanSpeed defaultGridConfig

-----------------------------------------------------------
-- Coordinate Conversions
-----------------------------------------------------------

-- | Convert grid coordinates to world-space position.
-- Returns the TOP-CENTER of the diamond for tile (gx, gy).
gridToWorld :: Int -> Int -> (Float, Float)
gridToWorld gx gy =
    let sx = fromIntegral (gx - gy) * tileHalfWidth
        sy = fromIntegral (gx + gy) * tileHalfDiamondHeight
    in (sx, sy)

-- | Convert grid coordinates to sprite draw origin (top-left of quad).
gridToScreen :: Int -> Int -> (Float, Float)
gridToScreen gx gy =
    let (cx, cy) = gridToWorld gx gy
    in (cx - tileHalfWidth, cy)

-- | Convert world-space position back to nearest grid coordinates.
worldToGrid :: Float -> Float -> (Int, Int)
worldToGrid wx wy =
    let fgx = (wx / tileHalfWidth + wy / tileHalfDiamondHeight) / 2.0
        fgy = (wy / tileHalfDiamondHeight - wx / tileHalfWidth) / 2.0
    in (round fgx, round fgy)
