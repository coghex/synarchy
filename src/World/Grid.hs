{-# LANGUAGE Strict, UnicodeSyntax #-}
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
    , backgroundMapLayer
      -- * Coordinate conversions
    , gridToWorld
    , gridToScreen
    , worldToGrid
    , worldScreenWidth
      -- * Camera constants
    , cameraPanSpeed
    , cameraPanAccel
    , cameraPanFriction
    , zoomMapLayer
    , chunkWorldWidth
    , chunkWorldDiamondHeight
    , zoomFadeStart
    , zoomFadeEnd
    ) where

import UPrelude
import World.Types (chunkSize)
import Engine.Scene.Base (LayerId(..))

-----------------------------------------------------------
-- Grid Configuration
-----------------------------------------------------------

-- | All world grid constants in one place.
-- Changing the sprite size or proportions only requires
-- editing defaultGridConfig — everything else is derived.
data GridConfig = GridConfig
    { gcTilePixelWidth  ∷ !Int    -- ^ Sprite width in pixels (96)
    , gcTilePixelHeight ∷ !Int    -- ^ Sprite height in pixels (64)
    , gcSidePixels      ∷ !Int    -- ^ Side-face height in pixels (16)
    , gcWorldTileWidth  ∷ !Float  -- ^ Sprite width in world-space units
    , gcWorldLayer      ∷ !LayerId -- ^ Layer for world tiles/sprites
    , gcUILayerThreshold ∷ !LayerId -- ^ Layers >= this use UI pipeline
    , gcCameraPanSpeed   ∷ !Float  -- ^ Camera pan speed in world units per second
    , gcCameraPanAccel   ∷ !Float  -- ^ Camera pan acceleration in world units per second²
    , gcCameraPanFriction ∷ !Float  -- ^ Camera pan friction (deceleration) in world units per second²
    } deriving (Show)

defaultGridConfig ∷ GridConfig
defaultGridConfig = GridConfig
    { gcTilePixelWidth  = 96
    , gcTilePixelHeight = 64
    , gcSidePixels      = 16
    , gcWorldTileWidth  = 0.15
    , gcWorldLayer      = LayerId 1
    , gcUILayerThreshold = LayerId 10
    , gcCameraPanSpeed   = 1.0
    , gcCameraPanAccel   = 2.0
    , gcCameraPanFriction = 4.0
    }

-----------------------------------------------------------
-- Derived Constants
-----------------------------------------------------------

-- | All derived values computed from defaultGridConfig.
-- These are top-level CAFs — computed once, shared everywhere.

tileWidth ∷ Float
tileWidth = gcWorldTileWidth defaultGridConfig

-- | Full sprite height, maintaining pixel aspect ratio
tileHeight ∷ Float
tileHeight = tileWidth * fromIntegral ph / fromIntegral pw
  where
    pw = gcTilePixelWidth defaultGridConfig
    ph = gcTilePixelHeight defaultGridConfig

-- | Diamond (top face) height: the isometric footprint
tileDiamondHeight ∷ Float
tileDiamondHeight = tileWidth * fromIntegral diamondPx / fromIntegral pw
  where
    pw = gcTilePixelWidth defaultGridConfig
    diamondPx = gcTilePixelHeight defaultGridConfig - gcSidePixels defaultGridConfig

-- | Side face height: one elevation level
tileSideHeight ∷ Float
tileSideHeight = tileWidth * fromIntegral (gcSidePixels defaultGridConfig) 
              / fromIntegral (gcTilePixelWidth defaultGridConfig)

tileHalfWidth ∷ Float
tileHalfWidth = tileWidth / 2.0

tileHalfDiamondHeight ∷ Float
tileHalfDiamondHeight = tileDiamondHeight / 2.0

-- | The layer used for world tiles and world-space scene sprites
worldLayer ∷ LayerId
worldLayer = gcWorldLayer defaultGridConfig

-- | Layers >= this threshold use the UI pipeline
uiLayerThreshold ∷ LayerId
uiLayerThreshold = gcUILayerThreshold defaultGridConfig

-- | Camera pan speed in world-space units per second
cameraPanSpeed ∷ Float
cameraPanSpeed = gcCameraPanSpeed defaultGridConfig

-- | Camera acceleration in world-space units per second²
cameraPanAccel ∷ Float
cameraPanAccel = gcCameraPanAccel defaultGridConfig

-- | Camera friction (deceleration) in world-space units per second²
cameraPanFriction ∷ Float
cameraPanFriction = gcCameraPanFriction defaultGridConfig

-- | Layer for background map chunks (renders below world tiles)
backgroundMapLayer ∷ LayerId
backgroundMapLayer = LayerId 0

-- | Layer for zoom map chunks (renders above world tiles)
zoomMapLayer ∷ LayerId
zoomMapLayer = LayerId 2

-- | Screen-space width of a full chunk diamond
chunkWorldWidth ∷ Float
chunkWorldWidth = fromIntegral chunkSize * tileWidth

-- | Screen-space diamond height of a full chunk
chunkWorldDiamondHeight ∷ Float
chunkWorldDiamondHeight = fromIntegral chunkSize * tileDiamondHeight

-- | Zoom level where the map starts fading in (alpha = 0 here)
zoomFadeStart ∷ Float
zoomFadeStart = 2.0

-- | Zoom level where the map is fully opaque (tiles fully hidden)
zoomFadeEnd ∷ Float
zoomFadeEnd = 2.5

-----------------------------------------------------------
-- World Screen Width (wrapping period in screen-space X)
-----------------------------------------------------------

worldScreenWidth ∷ Int → Float
worldScreenWidth worldSizeChunks =
    let worldTiles = worldSizeChunks * chunkSize
    in fromIntegral worldTiles * tileHalfWidth

-----------------------------------------------------------
-- Coordinate Conversions
-----------------------------------------------------------

-- | Convert grid coordinates to world-space position.
-- Returns the TOP-CENTER of the diamond for tile (gx, gy).
gridToWorld ∷ Int → Int → (Float, Float)
gridToWorld gx gy =
    let sx = fromIntegral (gx - gy) * tileHalfWidth
        sy = fromIntegral (gx + gy) * tileHalfDiamondHeight
    in (sx, sy)

-- | Convert grid coordinates to sprite draw origin (top-left of quad).
gridToScreen ∷ Int → Int → (Float, Float)
gridToScreen gx gy =
    let (cx, cy) = gridToWorld gx gy
    in (cx - tileHalfWidth, cy)

-- | Convert world-space position back to nearest grid coordinates.
worldToGrid ∷ Float → Float → (Int, Int)
worldToGrid wx wy =
    let fgx = (wx / tileHalfWidth + wy / tileHalfDiamondHeight) / 2.0
        fgy = (wy / tileHalfDiamondHeight - wx / tileHalfWidth) / 2.0
    in (round fgx, round fgy)
