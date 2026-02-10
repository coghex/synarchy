{-# LANGUAGE Strict #-}
module World.Grid
    ( -- * Tile dimensions (world-space units)
      tileWidth
    , tileHeight
    , tileDiamondHeight
    , tileSideHeight
      -- * Coordinate conversions
    , gridToWorld
    , gridToScreen
    , worldToGrid
    ) where

import UPrelude

-- | Full sprite dimensions in world-space units
-- Ratio must match sprite pixel ratio: 96:64 = 3:2
tileWidth :: Float
tileWidth = 0.15

tileHeight :: Float
tileHeight = 0.10

-- | The diamond (top face) portion: 48/64 of the full height
tileDiamondHeight :: Float
tileDiamondHeight = tileHeight * (48.0 / 64.0)  -- 0.075

-- | The side face portion: 16/64 of the full height
tileSideHeight :: Float
tileSideHeight = tileHeight * (16.0 / 64.0)  -- 0.025

-- | Half-tile dimensions for grid spacing
halfWidth :: Float
halfWidth = tileWidth / 2.0    -- 0.075

halfDiamondHeight :: Float
halfDiamondHeight = tileDiamondHeight / 2.0  -- 0.0375

-- | Convert grid coordinates to world-space position.
-- Returns the TOP-CENTER of the diamond for tile (gx, gy).
-- This is the anchor point — draw the sprite offset from here.
gridToWorld :: Int -> Int -> (Float, Float)
gridToWorld gx gy =
    let sx = fromIntegral (gx - gy) * halfWidth
        sy = fromIntegral (gx + gy) * halfDiamondHeight
    in (sx, sy)

-- | Convert grid coordinates to sprite draw origin (top-left of quad).
-- This is what you pass to the vertex generator.
gridToScreen :: Int -> Int -> (Float, Float)
gridToScreen gx gy =
    let (cx, cy) = gridToWorld gx gy
    in (cx - halfWidth, cy)

-- | Convert world-space position back to nearest grid coordinates.
-- Useful for mouse picking later.
worldToGrid :: Float -> Float -> (Int, Int)
worldToGrid wx wy =
    let -- Invert the grid-to-world equations:
        -- wx = (gx - gy) * halfWidth
        -- wy = (gx + gy) * halfDiamondHeight
        -- Solve:
        -- gx - gy = wx / halfWidth
        -- gx + gy = wy / halfDiamondHeight
        -- gx = (wx/halfWidth + wy/halfDiamondHeight) / 2
        -- gy = (wy/halfDiamondHeight - wx/halfWidth) / 2
        fgx = (wx / halfWidth + wy / halfDiamondHeight) / 2.0
        fgy = (wy / halfDiamondHeight - wx / halfWidth) / 2.0
    in (round fgx, round fgy)
