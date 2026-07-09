{-# LANGUAGE UnicodeSyntax #-}
-- | Screen-pixel → world-tile hit-test.
--
-- This is the single source of truth for unprojecting a screen pixel to
-- the tile under it, accounting for the isometric tilt, camera facing,
-- elevation, z-slice, and the u-wrap chunk-visibility test. Both the
-- per-frame render hover resolution ('World.Render.CursorQuads') and the
-- synchronous Lua pick (@world.pickTile@) call this so they can never
-- drift — a drift here would silently place buildings on the wrong tile.
module World.Render.HitTest
    ( HitResult
    , pickWorldTile
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Viewport (viewportDegenerate)
import World.Tile.Types (WorldTileData(..))
import World.Chunk.Types (LoadedChunk(..), ColumnTiles(..), columnIndex)
import World.Grid (worldToGrid, worldToGridF, tileSideHeight, tileHeight)
import World.Generate.Coordinates (globalToChunk)
import World.Render.ViewBounds (ViewBounds)
import World.Render.ChunkCulling (isChunkVisibleWrapped)

-- | Resolved hit: @(gx, gy, z, xOffset, hoverPos)@ where @(gx,gy,z)@ is
--   the solid tile under the cursor, @xOffset@ is the wrapped-chunk x
--   offset, and @hoverPos@ is the fractional grid position (item/unit
--   convention) at the click point.
type HitResult = (Int, Int, Int, Float, (Float, Float))

-- | Unproject a screen pixel to the tile under it. Mirror of the inline
--   hit-test that drives @worldHoverTile@ each frame; see that comment in
--   'World.Render.CursorQuads' for the elevation / fractional-position
--   rationale.
--
--   The arithmetic is identical to the render path; the parameters are the
--   exact render-frame locals it used (camera facing/zoom/z-slice/position,
--   framebuffer + window dims, world size, effective depth, view bounds and
--   the live tile data).
pickWorldTile
    ∷ CameraFacing      -- ^ camera facing
    → Float             -- ^ zoom
    → Int               -- ^ z-slice
    → Float             -- ^ camera x
    → Float             -- ^ camera y
    → Int → Int         -- ^ framebuffer width, height (for aspect)
    → Int → Int         -- ^ window width, height (for pixel→norm)
    → Int               -- ^ world size
    → Int               -- ^ effective depth
    → ViewBounds        -- ^ view bounds
    → WorldTileData     -- ^ live tile data
    → Int → Int         -- ^ screen pixel x, y
    → Maybe HitResult
pickWorldTile facing zoom zSlice camX camY fbW fbH winW winH
              worldSize effectiveDepth vb tileData pixX pixY
    -- Zero-size window/framebuffer (minimize): the aspect and pixel→norm
    -- divisions below would unproject to a non-finite (or centerline-
    -- collapsed) world coord and pick a garbage tile. Report "no tile".
    | viewportDegenerate winW winH fbW fbH = Nothing
    | otherwise = tryZ zSlice
  where
    aspect = fromIntegral fbW / fromIntegral fbH
    vw     = zoom * aspect
    vh     = zoom
    normX  = fromIntegral pixX / fromIntegral winW
    normY  = fromIntegral pixY / fromIntegral winH
    viewX  = (normX * 2.0 - 1.0) * vw
    viewY  = (normY * 2.0 - 1.0) * vh
    worldX = viewX + camX
    worldY = viewY + camY
    zMin   = zSlice - effectiveDepth

    tryZ z
      | z < zMin  = Nothing
      | otherwise =
        let relZ = z - zSlice
            adjustedWorldY = worldY + fromIntegral relZ * tileSideHeight
                           - tileHeight * 0.5
            (gx, gy) = worldToGrid facing worldX adjustedWorldY
            hoverPos = worldToGridF facing worldX
                (worldY + fromIntegral relZ * tileSideHeight)
            (chunkCoord, (lx, ly)) = globalToChunk gx gy
        in case HM.lookup chunkCoord (wtdChunks tileData) of
            Nothing → tryZ (z - 1)
            Just lc →
                let idx = columnIndex lx ly
                    col = lcTiles lc V.! idx
                    colLen  = VU.length (ctMats col)
                    colMinZ = ctStartZ col
                    i = z - colMinZ
                in if i < 0 ∨ i >= colLen
                   then tryZ (z - 1)
                   else if ctMats col VU.! i ≠ 0
                        then case isChunkVisibleWrapped facing worldSize vb camX chunkCoord of
                               Just xOff → Just (gx, gy, z, xOff, hoverPos)
                               Nothing   → tryZ (z - 1)
                        else tryZ (z - 1)
