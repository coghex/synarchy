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

-- | Resolved hit: @(gx, gy, z, wrapOffset, hoverPos)@ where @(gx,gy,z)@
--   is the solid tile under the cursor, @wrapOffset@ is the
--   wrapped-chunk screen shift as an @(x, y)@ pair — facing-aware since
--   #1176, because a u-wrap displaces screen Y and not screen X at
--   east/west facings — and @hoverPos@ is the fractional grid position
--   (item/unit convention) at the click point.
type HitResult = (Int, Int, Int, (Float, Float), (Float, Float))

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
        -- Raw lookup, and KNOWN INCOMPLETE at the U seam (#1135 audit,
        -- deferred to #1175 — not a justification, a recorded finding).
        --
        -- The camera is wrapped into the canonical range but the
        -- viewport around it is not, so near the seam the far half of
        -- the screen unprojects to a coord whose chunk is stored under
        -- the wrapped alias: this misses, and tryZ walks down to "no
        -- tile". Canonicalising HERE is not a local fix. This function's
        -- result is the frame every designation coord downstream lives
        -- in — anchors, rectangle corners, cancel and read keys, and the
        -- coords scripts/unit_ai.lua stores across ticks. Shifting only
        -- this one end makes those frames disagree, which is strictly
        -- worse than the current uniform-but-seam-blind behaviour: it
        -- was measured to turn a seam-crossing two-click drag into a
        -- cap-sized sweep of unrelated tiles, because two physically
        -- adjacent picks come back a whole world apart. The fix is to
        -- normalise the pick + designation frame together (form
        -- rectangles in the anchor's local alias frame; canonicalise
        -- per tile only at lookup/storage; do it across
        -- create/read/cancel/nearest for all five tools) — see #1175.
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
                        -- The 2-D offset propagates whenever the raw
                        -- lookup above DOES resolve (#1176); making the
                        -- lookup itself seam-aware is #1175's job, as
                        -- the note above records.
                        then case isChunkVisibleWrapped facing worldSize vb
                                      camX camY chunkCoord of
                               Just wrapOff → Just (gx, gy, z, wrapOff, hoverPos)
                               Nothing      → tryZ (z - 1)
                        else tryZ (z - 1)
