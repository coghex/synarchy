-- | Screen-pixel → world-tile hit-test.
--
-- This is the single source of truth for unprojecting a screen pixel to
-- the tile under it, accounting for the isometric tilt, camera facing,
-- elevation, z-slice, and the u-wrap chunk-visibility test. Both the
-- per-frame render hover resolution ('World.Render.CursorQuads') and the
-- synchronous Lua pick (@world.pickTile@) call this so they can never
-- drift — a drift here would silently place buildings on the wrong tile.
--
-- == The tile-coordinate frame contract (#1175)
--
-- Chunks are STORED under u-wrapped (canonical) coords. This function is
-- where a screen pixel first becomes a tile coord, so it is the head of
-- the frame contract every downstream designation coord obeys:
--
--   1. __What a pick reports is canonical.__ @pickWorldTile@, and every
--      Lua caller it backs (@world.pickTile@ / @pickPos@ / @getHoverTile@
--      / @getHoverPos@), report stored-frame coords. The fractional
--      hover position receives the SAME whole-tile shift as the integer
--      tile, so the two never name different places.
--   2. __What a designation stores is canonical.__ Every designation map
--      (mine / chop / till / plant / construct) is keyed canonically, so
--      one physical tile has exactly one key.
--   3. __What a Lua caller may pass is any alias.__ Point reads,
--      mutations and cancellations canonicalise their argument, so a
--      coord kept across a save, or computed in some other frame, still
--      resolves. Coords the engine RETURNS are canonical.
--   4. __Rectangles are formed in the anchor's local alias frame.__
--      Canonical coords are a storage frame, not a geometry frame: two
--      physically adjacent tiles across the seam sit at opposite ends of
--      the canonical range. A drag's second endpoint is re-expressed
--      relative to its anchor ('World.Generate.Coordinates.localizeTileToAnchor')
--      BEFORE any clamp or @min@/@max@, and each enumerated tile is
--      canonicalised only at lookup / storage.
--   5. __Away from the seam every step above is the identity__, as it is
--      for arena / non-wrapping worlds.
--
-- Seam VISIBILITY is the separate axis #1176 owns: the wrap offset
-- 'World.Render.ChunkCulling.bestWrapOffset' returns is a facing-aware
-- @(x, y)@ pair, because a u-wrap displaces screen Y rather than screen
-- X at east/west. The two halves compose — this function resolves WHICH
-- tile a pixel names, that one decides where the tile is drawn — and
-- both hold at all four facings.
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
import World.Generate.Coordinates (canonicalTileFrame)
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
            (rawGX, rawGY) = worldToGrid facing worldX adjustedWorldY
            (rawHX, rawHY) = worldToGridF facing worldX
                (worldY + fromIntegral relZ * tileSideHeight)
            -- #1175: the camera is wrapped into the canonical range but
            -- the viewport around it is not, so near the seam the far
            -- half of the screen unprojects to a coord whose chunk is
            -- stored under the wrapped alias. Resolving that alias to
            -- the stored frame is what makes the lookup hit — and the
            -- REPORTED coords move with it, integer tile and fractional
            -- hover position taking the identical whole-tile shift, so a
            -- caller can never receive a tile and a position naming
            -- different places. Identity away from the seam.
            (chunkCoord, (lx, ly), (dgx, dgy)) =
                canonicalTileFrame worldSize rawGX rawGY
            gx = rawGX + dgx
            gy = rawGY + dgy
            hoverPos = ( rawHX + fromIntegral dgx
                       , rawHY + fromIntegral dgy )
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
                        -- The lookup above is seam-aware (#1175) and the
                        -- offset this returns is facing-aware (#1176), so
                        -- a seam-side pixel now both resolves its tile and
                        -- reports where that tile is actually drawn.
                        then case isChunkVisibleWrapped facing worldSize vb
                                      camX camY chunkCoord of
                               Just wrapOff → Just (gx, gy, z, wrapOff, hoverPos)
                               Nothing      → tryZ (z - 1)
                        else tryZ (z - 1)
