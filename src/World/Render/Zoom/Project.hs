{-# LANGUAGE Strict #-}

-- | Where the ZOOM MAP draws one tile, in window pixels (#2485).
--
--   The detailed hit test ('World.Render.HitTest.pickWorldTile') cannot
--   answer this. It walks terrain z and unprojects through
--   @(z - zSlice) * tileSideHeight - tileHeight / 2@, while the zoom map
--   maps its atlas UVs over an elevation-free 'gridToWorld' rectangle
--   and assigns texels by an inverse isometric transform — so the two
--   agree only by accident, and a caller that needs a tile's ATLAS
--   footprint (an offscreen probe grading a live zoom refresh, say) gets
--   a padded terrain footprint instead.
--
--   This is that projection, run forwards: the same chunk rectangle
--   'World.Render.Zoom.Bake.bakeEntriesAtlas' bakes, the same wrap
--   offset 'World.Render.Zoom.Quads.makeMapQuads' applies, the same
--   texel transform 'World.ZoomMap.Types.zoomTexelTile' colours through,
--   and the inverse of the pixel-to-world unprojection the hit test
--   shares with the camera.
module World.Render.Zoom.Project
    ( zoomTileScreenRect
    , zoomTexelExtent
    ) where

import UPrelude
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Viewport (viewportDegenerate)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Generate.Coordinates (canonicalTileFrame)
import World.Grid (gridToWorld)
import World.Render.Zoom.ViewBounds (bestZoomWrapOffset)
import World.ZoomMap.Types (zoomTileSize, zoomTexelTile)

-- | The texels of a chunk's atlas block that belong to one LOCAL tile,
--   as an inclusive @(minX, minY, maxX, maxY)@ box.
--
--   'Nothing' for a local coordinate the block has no texel for, which
--   is how an out-of-chunk argument answers rather than by producing an
--   empty rectangle somewhere plausible.
zoomTexelExtent ∷ Int → Int → Maybe (Int, Int, Int, Int)
zoomTexelExtent lx ly
    -- A texel CAN map outside the chunk — those are the transparent
    -- corners of the square block, which 'generateChunkPixels' rejects
    -- rather than colouring — so an out-of-chunk local has no drawn
    -- texels however many texels name it.
    | lx < 0 ∨ lx ≥ chunkSize ∨ ly < 0 ∨ ly ≥ chunkSize = Nothing
    | null hits = Nothing
    | otherwise = Just ( minimum (map fst hits), minimum (map snd hits)
                       , maximum (map fst hits), maximum (map snd hits) )
  where
    hits = [ (px, py)
           | py ← [0 .. zoomTileSize - 1]
           , px ← [0 .. zoomTileSize - 1]
           , zoomTexelTile px py ≡ (lx, ly) ]

-- | The window-pixel rectangle @(x, y, w, h)@ the zoom map draws tile
--   @(gx, gy)@ into, or 'Nothing' when the viewport is degenerate.
--
--   The rectangle is where the tile's texels land, not where its terrain
--   would be picked: a tile is roughly a 2x2 patch of a
--   @zoomTileSize@-square block, so this is small by construction.
--
--   @(gx, gy)@ is canonicalized against @worldSize@ before anything is
--   projected, so every u-alias of a tile — at any number of wrap
--   periods — answers with the one rectangle the map drew it into.
zoomTileScreenRect
    ∷ CameraFacing
    → Float             -- ^ zoom
    → Float → Float     -- ^ camera x, y
    → Int → Int         -- ^ framebuffer width, height (for aspect)
    → Int → Int         -- ^ window width, height (for norm→pixel)
    → Int               -- ^ world size in chunks (the u wrap)
    → Int → Int         -- ^ global tile
    → Maybe (Float, Float, Float, Float)
zoomTileScreenRect facing zoom camX camY fbW fbH winW winH worldSize gx gy
    | viewportDegenerate winW winH fbW fbH = Nothing
    | otherwise = do
        (tx0, ty0, tx1, ty1) ← zoomTexelExtent lx ly
        let ts = fromIntegral zoomTileSize
            -- The texel box as a fraction of the block, then as world
            -- coordinates inside the chunk's own quad.
            wx0 = drawX + offX + blockW * (fromIntegral tx0 / ts)
            wy0 = drawY + offY + blockH * (fromIntegral ty0 / ts)
            wx1 = drawX + offX + blockW * (fromIntegral (tx1 + 1) / ts)
            wy1 = drawY + offY + blockH * (fromIntegral (ty1 + 1) / ts)
            (px0, py0) = project wx0 wy0
            (px1, py1) = project wx1 wy1
        pure (px0, py0, px1 - px0, py1 - py0)
  where
    -- Canonicalized FIRST, against the same wrap the quads use. A
    -- point query accepts any u-alias and answers about the tile the
    -- page actually stores (CLAUDE.md §Tile coordinates), and a bare
    -- 'globalToChunk' would leave that to 'bestZoomWrapOffset' — which
    -- only ever tries ONE screen wrap in either direction, so an alias
    -- two or more periods out would project to a rectangle the map
    -- never drew rather than to the tile it did.
    (ChunkCoord ccx ccy, (lx, ly), _) = canonicalTileFrame worldSize gx gy
    baseGX = ccx * chunkSize
    baseGY = ccy * chunkSize

    -- The same rectangle 'bakeEntriesAtlas' bakes: the axis-aligned box
    -- of the chunk's diamond, with UV 0..1 across it.
    (x0, y0) = gridToWorld facing baseGX baseGY
    (x1, y1) = gridToWorld facing (baseGX + chunkSize) baseGY
    (x2, y2) = gridToWorld facing baseGX (baseGY + chunkSize)
    (x3, y3) = gridToWorld facing (baseGX + chunkSize) (baseGY + chunkSize)
    drawX  = minimum [x0, x1, x2, x3]
    drawY  = minimum [y0, y1, y2, y3]
    blockW = maximum [x0, x1, x2, x3] - drawX
    blockH = maximum [y0, y1, y2, y3] - drawY
    -- …and the same wrap offset 'makeMapQuads' applies before drawing.
    (offX, offY) = bestZoomWrapOffset facing worldSize camX camY
                       (drawX + blockW / 2) (drawY + blockH / 2)

    -- The inverse of the camera unprojection the hit test uses.
    aspect = fromIntegral fbW / fromIntegral fbH
    project wx wy =
        ( ((wx - camX) / (zoom * aspect) + 1) / 2 * fromIntegral winW
        , ((wy - camY) / zoom + 1) / 2 * fromIntegral winH )
