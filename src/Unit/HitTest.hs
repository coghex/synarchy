{-# LANGUAGE Strict #-}
-- | Screen-pixel → UnitId hit testing.
--
-- Given mouse coordinates in framebuffer pixels, find which (if any)
-- spawned unit is under the cursor. Mirrors the screen→world projection
-- in `World/Render/CursorQuads.hs::renderWorldCursorQuads::hitTest` and
-- the per-unit sprite math in `Unit/Render.hs::unitToQuad`.
--
-- The hit box is sized from the frame the renderer is DRAWING
-- (`Unit.Render.pickFrame`, via `unitHitRect`), not from the static
-- T-pose it once used: with atlas storage a frame's texture handle
-- names the whole animation sheet, so only the sample knows the cell
-- size (#1259).
--
-- Returns the unit with the highest gridZ that contains the click —
-- so clicking a tile with two stacked units selects the one on top.
module Unit.HitTest
    ( hitTestUnitAt
    , hitTestUnitsInRect
    , unitHitRect
    , frameSampleOf
    ) where

import UPrelude
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv, unitManagerRef
  , resolveActiveWorld )
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Viewport (windowDegenerate)
import World.Grid (tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , applyFacingF, baseTileW, baseTileH)
import World.Generate (viewDepth)
import Unit.Types
import Unit.Render (pickFrame)
import Unit.Sprite (resolveTexture)

-- | Hit test at framebuffer-pixel coordinates. Returns the topmost
--   (highest-Z) unit whose sprite quad contains the click, or Nothing.
hitTestUnitAt ∷ EngineEnv → Double → Double → IO (Maybe UnitId)
hitTestUnitAt env pixX pixY = do
    um       ← readIORef (unitManagerRef env)
    let rv = toRenderViewCapability env
    camera   ← readIORef (rvCameraRef rv)
    (winW, winH) ← readIORef (rvWindowSizeRef rv)
    texSizes ← readIORef (rvTextureSizeRef rv)
    mgr      ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    -- Same game clock the renderer reads, so the hit box is sized from
    -- the frame that is actually on screen this tick.
    now      ← readIORef (wsGameTimeRef (toWorldSimCapability env))

    -- Only the active world's units are clickable (#78).
    let instances = case resolveActiveWorld mgr of
            Just (pid, _) → unitsOnPage pid (umInstances um)
            Nothing       → HM.empty
    -- Zero-size window (minimize): the pixel→world divisions below would
    -- yield a non-finite click coord. Report "no unit".
    if windowDegenerate winW winH ∨ HM.null instances
        then return Nothing
        else do
            let facing  = camFacing camera
                zoom    = camZoom camera
                zSlice  = camZSlice camera
                -- Match the render cull (Unit.Render): visible down to
                -- the terrain view depth, not a fixed 25.
                effDepth = min viewDepth
                               (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
                (camX, camY) = camPosition camera

                -- Screen pixel → world coord. Same math as the tile
                -- hit-test in `renderWorldCursorQuads::hitTest`:
                --   normX/Y in [0..1]
                --   viewX/Y in [-vw..vw] / [-vh..vh] world units
                --   worldX/Y = camera position + view offset
                vw     = zoom * (fromIntegral winW / fromIntegral winH)
                vh     = zoom
                normX  = realToFrac pixX / fromIntegral winW  ∷ Float
                normY  = realToFrac pixY / fromIntegral winH  ∷ Float
                viewX  = (normX * 2.0 - 1.0) * vw
                viewY  = (normY * 2.0 - 1.0) * vh
                worldX = viewX + camX
                worldY = viewY + camY

                -- Per-unit hit test. Each candidate produces
                -- (gridZ, distance) on hit; we take the largest gridZ
                -- (closest to camera), breaking ties by smallest distance.
                candidates =
                    [ (gridZ, dist, uid)
                    | (uid, inst) ← HM.toList instances
                    , let gridZ = uiGridZ inst
                    , gridZ ≤ zSlice
                    , gridZ ≥ zSlice - effDepth
                    , let (drawX, drawY, quadW, quadH) =
                              unitHitRect facing zSlice texSizes
                                  (frameSampleOf now facing (umDefs um) inst) inst
                          -- Sprite quad center
                          cx    = drawX + quadW * 0.5
                          cy    = drawY + quadH * 0.5
                          dx    = worldX - cx
                          dy    = worldY - cy
                    -- Inside the sprite quad bounding box
                    , abs dx ≤ quadW * 0.5
                    , abs dy ≤ quadH * 0.5
                    , let dist = sqrt (dx * dx + dy * dy)
                    ]
            case candidates of
                [] → return Nothing
                cs → let (_, _, uid) = pickBest cs in return (Just uid)
  where
    pickBest = foldr1 $ \a@(za, da, _) b@(zb, db, _) →
                          if za > zb ∨ (za ≡ zb ∧ da < db) then a else b

-- | Hit test all units whose sprite-quad CENTER lies inside the given
--   screen-space rect (window pixels). Used by drag-box selection.
--
--   Order of corners doesn't matter — we normalise to min/max.
hitTestUnitsInRect
    ∷ EngineEnv → Double → Double → Double → Double → IO [UnitId]
hitTestUnitsInRect env x1d y1d x2d y2d = do
    um       ← readIORef (unitManagerRef env)
    let rv = toRenderViewCapability env
    camera   ← readIORef (rvCameraRef rv)
    (winW, winH) ← readIORef (rvWindowSizeRef rv)
    texSizes ← readIORef (rvTextureSizeRef rv)
    mgr      ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    now      ← readIORef (wsGameTimeRef (toWorldSimCapability env))

    let x1 = realToFrac (min x1d x2d) ∷ Float
        x2 = realToFrac (max x1d x2d) ∷ Float
        y1 = realToFrac (min y1d y2d) ∷ Float
        y2 = realToFrac (max y1d y2d) ∷ Float

        -- Only the active world's units are selectable (#125) — mirrors
        -- the same filter in hitTestUnitAt so box-select and click-select
        -- agree and drag-select never grabs hidden-page units.
        instances = case resolveActiveWorld mgr of
            Just (pid, _) → unitsOnPage pid (umInstances um)
            Nothing       → HM.empty
        facing  = camFacing camera
        zoom    = camZoom camera
        zSlice  = camZSlice camera
        effDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
        (camX, camY) = camPosition camera
        vw      = zoom * (fromIntegral winW / fromIntegral winH)
        vh      = zoom

        -- World coord of the unit's sprite-quad center. Mirrors the
        -- math in hitTestUnitAt for consistency with click selection.
        unitCenter inst =
            let (drawX, drawY, quadW, quadH) =
                    unitHitRect facing zSlice texSizes
                        (frameSampleOf now facing (umDefs um) inst) inst
            in (drawX + quadW * 0.5, drawY + quadH * 0.5)

        -- World → screen pixel (inverse of hitTestUnitAt's projection).
        worldToPixel (cx, cy) =
            let viewX = cx - camX
                viewY = cy - camY
                normX = (viewX / vw + 1.0) / 2.0
                normY = (viewY / vh + 1.0) / 2.0
            in (normX * fromIntegral winW, normY * fromIntegral winH)

        inRect inst =
            let z = uiGridZ inst
            in z ≤ zSlice ∧ z ≥ zSlice - effDepth ∧
               let (pixX, pixY) = worldToPixel (unitCenter inst)
               in pixX ≥ x1 ∧ pixX ≤ x2 ∧ pixY ≥ y1 ∧ pixY ≤ y2

    -- Zero-size window (minimize): the projection above maps every unit
    -- to a non-finite screen pixel. Select nothing.
    return $ if windowDegenerate winW winH
                then []
                else [uid | (uid, inst) ← HM.toList instances, inRect inst]

-- | The screen-space rect a unit's sprite quad occupies, as
--   @(x, y, width, height)@ — the ONE hit-box geometry, shared by click
--   and box selection so the two cannot drift from each other.
--
--   The size comes from the FRAME ('frameDimensions'), which for an
--   atlas sample is the cell and never the whole sheet (#1259
--   requirement 4). The rest mirrors 'Unit.Render.unitToQuad': the
--   continuous position means @rawY@ is already the ground point, so
--   there is NO @tileHalfDiamondHeight@ term (that apex→centre shift is
--   only flora's and ground items'). The one deliberate difference from
--   the renderer is the height offset, which uses the INTEGER
--   @uiGridZ@ here against the renderer's continuous @uiRealZ@ — hit
--   testing is per-tile.
unitHitRect
    ∷ CameraFacing
    → Int                                    -- ^ camera z-slice
    → HM.HashMap TextureHandle (Int, Int)
    → FrameSample
    → UnitInstance
    → (Float, Float, Float, Float)
unitHitRect facing zSlice texSizes sample inst =
    let (texW, texH) = frameDimensions texSizes (baseTileW, baseTileH) sample
        scaleX = texW / baseTileW
        scaleY = texH / baseTileH
        quadW  = tileWidth  * scaleX
        quadH  = tileHeight * scaleY
        (faF, fbF) = applyFacingF facing (uiGridX inst) (uiGridY inst)
        rawX = (faF - fbF) * tileHalfWidth - tileHalfWidth
        rawY = (faF + fbF) * tileHalfDiamondHeight
        relativeZ    = uiGridZ inst - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        baseRadius   = uiBaseWidth inst * 0.5 / baseTileH * tileHeight
        drawX = rawX + (tileWidth - quadW) * 0.5
        drawY = rawY - heightOffset - quadH + baseRadius
    in (drawX, drawY, quadW, quadH)

-- | The frame the renderer is drawing for this unit right now, for
--   hit-box SIZING.
--
--   Delegates to 'Unit.Render.pickFrame' — the very function
--   'Unit.Render.unitToQuad' calls — so the hit box is sized from the
--   same frame that is on screen, including the 'mirrorDir' fallback
--   that produces W/SW/NW from their eastern counterparts. (A copy here
--   once omitted that fallback, so those units' hit-boxes were sized
--   from the default texture instead — #389.)
--
--   This deliberately supersedes the older T-pose-only sizing (#1259
--   requirement 4): with atlas storage the whole-image dimensions of a
--   frame's texture are the SHEET's, so "size it from the handle" no
--   longer means "size it from the frame", and the only way for click
--   and box selection to agree with what is painted is to resolve the
--   same 'FrameSample'. A unit whose def is not loaded keeps the
--   directional T-pose fallback, exactly as the renderer does.
frameSampleOf
    ∷ Double
    → CameraFacing
    → HM.HashMap Text UnitDef
    → UnitInstance
    → FrameSample
frameSampleOf now facing defs inst =
    case HM.lookup (uiDefName inst) defs of
        Just def → pickFrame now facing inst def
        Nothing  → uncurry wholeImageSample $
            resolveTexture facing (uiFacing inst)
                           (uiDirSprites inst) (uiTexture inst)
