{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Screen-pixel → UnitId hit testing.
--
-- Given mouse coordinates in framebuffer pixels, find which (if any)
-- spawned unit is under the cursor. Mirrors the screen→world projection
-- in `World/Render/Quads.hs::renderWorldCursorQuads::hitTest` and the
-- per-unit sprite math in `Unit/Render.hs::unitToQuad`.
--
-- Returns the unit with the highest gridZ that contains the click —
-- so clicking a tile with two stacked units selects the one on top.
module Unit.HitTest
    ( hitTestUnitAt
    , hitTestUnitsInRect
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..), resolveActiveWorld)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Viewport (windowDegenerate)
import World.Grid (tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , applyFacingF, GridConfig(..), defaultGridConfig)
import World.Generate (viewDepth)
import Unit.Types
import Unit.Direction (Direction)
import Unit.Sprite (resolveTexture)

baseTileW ∷ Float
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)

baseTileH ∷ Float
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)

-- | Hit test at framebuffer-pixel coordinates. Returns the topmost
--   (highest-Z) unit whose sprite quad contains the click, or Nothing.
hitTestUnitAt ∷ EngineEnv → Double → Double → IO (Maybe UnitId)
hitTestUnitAt env pixX pixY = do
    um       ← readIORef (unitManagerRef env)
    camera   ← readIORef (cameraRef env)
    (winW, winH) ← readIORef (windowSizeRef env)
    texSizes ← readIORef (textureSizeRef env)
    mgr      ← readIORef (worldManagerRef env)

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
                    , let gridZ     = uiGridZ inst
                          relativeZ = gridZ - zSlice
                    , gridZ ≤ zSlice
                    , gridZ ≥ zSlice - effDepth
                    , let texHandle = resolveTextureH facing (uiFacing inst)
                                                      (uiDirSprites inst)
                                                      (uiTexture inst)
                          (texW, texH) = case HM.lookup texHandle texSizes of
                              Just (w, h) → (fromIntegral w, fromIntegral h)
                              Nothing     → (baseTileW, baseTileH)
                          scaleX = texW / baseTileW
                          scaleY = texH / baseTileH
                          quadW  = tileWidth  * scaleX
                          quadH  = tileHeight * scaleY
                          gxF    = uiGridX inst
                          gyF    = uiGridY inst
                          (faF, fbF) = applyFacingF facing gxF gyF
                          rawX = (faF - fbF) * tileHalfWidth - tileHalfWidth
                          rawY = (faF + fbF) * tileHalfDiamondHeight
                          heightOffset = fromIntegral relativeZ * tileSideHeight
                          baseRadius   = uiBaseWidth inst * 0.5
                                       / baseTileH * tileHeight
                          drawX = rawX + (tileWidth - quadW) * 0.5
                          -- Must match Unit.Render.unitToQuad: continuous
                          -- position → rawY is already the ground point,
                          -- so NO tileHalfDiamondHeight (that's the
                          -- apex→centre shift only flora/items need).
                          drawY = rawY - heightOffset - quadH + baseRadius
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
    camera   ← readIORef (cameraRef env)
    (winW, winH) ← readIORef (windowSizeRef env)
    texSizes ← readIORef (textureSizeRef env)
    mgr      ← readIORef (worldManagerRef env)

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
            let texHandle = resolveTextureH facing (uiFacing inst)
                                              (uiDirSprites inst)
                                              (uiTexture inst)
                (texW, texH) = case HM.lookup texHandle texSizes of
                    Just (w, h) → (fromIntegral w, fromIntegral h)
                    Nothing     → (baseTileW, baseTileH)
                scaleX = texW / baseTileW
                scaleY = texH / baseTileH
                quadW  = tileWidth  * scaleX
                quadH  = tileHeight * scaleY
                gxF    = uiGridX inst
                gyF    = uiGridY inst
                (faF, fbF) = applyFacingF facing gxF gyF
                rawX = (faF - fbF) * tileHalfWidth - tileHalfWidth
                rawY = (faF + fbF) * tileHalfDiamondHeight
                relativeZ    = uiGridZ inst - zSlice
                heightOffset = fromIntegral relativeZ * tileSideHeight
                baseRadius   = uiBaseWidth inst * 0.5
                             / baseTileH * tileHeight
                drawX = rawX + (tileWidth - quadW) * 0.5
                -- Match Unit.Render.unitToQuad (no tileHalfDiamondHeight —
                -- rawY from the continuous position is already the ground
                -- point; see the render note).
                drawY = rawY - heightOffset - quadH + baseRadius
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

-- | Resolve which texture handle the unit displays for a given camera
--   facing, for hit-box SIZING. Delegates to the shared
--   'Unit.Sprite.resolveTexture' so the hit-box is sized from the same
--   sprite the renderer draws — including the 'mirrorDir' fallback that
--   produces W/SW/NW from their eastern counterparts. (A previous copy
--   here omitted that fallback, so those units' hit-boxes were sized
--   from the default texture instead — #389.) The flip flag doesn't
--   affect sprite dimensions, so we drop it.
resolveTextureH
    ∷ CameraFacing
    → Direction
    → Map.Map Direction TextureHandle
    → TextureHandle
    → TextureHandle
resolveTextureH camFacing unitFacing dirSprites fallback =
    fst (resolveTexture camFacing unitFacing dirSprites fallback)
