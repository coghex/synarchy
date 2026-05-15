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
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import World.Grid (tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , applyFacingF, GridConfig(..), defaultGridConfig)
import Unit.Types
import Unit.Direction (Direction(..), dirIndex, indexToDir)

baseTileW ∷ Float
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)

baseTileH ∷ Float
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)

cameraRotSteps ∷ CameraFacing → Int
cameraRotSteps FaceSouth = 0
cameraRotSteps FaceWest  = 2
cameraRotSteps FaceNorth = 4
cameraRotSteps FaceEast  = 6

-- | Hit test at framebuffer-pixel coordinates. Returns the topmost
--   (highest-Z) unit whose sprite quad contains the click, or Nothing.
hitTestUnitAt ∷ EngineEnv → Double → Double → IO (Maybe UnitId)
hitTestUnitAt env pixX pixY = do
    um       ← readIORef (unitManagerRef env)
    camera   ← readIORef (cameraRef env)
    (winW, winH) ← readIORef (windowSizeRef env)
    texSizes ← readIORef (textureSizeRef env)

    let instances = umInstances um
    if HM.null instances
        then return Nothing
        else do
            let facing  = camFacing camera
                zoom    = camZoom camera
                zSlice  = camZSlice camera
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
                    , gridZ ≥ zSlice - 25
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
                          drawY = rawY - heightOffset
                                + tileHalfDiamondHeight - quadH + baseRadius
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

    let x1 = realToFrac (min x1d x2d) ∷ Float
        x2 = realToFrac (max x1d x2d) ∷ Float
        y1 = realToFrac (min y1d y2d) ∷ Float
        y2 = realToFrac (max y1d y2d) ∷ Float

        instances = umInstances um
        facing  = camFacing camera
        zoom    = camZoom camera
        zSlice  = camZSlice camera
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
                drawY = rawY - heightOffset
                      + tileHalfDiamondHeight - quadH + baseRadius
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
            in z ≤ zSlice ∧ z ≥ zSlice - 25 ∧
               let (pixX, pixY) = worldToPixel (unitCenter inst)
               in pixX ≥ x1 ∧ pixX ≤ x2 ∧ pixY ≥ y1 ∧ pixY ≤ y2

    return [uid | (uid, inst) ← HM.toList instances, inRect inst]

-- | Resolve which texture handle the unit displays for a given camera
--   facing. Duplicated from Unit.Render so HitTest doesn't import the
--   whole renderer.
resolveTextureH
    ∷ CameraFacing
    → Direction
    → Map.Map Direction TextureHandle
    → TextureHandle
    → TextureHandle
resolveTextureH camFacing unitFacing dirSprites fallback
    | Map.null dirSprites = fallback
    | otherwise =
        let screenIdx = (dirIndex unitFacing - cameraRotSteps camFacing) `mod` 8
            screenDir = indexToDir screenIdx
        in case Map.lookup screenDir dirSprites of
            Just h  → h
            Nothing → fallback
