{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Screen-pixel → BuildingId hit testing.
--
-- Mirrors 'Unit.HitTest.hitTestUnitAt' but tests against each
-- building's sprite quad. Buildings have multi-tile footprints; the
-- sprite-quad math here is the same as 'Building.Render.buildingToQuad'
-- so the click target matches the visible sprite exactly.
--
-- Returns the building with the highest gridZ that contains the click;
-- on ties, the closer (smaller distance to quad center) wins.
module Building.HitTest
    ( hitTestBuildingAt
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..), resolveActiveWorld)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Viewport (windowDegenerate)
import World.Grid (tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , applyFacingF, GridConfig(..), defaultGridConfig)
import World.Generate (viewDepth)
import Building.Types

baseTileW ∷ Float
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)

baseTileH ∷ Float
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)

-- | Hit test at framebuffer-pixel coordinates. Returns the topmost
--   (highest-Z) building whose sprite quad contains the click.
hitTestBuildingAt ∷ EngineEnv → Double → Double → IO (Maybe BuildingId)
hitTestBuildingAt env pixX pixY = do
    bm       ← readIORef (buildingManagerRef env)
    camera   ← readIORef (cameraRef env)
    (winW, winH) ← readIORef (windowSizeRef env)
    texSizes ← readIORef (textureSizeRef env)
    mgr      ← readIORef (worldManagerRef env)

    -- Only the active world's buildings are clickable (#76) — matches the
    -- render scoping; a hidden world's building must not win the hit-test.
    let instances = case resolveActiveWorld mgr of
            Just (pid, _) → buildingsOnPage pid (bmInstances bm)
            Nothing       → HM.empty
    -- Zero-size window (minimize): the pixel→world divisions below would
    -- yield a non-finite click coord. Report "no building".
    if windowDegenerate winW winH ∨ HM.null instances
        then return Nothing
        else do
            let facing  = camFacing camera
                zoom    = camZoom camera
                zSlice  = camZSlice camera
                -- Match the render cull (Building.Render): visible down
                -- to the terrain view depth, not a fixed 25.
                effDepth = min viewDepth
                               (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
                (camX, camY) = camPosition camera

                -- Screen pixel → world coord (same projection as
                -- Unit.HitTest and the world cursor hit test).
                vw     = zoom * (fromIntegral winW / fromIntegral winH)
                vh     = zoom
                normX  = realToFrac pixX / fromIntegral winW  ∷ Float
                normY  = realToFrac pixY / fromIntegral winH  ∷ Float
                viewX  = (normX * 2.0 - 1.0) * vw
                viewY  = (normY * 2.0 - 1.0) * vh
                worldX = viewX + camX
                worldY = viewY + camY

                candidates =
                    [ (gridZ, dist, bid)
                    | (bid, inst) ← HM.toList instances
                    , let gridZ     = biGridZ inst
                          relativeZ = gridZ - zSlice
                    , gridZ ≤ zSlice
                    , gridZ ≥ zSlice - effDepth
                    , let texHandle = biTexture inst
                          (texW, texH) = case HM.lookup texHandle texSizes of
                              Just (w, h) → (fromIntegral w, fromIntegral h)
                              Nothing     → (baseTileW, baseTileH)
                          scaleX = texW / baseTileW
                          scaleY = texH / baseTileH
                          quadW  = tileWidth  * scaleX
                          quadH  = tileHeight * scaleY
                          -- Mirror buildingToQuad: anchor centered on
                          -- the bottom-left tile of the footprint.
                          gxF = fromIntegral (biAnchorX inst) + 0.5
                          gyF = fromIntegral (biAnchorY inst) + 0.5
                          (faF, fbF) = applyFacingF facing gxF gyF
                          rawX = (faF - fbF) * tileHalfWidth - tileHalfWidth
                          rawY = (faF + fbF) * tileHalfDiamondHeight
                          heightOffset = fromIntegral relativeZ * tileSideHeight
                          drawX = rawX + (tileWidth - quadW) * 0.5
                          drawY = rawY - heightOffset
                                + tileHalfDiamondHeight - quadH
                          cx    = drawX + quadW * 0.5
                          cy    = drawY + quadH * 0.5
                          dx    = worldX - cx
                          dy    = worldY - cy
                    , abs dx ≤ quadW * 0.5
                    , abs dy ≤ quadH * 0.5
                    , let dist = sqrt (dx * dx + dy * dy)
                    ]
            case candidates of
                [] → return Nothing
                cs → let (_, _, bid) = pickBest cs in return (Just bid)
  where
    pickBest = foldr1 $ \a@(za, da, _) b@(zb, db, _) →
                          if za > zb ∨ (za ≡ zb ∧ da < db) then a else b
