{-# LANGUAGE Strict #-}
-- | Screen-pixel → BuildingId hit testing.
--
-- Mirrors 'Unit.HitTest.hitTestUnitAt' but tests against each
-- building's sprite quad. The quad is not computed here: it is the
-- SAME 'Building.Visual.placedBuildingQuad' the renderer draws from
-- (#2088) — the facing's own declared view at the lifecycle frame the
-- progress / clock selects (or the static view of a pre-delivery
-- ghost), sized from that texture, with the sprite-anchor drop applied
-- — so the click target IS the visible quad at every camera facing. A
-- pixel that lies only inside some other view's bounds does not hit.
--
-- What stays this module's own is the POLICY around the quad: only the
-- active world is clickable, the z-slice / view-depth band, the
-- degenerate-window guard, highest grid z wins, and equal-z ties
-- prefer the closer quad centre. Hit-testing is quad-based, not
-- per-alpha-pixel.
module Building.HitTest
    ( hitTestBuildingAt
    ) where

import UPrelude
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv, buildingManagerRef
  , resolveActiveWorld )
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Viewport (windowDegenerate)
import World.Generate (viewDepth)
import Building.Types
import Building.Visual (BuildingQuadRect(..), placedBuildingQuad)

-- | Hit test at framebuffer-pixel coordinates. Returns the topmost
--   (highest-Z) building whose sprite quad contains the click.
hitTestBuildingAt ∷ EngineEnv → Double → Double → IO (Maybe BuildingId)
hitTestBuildingAt env pixX pixY = do
    bm       ← readIORef (buildingManagerRef env)
    let rv = toRenderViewCapability env
    camera   ← readIORef (rvCameraRef rv)
    (winW, winH) ← readIORef (rvWindowSizeRef rv)
    texSizes ← readIORef (rvTextureSizeRef rv)
    mgr      ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    -- Same game clock the renderer reads, so the target is sized from
    -- the frame that is actually on screen this tick.
    now      ← readIORef (wsGameTimeRef (toWorldSimCapability env))

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
                    , let gridZ = biGridZ inst
                    , gridZ ≤ zSlice
                    , gridZ ≥ zSlice - effDepth
                    , let mDef = HM.lookup (biDefName inst) (bmDefs bm)
                          (_, BuildingQuadRect
                                { bqX = drawX, bqY = drawY
                                , bqW = quadW, bqH = quadH }) =
                              placedBuildingQuad facing now zSlice texSizes
                                                 inst mDef
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
