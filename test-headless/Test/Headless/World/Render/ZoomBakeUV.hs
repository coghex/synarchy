{-# LANGUAGE UnicodeSyntax #-}
-- | Regression guard for the zoom map's per-corner longitude gradient
--   (#483 review follow-up): 'zoomQuadWorldUVs' must recover the TRUE
--   (u,v) at each of a baked chunk quad's four screen-space RECTANGLE
--   corners, not just tag the whole quad with one value — otherwise
--   the terminator steps at chunk boundaries instead of sweeping
--   smoothly, and a naive per-corner assignment would run backwards
--   under a rotated camera facing (screenX/screenY is a SIGNED
--   PERMUTATION of u/v that differs per facing).
module Test.Headless.World.Render.ZoomBakeUV (spec) where

import UPrelude
import Data.List (sort)
import Test.Hspec
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (packUV)
import World.Render.Zoom.Bake (zoomQuadWorldUVs)

-- A chunk whose four corners' (u,v) are NOT all equal, so a
-- meaningfully distinct value per rectangle corner is checkable.
-- baseGX=0, baseGY=0, chunkSize=16 (via World.Chunk.Types).
spec ∷ Spec
spec = describe "zoomQuadWorldUVs" $ do
    it "FaceSouth: recovers the bounding min/max (u,v) directly (identity rotation)" $
        zoomQuadWorldUVs FaceSouth 0 0
            `shouldBe` ( packUV (-16) 0
                       , packUV 16 0
                       , packUV 16 32
                       , packUV (-16) 32
                       )

    it "FaceNorth (180 deg): the same four values, rotated by 2 (opposite corner)" $
        zoomQuadWorldUVs FaceNorth 0 0
            `shouldBe` ( packUV 16 32
                       , packUV (-16) 32
                       , packUV (-16) 0
                       , packUV 16 0
                       )

    it "FaceWest (90 deg): the same four values, cyclically rotated by 1" $
        zoomQuadWorldUVs FaceWest 0 0
            `shouldBe` ( packUV 16 0
                       , packUV 16 32
                       , packUV (-16) 32
                       , packUV (-16) 0
                       )

    it "FaceEast (270 deg): the same four values, cyclically rotated by 3" $
        zoomQuadWorldUVs FaceEast 0 0
            `shouldBe` ( packUV (-16) 32
                       , packUV (-16) 0
                       , packUV 16 0
                       , packUV 16 32
                       )

    it "every facing uses the SAME set of 4 values, just permuted (no value invented/lost)" $
        let asList (a, b, c, d) = [a, b, c, d]
            south = asList (zoomQuadWorldUVs FaceSouth 5 (-3))
            west  = asList (zoomQuadWorldUVs FaceWest  5 (-3))
            north = asList (zoomQuadWorldUVs FaceNorth 5 (-3))
            east  = asList (zoomQuadWorldUVs FaceEast  5 (-3))
        in mapM_ (\s → sort s `shouldBe` sort south) [west, north, east]
