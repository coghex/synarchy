{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for 'World.Render.SideDecoQuads.waterSideFaceQuads' — the
--   water side-face (waterfall / water-cliff) generator.
--
--   The regression under test (issue #26): side faces used to be hard
--   filtered to in-chunk neighbors, so a water drop landing right on a
--   chunk seam produced no side face. The fix threads the same
--   cross-chunk neighbor lookup that @waterSlopeAt@ already uses, so a
--   neighbor sitting in the adjacent chunk is resolved instead of dropped.
--
--   No engine needed: @waterSideFaceQuads@ is pure. We hand-build a 16×16
--   home chunk with one water tile on the east edge and feed neighbor
--   chunks through the lookup callbacks. Slot lookups are stubbed
--   (non-zero face-map slot so quads are emitted) and the view bounds
--   accept every tile, so the emitted-quad COUNT is exactly the number of
--   z-levels in the drop.
module Test.Headless.World.Render.SideFace (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Scene.Types (SortableQuad)
import World.Chunk.Types (ChunkCoord(..), chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Render.SideDecoQuads (waterSideFaceQuads)
import World.Render.Textures.Types (defaultWorldTextures)
import World.Render.ViewBounds (ViewBounds(..))

-- | One chunk's fluid map: all-empty, with the listed cells set.
fluidMapWith ∷ [((Int, Int), FluidCell)] → V.Vector (Maybe FluidCell)
fluidMapWith cells =
    V.replicate (chunkSize * chunkSize) Nothing
      V.// [ (columnIndex x y, Just fc) | ((x, y), fc) ← cells ]

-- | One chunk's terrain-surface map: a flat base z with overrides.
terrMapWith ∷ Int → [((Int, Int), Int)] → VU.Vector Int
terrMapWith base overrides =
    VU.replicate (chunkSize * chunkSize) base
      VU.// [ (columnIndex x y, z) | ((x, y), z) ← overrides ]

-- | View bounds that accept every tile, so visibility never trims a quad.
allVisible ∷ ViewBounds
allVisible = ViewBounds (-1.0e9) 1.0e9 (-1.0e9) 1.0e9

-- | Drive the generator with a fixed camera/world setup. Slot lookup
--   returns 0 (any tile texture) and the face-map slot a non-zero stub
--   (so 'waterSideQuad' doesn't early-out), zSlice 10 / depth 64 so the
--   whole drop is inside the rendered z-window.
run ∷ V.Vector (Maybe FluidCell) → VU.Vector Int
    → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell)))
    → (ChunkCoord → Maybe (VU.Vector Int))
    → [SortableQuad]
run fm tm fluidLookup terrLookup =
    waterSideFaceQuads (\_ → 0) (\_ → 1.0) defaultWorldTextures
        FaceSouth (ChunkCoord 0 0) fm tm fluidLookup terrLookup
        10 64 1.0 0.0 allVisible

spec ∷ Spec
spec = describe "waterSideFaceQuads across chunk seams" $ do

    -- Home chunk (0,0): one Lake tile on the EAST edge (lx = 15) at z=10.
    -- Flat terrain at z=10 everywhere, so the in-chunk (left) neighbor is
    -- level with the water and never draws — every emitted quad therefore
    -- comes from the cross-chunk (right) neighbor.
    let homeFluid = fluidMapWith [((15, 8), FluidCell Lake 10)]
        homeTerr  = terrMapWith 10 []

    it "renders side faces over a DRY drop in the adjacent chunk (the bug)" $ do
        -- Neighbor chunk (1,0): dry, terrain at z=0 → a 10-tall waterfall
        -- face straddling the seam. Before the fix this produced nothing.
        let fluidLookup (ChunkCoord 1 0) = Just (fluidMapWith [])
            fluidLookup _                = Nothing
            terrLookup  (ChunkCoord 1 0) = Just (terrMapWith 0 [])
            terrLookup  _                = Nothing
        -- z = 0..9 → ten side-face quads.
        length (run homeFluid homeTerr fluidLookup terrLookup) `shouldBe` 10

    it "renders side faces over a LOWER-WATER drop in the adjacent chunk" $ do
        -- Neighbor (0,8) holds water at surface 5 (< 10-1), so the stack
        -- bottoms out on that surface: faces from z=5..9 (five quads).
        let fluidLookup (ChunkCoord 1 0) =
                Just (fluidMapWith [((0, 8), FluidCell Lake 5)])
            fluidLookup _                = Nothing
            terrLookup  (ChunkCoord 1 0) = Just (terrMapWith 0 [])
            terrLookup  _                = Nothing
        length (run homeFluid homeTerr fluidLookup terrLookup) `shouldBe` 5

    it "draws nothing at the seam when the neighbor chunk is not loaded" $
        -- Both lookups miss → the drop is unknown, so no side face (the
        -- conservative default, matching waterSlopeAt at an unloaded seam).
        length (run homeFluid homeTerr (const Nothing) (const Nothing))
            `shouldBe` 0

    it "still renders a waterfall face WITHIN a chunk (regression guard)" $ do
        -- Water at interior tile (5,8); the in-chunk right neighbor (6,8)
        -- is a dry 10-tile drop. No cross-chunk lookup is consulted.
        let inFluid = fluidMapWith [((5, 8), FluidCell Lake 10)]
            inTerr  = terrMapWith 10 [((6, 8), 0)]
        length (run inFluid inTerr (const Nothing) (const Nothing))
            `shouldBe` 10
