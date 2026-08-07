{-# LANGUAGE Strict #-}
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
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Scene.Types (SortableQuad(..))
import World.Chunk.Types (ChunkCoord(..), chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Render.ChunkLookup (canonicalChunkLookup)
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
spec = do
  inChunkSpec
  seamSpec

inChunkSpec ∷ Spec
inChunkSpec = describe "waterSideFaceQuads across chunk seams" $ do

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

-- | #1135: 'neighborCell' builds its cross-chunk coord in the HOME
--   chunk's raw frame, but chunks are STORED u-wrapped. Right at the
--   seam those disagree, so the raw @HM.lookup@ missed a LOADED
--   neighbour and the resulting Nothing read as "not loaded" — side
--   faces silently vanished along the whole seam.
--
--   These drive the real production lookup boundary
--   ('World.Render.ChunkLookup.canonicalChunkLookup' — the same helper
--   'renderWorldQuads' builds its two callbacks from) against a map
--   keyed ONLY by the canonical coord. A test-local 'wrapChunkCoordU'
--   would pass even if the production lookup regressed.
seamSpec ∷ Spec
seamSpec = describe "waterSideFaceQuads across the U seam (#1135)" $ do
    -- worldSize 64 → canonical chunk u ∈ [-32, 32). Home chunk (16,-15)
    -- has u = 31, so its raw EAST neighbour (17,-15) has u = 32 — one
    -- past the range, and is stored under ChunkCoord (-15) 17 instead.
    let seamHome   = ChunkCoord 16 (-15)
        seamStored = ChunkCoord (-15) 17
        seamWorld  = 64
        -- One Lake tile on the home chunk's EAST edge at z=10, flat
        -- terrain at 10, so every emitted quad comes from the seam step.
        homeFluid = fluidMapWith [((chunkSize - 1, 8), FluidCell Lake 10)]
        homeTerr  = terrMapWith 10 []
        lookupVia m = canonicalChunkLookup seamWorld
                          (HM.fromList [(seamStored, m)])
        runAt coord fluidLookup terrLookup =
            waterSideFaceQuads (\_ → 0) (\_ → 1.0) defaultWorldTextures
                FaceSouth coord homeFluid homeTerr fluidLookup terrLookup
                10 64 1.0 0.0 allVisible

    it "renders side faces over a DRY drop across the seam" $
        -- Neighbour stored under the wrapped key is dry at z=0 → z=0..9.
        length (runAt seamHome (lookupVia (fluidMapWith []))
                               (lookupVia (terrMapWith 0 [])))
            `shouldBe` 10

    it "renders side faces over a LOWER-WATER drop across the seam" $
        length (runAt seamHome
                    (lookupVia (fluidMapWith [((0, 8), FluidCell Lake 5)]))
                    (lookupVia (terrMapWith 0 [])))
            `shouldBe` 5

    it "matches the equivalent interior fixture exactly" $ do
        -- Same relative geometry one chunk IN from the seam: home chunk
        -- (15,-15) has u = 29, so its raw east neighbour (16,-15) is
        -- itself canonical and the wrap is the identity (requirement 4).
        --
        -- The two fixtures sit at different world positions, so the
        -- absolute sort keys legitimately differ by the tile offset
        -- between them. Compare the per-quad structure instead —
        -- normalised against each fixture's own base key, which is
        -- exactly the z-stack the drop produces (one quad per z, keys
        -- 0.001 apart). Normalising subtracts Floats of different
        -- magnitudes, so compare within a tolerance two orders of
        -- magnitude below that step rather than bit-exactly.
        let interiorVia m = canonicalChunkLookup seamWorld
                                (HM.fromList [(ChunkCoord 16 (-15), m)])
            seamQuads = runAt seamHome (lookupVia (fluidMapWith []))
                                       (lookupVia (terrMapWith 0 []))
            interiorQuads = runAt (ChunkCoord 15 (-15))
                                (interiorVia (fluidMapWith []))
                                (interiorVia (terrMapWith 0 []))
            normalised qs = let ks = map sqSortKey qs
                            in map (subtract (minimum ks)) ks
        length seamQuads `shouldBe` 10
        length seamQuads `shouldBe` length interiorQuads
        zip (normalised seamQuads) (normalised interiorQuads)
            `shouldSatisfy` all (\(a, b) → abs (a - b) < 1.0e-5)

    it "draws nothing when the seam neighbour is genuinely unloaded" $
        -- The negative case the raw lookup could not tell apart from a
        -- loaded-but-aliased neighbour: an empty map means NOT LOADED.
        let emptyVia ∷ ChunkCoord → Maybe a
            emptyVia = canonicalChunkLookup seamWorld HM.empty
        in length (runAt seamHome emptyVia emptyVia) `shouldBe` 0
