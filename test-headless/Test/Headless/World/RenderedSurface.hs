{-# LANGUAGE Strict #-}
-- | Pure tests for the shared rendered-surface rule (#1112).
--
--   The rule — River renders FLAT at its fluid surface (hiding terrain
--   protruding through the carved channel), every other fluid renders
--   at @max terrain fluid@, dry renders at terrain — used to be
--   hand-written at four sites and MISSING at two more. The dig path
--   (@WeDeleteTile@) and the carve path (@WeSetCell@ →
--   @recomputeColumnSurface@) applied a bare @max@, so digging a River
--   tile whose terrain protruded above the water rendered the
--   protrusion while generation and the sim writeback rendered it flat.
--
--   These are pure @LoadedChunk@ tests: no engine, no world thread.
module Test.Headless.World.RenderedSurface (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Structure.Types (emptyChunkStructures)
import World.Chunk.Types (LoadedChunk(..), ColumnTiles(..), chunkSize, columnIndex)
import World.Edit.Apply (applyEdit, replayEdits)
import World.Edit.Types (WorldEdit(..))
import World.Flora.Types (FloraChunkData(..))
import World.Fluid.Types (FluidCell(..), FluidType(..), emptyIceMap, renderedSurfaceZ)
import World.Material.Id (MaterialId(..))
import World.Types (ChunkCoord(..))

-- Fixture ---------------------------------------------------------

stone ∷ MaterialId
stone = MaterialId 1

-- | A column of solid stone from z 0 up to and including @topZ@.
--   @topZ < 0@ makes an entirely empty column.
solidColumn ∷ Int → ColumnTiles
solidColumn topZ = ColumnTiles
    { ctStartZ = 0
    , ctMats   = VU.replicate (topZ + 1) 1
    , ctSlopes = VU.replicate (topZ + 1) 0
    , ctVeg    = VU.replicate (topZ + 1) 0
    }

-- | A chunk at (0,0) whose columns are described by
--   @(terrain top z, fluid cell)@ for the first few local columns along
--   y=0; every other column is flat dry stone at z 0.
--
--   The rendered-surface map is seeded with the shared rule, exactly as
--   generation would produce it — so any divergence a test observes was
--   introduced by the edit under test, not by the fixture.
mkChunk ∷ [(Int, Maybe FluidCell)] → LoadedChunk
mkChunk spec' = LoadedChunk
    { lcCoord             = ChunkCoord 0 0
    , lcTiles             = V.generate area (solidColumn . terrainAt)
    , lcSurfaceMap        = VU.generate area
                                (\i → renderedSurfaceZ (terrainAt i) (fluidAt i))
    , lcTerrainSurfaceMap = VU.generate area terrainAt
    , lcFluidMap          = V.generate area fluidAt
    , lcIceMap            = emptyIceMap
    , lcFlora             = FloraChunkData []
    , lcSideDeco          = VU.replicate area 0
      -- Far below every fixture column, so digging never reveals
      -- groundwater and muddies a River assertion.
    , lcWaterTableMap     = VU.replicate area (-100)
    , lcMagma             = Nothing
    , lcStructures        = emptyChunkStructures
    }
  where
    area = chunkSize * chunkSize
    terrainAt i = maybe 0 fst (lookup i indexed)
    fluidAt   i = maybe Nothing snd (lookup i indexed)
    indexed = zip [0 ..] spec'

-- | A River column whose terrain protrudes TWO levels above the water.
--   Two, not one: after a single dig a one-level protrusion collapses
--   to terrain == fcSurface, where the old buggy @max@ coincidentally
--   agrees with the rule and the regression would pass unfixed.
riverSurfZ, riverTerrainZ ∷ Int
riverSurfZ    = 10
riverTerrainZ = 12

riverChunk ∷ LoadedChunk
riverChunk = mkChunk [(riverTerrainZ, Just (FluidCell River riverSurfZ))]

surfaceAt, terrainSurfaceAt ∷ LoadedChunk → Int → Int → Int
surfaceAt        lc lx ly = lcSurfaceMap        lc VU.! columnIndex lx ly
terrainSurfaceAt lc lx ly = lcTerrainSurfaceMap lc VU.! columnIndex lx ly

spec ∷ Spec
spec = do
    describe "renderedSurfaceZ truth table" $ do
        it "a dry column renders at its terrain top" $
            renderedSurfaceZ 7 Nothing `shouldBe` 7

        it "River renders flat at the fluid surface, hiding a protrusion" $
            renderedSurfaceZ 12 (Just (FluidCell River 10)) `shouldBe` 10

        it "River renders flat at the fluid surface below terrain too" $
            renderedSurfaceZ 3 (Just (FluidCell River 10)) `shouldBe` 10

        it "every other fluid type renders at max(terrain, fluid)" $
            [ renderedSurfaceZ 12 (Just (FluidCell ft 10)) | ft ← [Ocean, Lake, Lava] ]
              `shouldBe` [12, 12, 12]

        it "every other fluid type still floods above terrain" $
            [ renderedSurfaceZ 3 (Just (FluidCell ft 10)) | ft ← [Ocean, Lake, Lava] ]
              `shouldBe` [10, 10, 10]

    describe "digging a River tile whose terrain protrudes (#1112)" $ do
        let dug = applyEdit (WeDeleteTile 0 0) riverChunk

        it "leaves the terrain top still above the river surface" $
            -- Guards the fixture itself: if this collapses to equality
            -- the rendered-surface assertion below stops discriminating.
            terrainSurfaceAt dug 0 0 `shouldSatisfy` (> riverSurfZ)

        it "advances the terrain top by exactly one level" $
            terrainSurfaceAt dug 0 0 `shouldBe` riverTerrainZ - 1

        it "renders flat at the river surface, not at the protrusion" $
            surfaceAt dug 0 0 `shouldBe` riverSurfZ

        it "keeps the river fluid cell (digging never displaces fluid)" $
            (lcFluidMap dug V.! columnIndex 0 0)
              `shouldBe` Just (FluidCell River riverSurfZ)

    describe "the dug flat surface survives an eviction/reload replay" $ do
        -- Chunk eviction and the load path both regenerate the chunk
        -- and replay its edit log onto it. That replay is the route
        -- that has no trailing WeSetFluidSnapshot to paper over a
        -- divergent surface, so it is the one that must agree.
        let edits   = HM.singleton (ChunkCoord 0 0) [WeDeleteTile 0 0]
            replayed = replayEdits edits riverChunk
            live     = applyEdit (WeDeleteTile 0 0) riverChunk

        it "replays to the river surface, not the protrusion" $
            surfaceAt replayed 0 0 `shouldBe` riverSurfZ

        it "replays identically to the live edit" $
            replayed `shouldBe` live

        it "stays flat after a second dig" $ do
            -- A three-level protrusion, so two digs still leave terrain
            -- ABOVE the water: from the two-level fixture the second dig
            -- lands exactly on fcSurface, where a bare max agrees with
            -- the rule and the assertion stops discriminating.
            let tall  = mkChunk [(riverSurfZ + 3, Just (FluidCell River riverSurfZ))]
                twice = replayEdits
                    (HM.singleton (ChunkCoord 0 0)
                                  [WeDeleteTile 0 0, WeDeleteTile 0 0])
                    tall
            terrainSurfaceAt twice 0 0 `shouldBe` riverSurfZ + 1
            surfaceAt twice 0 0 `shouldBe` riverSurfZ

    describe "carving terrain up past a River surface (WeSetCell)" $ do
        -- recomputeColumnSurface is only reached via WeSetCell; it had
        -- no River case at all.
        let carved = applyEdit (WeSetCell 0 0 (riverSurfZ + 4) stone)
                               (mkChunk [(3, Just (FluidCell River riverSurfZ))])

        it "puts the terrain surface at the new terrain top" $
            terrainSurfaceAt carved 0 0 `shouldBe` riverSurfZ + 4

        it "leaves the rendered surface flat at the river surface" $
            surfaceAt carved 0 0 `shouldBe` riverSurfZ

        it "still renders non-River fluid at max(terrain, fluid)" $ do
            let lakeCarved = applyEdit (WeSetCell 0 0 (riverSurfZ + 4) stone)
                                 (mkChunk [(3, Just (FluidCell Lake riverSurfZ))])
            surfaceAt lakeCarved 0 0 `shouldBe` riverSurfZ + 4

    describe "WeSetFluidTile measures against terrain, not the old surface" $ do
        -- The column carries a stale rendered surface from a deep fluid
        -- cell this edit REPLACES; folding that height back in would
        -- keep the superseded cell's surface alive.
        let deep    = mkChunk [(5, Just (FluidCell Ocean 20))]
            refilled = applyEdit (WeSetFluidTile 0 0 Lake) deep

        it "seeds the stale surface the edit must not reuse" $
            surfaceAt deep 0 0 `shouldBe` 20

        it "renders the replacement cell one level above terrain" $
            surfaceAt refilled 0 0 `shouldBe` 6

        it "renders a River replacement flat at its own surface" $
            surfaceAt (applyEdit (WeSetFluidTile 0 0 River) deep) 0 0
              `shouldBe` 6

    describe "WeAddTile keeps its displacement guard" $ do
        -- Filling BELOW the fluid surface leaves the cell in place; the
        -- guard means surviving fluid always has newTopZ < fcSurface,
        -- so River and non-River agree here.
        let filled = applyEdit (WeAddTile 0 0 stone)
                               (mkChunk [(5, Just (FluidCell River riverSurfZ))])

        it "raises the terrain top under the water" $
            terrainSurfaceAt filled 0 0 `shouldBe` 6

        it "still renders at the river surface" $
            surfaceAt filled 0 0 `shouldBe` riverSurfZ

        it "displaces the fluid entirely once the fill reaches it" $ do
            let buried = applyEdit (WeAddTile 0 0 stone)
                             (mkChunk [(riverSurfZ - 1,
                                        Just (FluidCell River riverSurfZ))])
            lcFluidMap buried V.! columnIndex 0 0 `shouldBe` Nothing
            surfaceAt buried 0 0 `shouldBe` riverSurfZ
