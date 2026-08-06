{-# LANGUAGE Strict #-}
-- | Pure tests for the dry "island column" smoother (#1131).
--
--   'smoothIslandColumns' turns a dry tile whose terrain pokes a few z
--   above a surrounding lake into water at that lake's surface,
--   dropping its terrain to @surface − 1@. Before #1131 the function
--   was only reachable through a full world generation, so its
--   qualifying rule — one surface shared by at least THREE of the
--   at-most-four cardinal neighbors — had no direct coverage, and the
--   frequency count it rests on was hand-rolled.
--
--   These are pure vector-in/vector-out tests: no engine, no world
--   thread, no chunk.
module Test.Headless.World.IslandColumns (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (chunkSize)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Generate.Chunk.Fluid (maxColumnPeek, smoothIslandColumns)

-- Fixture ---------------------------------------------------------

area ∷ Int
area = chunkSize * chunkSize

-- | Chunk-local (x, y) → the flat index the smoother indexes by.
li ∷ (Int, Int) → Int
li (lx, ly) = ly * chunkSize + lx

-- | The surface every fixture lake sits at. Well above the background
--   terrain, so no tile the fixture does not name can ever satisfy the
--   @terrain > surface@ half of the rule.
lakeSurf ∷ Int
lakeSurf = 10

-- | A chunk-sized (terrain, fluid) pair. Every tile is dry bedrock at
--   z 0 except the named terrain heights and the named Lake cells.
mkMaps
    ∷ [((Int, Int), Int)]   -- ^ terrain overrides
    → [((Int, Int), Int)]   -- ^ Lake cells, by surface z
    → (VU.Vector Int, V.Vector (Maybe FluidCell))
mkMaps terrs lakes =
    ( VU.generate area (\i → fromMaybe 0 (lookup i terrIdx))
    , V.generate area (\i → FluidCell Lake ⊚ lookup i lakeIdx) )
  where
    terrIdx = [ (li c, z) | (c, z) ← terrs ]
    lakeIdx = [ (li c, z) | (c, z) ← lakes ]

terrainAt ∷ VU.Vector Int → (Int, Int) → Int
terrainAt v c = v VU.! li c

fluidAt ∷ V.Vector (Maybe FluidCell) → (Int, Int) → Maybe FluidCell
fluidAt v c = v V.! li c

-- | The tile under test, and its four cardinal neighbors in the order
--   the smoother samples them (west, east, north, south).
target, west, east, north, south ∷ (Int, Int)
target = (5, 5)
west   = (4, 5)
east   = (6, 5)
north  = (5, 4)
south  = (5, 6)

-- | Run the smoother over a fixture whose only interesting tile is
--   'target', at the given terrain height and ringed by the given Lake
--   neighbors.
smoothTarget
    ∷ Int                   -- ^ target terrain z
    → [((Int, Int), Int)]   -- ^ Lake neighbors, by surface z
    → (VU.Vector Int, V.Vector (Maybe FluidCell))
smoothTarget targetZ lakes =
    uncurry smoothIslandColumns (mkMaps [(target, targetZ)] lakes)

spec ∷ Spec
spec = do
    describe "a dry column ringed by three matching Lake neighbors" $ do
        let (terr, fluid) = smoothTarget (lakeSurf + 2)
                [(west, lakeSurf), (north, lakeSurf), (south, lakeSurf)]

        it "renders as Lake at the neighbors' surface" $
            fluidAt fluid target `shouldBe` Just (FluidCell Lake lakeSurf)

        it "drops its terrain to one below that surface" $
            terrainAt terr target `shouldBe` lakeSurf - 1

    describe "columns the three-neighbor rule must leave alone" $ do
        let unchanged label maps = describe label $ do
                let (terr, fluid) = maps
                it "stays dry" $
                    fluidAt fluid target `shouldBe` Nothing
                it "keeps its terrain" $
                    terrainAt terr target `shouldBe` lakeSurf + 2

        unchanged "only two Lake neighbors" $
            smoothTarget (lakeSurf + 2) [(west, lakeSurf), (north, lakeSurf)]

        -- Two surfaces at two neighbors each: the most frequent value
        -- is not enough, three of the same value is the rule. Both
        -- candidate surfaces would otherwise fire, since the target sits
        -- inside the peek window of each.
        unchanged "four Lake neighbors split two/two across surfaces" $
            smoothTarget (lakeSurf + 2)
                [ (west, lakeSurf),      (east,  lakeSurf)
                , (north, lakeSurf - 3), (south, lakeSurf - 3) ]

    describe "the qualifying surface need not be adjacent in sample order" $ do
        -- Sampled west, east, north, south — so this reads 10, 7, 10, 10.
        -- Counting equal-and-adjacent runs alone would see a longest run
        -- of two and leave the tile dry.
        let (terr, fluid) = smoothTarget (lakeSurf + 2)
                [ (west, lakeSurf), (east, lakeSurf - 3)
                , (north, lakeSurf), (south, lakeSurf) ]

        it "still finds the surface shared by three neighbors" $
            fluidAt fluid target `shouldBe` Just (FluidCell Lake lakeSurf)

        it "drops the terrain against that surface" $
            terrainAt terr target `shouldBe` lakeSurf - 1

    describe "the peek window bounds which terrain heights qualify" $ do
        let ring = [ (west, lakeSurf), (north, lakeSurf), (south, lakeSurf) ]

        it "smooths a column exactly maxColumnPeek above the surface" $ do
            let (terr, fluid) = smoothTarget (lakeSurf + maxColumnPeek) ring
            fluidAt fluid target `shouldBe` Just (FluidCell Lake lakeSurf)
            terrainAt terr target `shouldBe` lakeSurf - 1

        it "leaves a column one z above the window" $ do
            let (terr, fluid) = smoothTarget (lakeSurf + maxColumnPeek + 1) ring
            fluidAt fluid target `shouldBe` Nothing
            terrainAt terr target `shouldBe` lakeSurf + maxColumnPeek + 1

        it "leaves a column level with the surface" $ do
            let (terr, fluid) = smoothTarget lakeSurf ring
            fluidAt fluid target `shouldBe` Nothing
            terrainAt terr target `shouldBe` lakeSurf

    describe "a smoothed column becomes a neighbor for the next pass" $ do
        -- 'target' qualifies on pass 1; 'west' only reaches three
        -- matching neighbors once 'target' has become Lake, and it is
        -- visited BEFORE 'target' in index order, so it can only be
        -- caught on pass 2. That second visit re-reads terrain for
        -- tiles the first pass already wrote, which is what pins the
        -- write-once invariant below.
        let (terr, fluid) = uncurry smoothIslandColumns $ mkMaps
                [ (target, lakeSurf + 2), (west, lakeSurf + 2) ]
                [ (east, lakeSurf), (north, lakeSurf), (south, lakeSurf)
                , ((4, 4), lakeSurf), ((4, 6), lakeSurf) ]

        it "smooths the first-pass column" $ do
            fluidAt fluid target `shouldBe` Just (FluidCell Lake lakeSurf)
            terrainAt terr target `shouldBe` lakeSurf - 1

        it "smooths the column that only qualified afterwards" $ do
            fluidAt fluid west `shouldBe` Just (FluidCell Lake lakeSurf)
            terrainAt terr west `shouldBe` lakeSurf - 1

        it "never re-lowers a column it already smoothed" $
            -- A second write would land at (lakeSurf - 1) - 1.
            terrainAt terr target `shouldSatisfy` (≡ lakeSurf - 1)

        it "leaves the terrain under pre-existing Lake cells untouched" $
            -- Every seeded neighbor starts wet, so the loop skips it and
            -- its background terrain must survive at z 0.
            map (terrainAt terr) [east, north, south] `shouldBe` [0, 0, 0]
