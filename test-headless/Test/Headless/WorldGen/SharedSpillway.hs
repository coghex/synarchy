{-# LANGUAGE Strict #-}

-- | Directly-constructed topology for #2323: a spillway tile that two
--   adjacent basins both select.
--
--   The pre-fix pipeline stored the per-tile inverse of the spillway
--   table as a scalar and let the last (highest-id) writer win, so the
--   descent at a shared tile excluded only one contributor while flow
--   accumulation still injected both. The discarded contributor's
--   outflow was routed straight back into its own basin, and the
--   resulting river's scalar source lake was whichever spillway
--   row-major traversal happened to visit last.
--
--   Every fixture here builds its own lakes and terrain and drives the
--   REAL pipeline stage ('resolveSpillways', the same call
--   'World.Fluid.River.Identify.identifyWorldRivers' makes) rather
--   than a re-implementation of its wiring, so a regression in either
--   the ownership relation or the order it is applied in is visible.
module Test.Headless.WorldGen.SharedSpillway (spec) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Hspec
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Lake.Types
    (Lake(..), LakeChunkEntry(..), WorldLakes(..), emptyWorldLakes)
import World.Fluid.River.Identify.Common
    ( SpillwayOwners, dirEast, dirNone, dirSouth, dirWest, stepDir
    , spillwayOwnersAt )
import World.Fluid.River.Identify.Components
    (buildRivers, labelRiverComponents)
import World.Fluid.River.Identify.Flow
    ( bucketSortAscending, buildLakeIdAt, buildSpillwayOwners
    , computeFlowAccumulation, computeSpillways, resolveSpillways )
import World.Fluid.River.Types (River(..))

-- * Fixture grid
--
-- worldSize 2 → a 32×32 tile grid whose lower-left quadrant is chunk
-- (-1, -1). Every fixture coordinate below stays inside
-- @0 ≤ x, y < 16@, so that one chunk carries every lake bitmask and
-- grid-offset coordinates equal chunk-local ones.

worldSizeT ∷ Int
worldSizeT = 2

worldTilesT ∷ Int
worldTilesT = worldSizeT * chunkSize

halfT ∷ Int
halfT = worldTilesT `div` 2

nTilesT ∷ Int
nTilesT = worldTilesT * worldTilesT

-- | Grid-offset @(x, y)@ → flat tile index.
at ∷ Int → Int → Int
at x y = y * worldTilesT + x

-- | Base elevation for every tile a fixture does not name. High
--   enough that no unnamed tile is a descent candidate, and well above
--   'seaLevel' so it is never treated as ocean.
baseZ ∷ Int
baseZ = 100

mkTerrain ∷ [((Int, Int), Int)] → VU.Vector Int
mkTerrain overrides =
    VU.replicate nTilesT baseZ VU.// [ (at x y, z) | ((x, y), z) ← overrides ]

mkUnits ∷ [((Int, Int), Int)] → VU.Vector Int
mkUnits overrides =
    VU.replicate nTilesT 0 VU.// [ (at x y, n) | ((x, y), n) ← overrides ]

-- | Build 'WorldLakes' from per-lake grid-offset tile lists. List
--   position is the 'LakeId'.
mkLakes ∷ [[(Int, Int)]] → WorldLakes
mkLakes []        = emptyWorldLakes
mkLakes lakeTiles = WorldLakes
    { wlLakes      = V.fromList (map mkLake lakeTiles)
    , wlByChunk    = HM.singleton (ChunkCoord (-1) (-1))
                                  (V.fromList (zipWith mkEntry [0 ..] lakeTiles))
    , wlCarveDelta = HM.empty
    }
  where
    mkLake ts = Lake
        { lkSurface  = 0
        , lkFloor    = 0
        , lkArea     = length ts
        , lkBBoxMinX = minimum [ x - halfT | (x, _) ← ts ]
        , lkBBoxMinY = minimum [ y - halfT | (_, y) ← ts ]
        , lkBBoxMaxX = maximum [ x - halfT | (x, _) ← ts ]
        , lkBBoxMaxY = maximum [ y - halfT | (_, y) ← ts ]
        }
    mkEntry lid ts = LakeChunkEntry
        { lceLakeId  = lid
        , lceBitmask = VU.generate (chunkSize * chunkSize) $ \li →
              (li `mod` chunkSize, li `div` chunkSize) `elem` ts
        }

-- * Driving the real pipeline stage

-- | Everything one fixture produces. 'fxSpillPre' / 'fxOwnersPre' are
--   the tables BEFORE the shared-spillway demotion; 'fxSpill' /
--   'fxOwners' are the effective ones the rest of the pipeline sees.
data Fixture = Fixture
    { fxTerrain   ∷ VU.Vector Int
    , fxLakeIdAt  ∷ VU.Vector Int
    , fxSpillPre  ∷ VU.Vector Int
    , fxOwnersPre ∷ SpillwayOwners
    , fxSpill     ∷ VU.Vector Int
    , fxOwners    ∷ SpillwayOwners
    , fxDir       ∷ VU.Vector Word8
    , fxFlow      ∷ VU.Vector Int
    , fxLakeFlow  ∷ VU.Vector Int
    }

runFixture
    ∷ [[(Int, Int)]]       -- ^ lakes, by 'LakeId'
    → [((Int, Int), Int)]  -- ^ terrain overrides
    → [((Int, Int), Int)]  -- ^ precip-unit overrides
    → Fixture
runFixture lakeTiles terrainOverrides precipOverrides =
    let lakes    = mkLakes lakeTiles
        terrain  = mkTerrain terrainOverrides
        precip   = mkUnits precipOverrides
        evap     = VU.replicate nTilesT 0
        lakeIdAt = buildLakeIdAt worldSizeT lakes
        -- The pre-demotion view, for assertions about which basins
        -- were identified as contributors in the first place.
        spillPre  = computeSpillways worldSizeT lakes terrain lakeIdAt
        ownersPre = buildSpillwayOwners nTilesT spillPre
        -- The production stage, verbatim.
        (spill, owners, dir) =
            resolveSpillways worldSizeT lakes terrain lakeIdAt
        ascOrder = bucketSortAscending terrain
        (flow, lakeFlow) = computeFlowAccumulation
            worldTilesT terrain lakeIdAt dir spill precip evap ascOrder
    in Fixture
        { fxTerrain   = terrain
        , fxLakeIdAt  = lakeIdAt
        , fxSpillPre  = spillPre
        , fxOwnersPre = ownersPre
        , fxSpill     = spill
        , fxOwners    = owners
        , fxDir       = dir
        , fxFlow      = flow
        , fxLakeFlow  = lakeFlow
        }

-- | Component labelling + 'buildRivers' over the fixture's own flow
--   field, mirroring 'World.Fluid.River.Identify.traceRivers' step 4.
riversOf ∷ Fixture → Int → V.Vector River
riversOf fx threshold = riversOver fx (riverTilesOf fx threshold)

-- | The same, over an explicitly chosen river-tile mask. Used where a
--   fixture's own flow is (correctly) too low to raise a river but the
--   metadata a spillway tile WOULD contribute still has to be pinned.
riversOver ∷ Fixture → VU.Vector Bool → V.Vector River
riversOver fx isRiverTile =
    let (compId, nComps) = labelRiverComponents worldTilesT isRiverTile
    in buildRivers worldSizeT (fxTerrain fx) (fxLakeIdAt fx) (fxOwners fx)
                   (fxSpill fx) (fxDir fx) (fxFlow fx) isRiverTile
                   compId nComps

riverTilesOf ∷ Fixture → Int → VU.Vector Bool
riverTilesOf fx threshold = VU.generate nTilesT $ \i →
    let t = fxTerrain fx VU.! i
    in t ≢ minBound ∧ t > seaLevel
       ∧ fxLakeIdAt fx VU.! i < 0
       ∧ fxFlow fx VU.! i ≥ threshold

ownersAt ∷ SpillwayOwners → Int → [Int]
ownersAt owners idx = VU.toList (spillwayOwnersAt owners idx)

-- | Where the tile's descent direction actually lands, and which lake
--   (if any) owns that neighbour.
outletOf ∷ Fixture → Int → (Maybe Int, Maybe Int)
outletOf fx idx = case stepDir worldTilesT idx (fxDir fx VU.! idx) of
    Nothing → (Nothing, Nothing)
    Just dn → (Just dn, let l = fxLakeIdAt fx VU.! dn
                        in if l ≥ 0 then Just l else Nothing)

spec ∷ Spec
spec = do

    -- ------------------------------------------------------------------
    describe "a shared outlet with a valid descent" $ do
        -- Lakes 0 and 1 sit either side of one 50z saddle; the only
        -- descent that is not into a contributor runs south.
        let sw  = at 4 8
            fx  = runFixture
                    [ [(3, 8)], [(5, 8)] ]
                    [ ((3, 8), 10), ((5, 8), 10), ((4, 8), 50)
                    , ((4, 9), 40), ((4, 10), 30), ((4, 11), 20)
                    , ((4, 12), 10) ]
                    [ ((3, 8), 100), ((5, 8), 200) ]

        it "records both basins as contributors of the shared tile" $
            ownersAt (fxOwnersPre fx) sw `shouldBe` [0, 1]

        it "discards neither contributor" $ do
            VU.toList (fxSpill fx) `shouldBe` [sw, sw]
            ownersAt (fxOwners fx) sw `shouldBe` [0, 1]

        it "chooses a descent that enters neither contributing basin" $ do
            fxDir fx VU.! sw `shouldBe` dirSouth
            outletOf fx sw `shouldBe` (Just (at 4 9), Nothing)

        it "returns neither contributor's injection to its own lake" $
            VU.toList (fxLakeFlow fx) `shouldBe` [100, 200]

        it "carries both injections into the shared downstream chain" $ do
            fxFlow fx VU.! sw       `shouldBe` 300
            fxFlow fx VU.! at 4 9   `shouldBe` 300
            fxFlow fx VU.! at 4 12  `shouldBe` 300

        it "leaves the resulting river without a single source lake" $ do
            let rs = riversOf fx 100
            V.length rs `shouldBe` 1
            rivSourceLake (rs V.! 0) `shouldBe` Nothing

    -- ------------------------------------------------------------------
    describe "a shared outlet the contributor exclusion strands" $ do
        -- Same saddle, but nothing below it except the two basins, so
        -- excluding both removes the last descent candidate.
        let sw = at 4 8
            fx = runFixture
                    [ [(3, 8)], [(5, 8)] ]
                    [ ((3, 8), 10), ((5, 8), 10), ((4, 8), 50) ]
                    [ ((3, 8), 100), ((5, 8), 200) ]

        it "still identifies both basins as contributors" $
            ownersAt (fxOwnersPre fx) sw `shouldBe` [0, 1]

        it "demotes every contributor to no spillway at all" $ do
            VU.toList (fxSpillPre fx) `shouldBe` [sw, sw]
            VU.toList (fxSpill fx)    `shouldBe` [-1, -1]
            ownersAt (fxOwners fx) sw `shouldBe` []

        it "keeps the tile's own exclusion-derived dirNone" $
            fxDir fx VU.! sw `shouldBe` dirNone

        it "adds no contributor's accumulated flow at the tile" $
            fxFlow fx VU.! sw `shouldBe` 0

        it "starts no injection walk and absorbs nothing back" $
            VU.toList (fxLakeFlow fx) `shouldBe` [100, 200]

        it "contributes no source-lake metadata to a river there" $ do
            let forced = VU.generate nTilesT (\i → i ≡ sw ∨ i ≡ at 4 9)
                rs     = riversOver fx forced
            V.length rs `shouldBe` 1
            rivSourceLake (rs V.! 0) `shouldBe` Nothing

    -- ------------------------------------------------------------------
    describe "a shared outlet with no descent for an unrelated reason" $ do
        -- The saddle is itself a third lake's tile, so it has no
        -- descent whether or not contributors are excluded. Nothing is
        -- being routed back into itself, so nothing is demoted.
        let sw = at 4 8
            fx = runFixture
                    [ [(3, 8)], [(5, 8)], [(4, 8)] ]
                    [ ((3, 8), 10), ((5, 8), 10), ((4, 8), 50)
                    , ((4, 7), -5) ]
                    [ ((3, 8), 100), ((5, 8), 200) ]

        it "leaves both contributors holding the shared spillway" $ do
            VU.toList (fxSpill fx)    `shouldBe` [sw, sw, -1]
            ownersAt (fxOwners fx) sw `shouldBe` [0, 1]

        it "keeps the tile's dirNone" $
            fxDir fx VU.! sw `shouldBe` dirNone

        it "still injects both contributors at the tile" $ do
            fxFlow fx VU.! sw `shouldBe` 300
            VU.toList (fxLakeFlow fx) `shouldBe` [100, 200, 0]

    -- ------------------------------------------------------------------
    describe "a unique-owner outlet" $ do
        -- Lake 0 spills south through a three-tile reach into lake 1.
        let sw = at 4 8
            fx = runFixture
                    [ [(3, 8)], [(4, 11)] ]
                    [ ((3, 8), 10), ((4, 8), 50), ((4, 9), 40)
                    , ((4, 10), 30), ((4, 11), 20), ((4, 12), -5) ]
                    [ ((3, 8), 100) ]

        it "keeps its spillway tile and its sole contributor" $ do
            fxSpill fx VU.! 0         `shouldBe` sw
            ownersAt (fxOwners fx) sw `shouldBe` [0]

        it "keeps its descent direction" $ do
            fxDir fx VU.! sw `shouldBe` dirSouth
            outletOf fx sw   `shouldBe` (Just (at 4 9), Nothing)

        it "injects its accumulated flow exactly as before" $ do
            fxFlow fx VU.! sw      `shouldBe` 100
            fxFlow fx VU.! at 4 9  `shouldBe` 100
            fxFlow fx VU.! at 4 10 `shouldBe` 100

        it "is still absorbed by the downstream, non-contributing lake" $ do
            fxLakeFlow fx VU.! 1   `shouldBe` 100
            fxFlow fx VU.! at 4 11 `shouldBe` 0

        it "keeps its river's single source lake" $ do
            let rs = riversOf fx 100
            V.length rs `shouldBe` 1
            rivSourceLake (rs V.! 0) `shouldBe` Just 0

    -- ------------------------------------------------------------------
    describe "a unique-owner outlet with no valid descent" $ do
        -- One contributor, so the shared-outlet demotion must not
        -- reach it: master injects here and the walk simply ends.
        let sw = at 4 8
            fx = runFixture
                    [ [(3, 8)] ]
                    [ ((3, 8), 10), ((4, 8), 50) ]
                    [ ((3, 8), 100) ]

        it "is not demoted" $ do
            VU.toList (fxSpill fx)    `shouldBe` [sw]
            ownersAt (fxOwners fx) sw `shouldBe` [0]

        it "has no descent" $
            fxDir fx VU.! sw `shouldBe` dirNone

        it "still injects, and the walk terminates at the tile" $ do
            fxFlow fx VU.! sw `shouldBe` 100
            VU.toList (fxLakeFlow fx) `shouldBe` [100]

    -- ------------------------------------------------------------------
    describe "two non-shared outlets feeding one component" $ do
        -- Separate spillways, one component. The union is taken over
        -- the whole component, so no traversal order can elect one of
        -- them as the river's source.
        let swW = at 3 8
            swE = at 5 8
            layout =
                [ ((2, 8), 10), ((3, 8), 50), ((4, 8), 40)
                , ((6, 8), 10), ((5, 8), 50)
                , ((4, 9), 30), ((4, 10), 20), ((4, 11), 10) ]

            -- Lake 0 west, lake 1 east.
            fxA = runFixture [ [(2, 8)], [(6, 8)] ] layout
                             [ ((2, 8), 100), ((6, 8), 200) ]
            -- The same topology with the two lake ids exchanged, so a
            -- row-major "last writer" and a "first writer" would each
            -- report a DIFFERENT single source across the two runs.
            fxB = runFixture [ [(6, 8)], [(2, 8)] ] layout
                             [ ((6, 8), 200), ((2, 8), 100) ]

        it "keeps each outlet's own contributor" $ do
            ownersAt (fxOwners fxA) swW `shouldBe` [0]
            ownersAt (fxOwners fxA) swE `shouldBe` [1]

        it "joins both reaches into one component" $ do
            let rs = riversOf fxA 100
            V.length rs `shouldBe` 1
            fxFlow fxA VU.! at 4 8 `shouldBe` 300

        it "records no single source lake, whichever id sits west" $ do
            let rsA = riversOf fxA 100
                rsB = riversOf fxB 100
            V.length rsA `shouldBe` 1
            V.length rsB `shouldBe` 1
            rivSourceLake (rsA V.! 0) `shouldBe` Nothing
            rivSourceLake (rsB V.! 0) `shouldBe` Nothing

    -- ------------------------------------------------------------------
    describe "a precipitation-fed component" $ do
        let fx = runFixture []
                    [ ((4, 4), 60), ((4, 5), 50), ((4, 6), 40), ((4, 7), 30) ]
                    [ ((4, 4), 150) ]

        it "has no contributors anywhere along it" $ do
            ownersAt (fxOwners fx) (at 4 4) `shouldBe` []
            ownersAt (fxOwners fx) (at 4 7) `shouldBe` []

        it "records no source lake" $ do
            let rs = riversOf fx 100
            V.length rs `shouldBe` 1
            rivSourceLake (rs V.! 0) `shouldBe` Nothing

    -- ------------------------------------------------------------------
    describe "the ownership relation itself" $ do
        it "orders a shared tile's contributors ascending by lake id" $ do
            -- Built from a table whose highest id is written first, so
            -- an append-in-encounter-order bug would show up here.
            let owners = buildSpillwayOwners 8 (VU.fromList [5, 3, 5, 3, 5])
            VU.toList (spillwayOwnersAt owners 5) `shouldBe` [0, 2, 4]
            VU.toList (spillwayOwnersAt owners 3) `shouldBe` [1, 3]
            VU.toList (spillwayOwnersAt owners 0) `shouldBe` []

        it "drops lakes with no usable spillway" $ do
            let owners = buildSpillwayOwners 4 (VU.fromList [-1, 2, -1])
            VU.toList (spillwayOwnersAt owners 2) `shouldBe` [1]
            VU.sum (VU.map (const (1 ∷ Int)) (spillwayOwnersAt owners 0))
                `shouldBe` 0

    -- ------------------------------------------------------------------
    describe "direction codes used by these fixtures" $
        it "are the pipeline's own" $ do
            -- Guards the fixtures above against a silent renumbering
            -- of the D4 codes they assert on.
            [dirEast, dirSouth, dirWest] `shouldBe` [1, 2, 3]
            dirNone `shouldBe` 4
