{-# LANGUAGE Strict #-}
-- | Contracts for the one per-tile fluid-surface fold in
--   "World.Generate.Chunk.Fluid" (#1111). Both shapes — lake-keyed
--   (one surface per body) and river-keyed (a per-tile surface
--   vector) — plus the absent-sentinel merge that lets
--   'chunkWaterSurfMap' fold lakes and rivers into one accumulator.
--
--   The point of the suite is drift: 'composeFluidMap' and
--   'chunkWaterSurfMap' used to hold two copies of this fold in step
--   by comment, and a disagreement between them puts lava into a
--   water column. So the merge rule is restated here independently
--   of the implementation, and the two consumers are compared against
--   each other rather than against a golden vector.
--
--   Synthetic tables only — no worldgen.
module Test.Headless.WorldGen.FluidSurfaceFold (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Lake.Types
    ( Lake(..), LakeChunkEntry(..), WorldLakes(..) )
import World.Fluid.River.Types
    ( RiverChunkEntry(..), WorldRivers(..) )
-- 'Lake' is a constructor of both 'FluidType' and the lake table's
-- own record type, so take only the classifier this suite asserts on.
import World.Fluid.Types (FluidCell(..), FluidType(River))
import World.Generate.Chunk.Fluid
    ( chunkWaterSurfMap, composeFluidMap, lakeSurfaceMap
    , riverSurfaceMap )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Geology.Timeline.Types (GeoTimeline(..))

chunkAreaT ∷ Int
chunkAreaT = chunkSize * chunkSize

coordT ∷ ChunkCoord
coordT = ChunkCoord 0 0

-- | 'minBound' is the fold's ABSENT sentinel, spelled out here so the
--   expectations below never borrow the implementation's spelling.
absent ∷ Int
absent = minBound

-- | The documented merge, written independently of the fold: absent
--   plus absent is absent, absent plus a real value is that value,
--   two real values are their minimum. A plain @min@ would fail the
--   second clause, since 'absent' is the smallest 'Int'.
mergeSurfaces ∷ Int → Int → Int
mergeSurfaces a b
    | a ≡ absent = b
    | b ≡ absent = a
    | otherwise  = min a b

bitmaskAt ∷ [Int] → VU.Vector Bool
bitmaskAt is = VU.generate chunkAreaT (`elem` is)

-- | A river entry's per-tile surface vector: 'absent' off the
--   bitmask, as the real identifier produces.
surfsAt ∷ [(Int, Int)] → VU.Vector Int
surfsAt pairs =
    VU.generate chunkAreaT $ \i →
        case lookup i pairs of
            Just z  → z
            Nothing → absent

mkLake ∷ Int → Lake
mkLake surf = Lake
    { lkSurface  = surf
    , lkFloor    = surf - 5
    , lkArea     = 1
    , lkBBoxMinX = 0
    , lkBBoxMinY = 0
    , lkBBoxMaxX = chunkSize - 1
    , lkBBoxMaxY = chunkSize - 1
    }

-- | A lake table whose only chunk is 'coordT'. Each @(surface,
--   tiles)@ pair becomes one lake and its chunk entry.
mkLakes ∷ [(Int, [Int])] → WorldLakes
mkLakes bodies = WorldLakes
    { wlLakes      = V.fromList [ mkLake s | (s, _) ← bodies ]
    , wlByChunk    = HM.singleton coordT $ V.fromList
        [ LakeChunkEntry { lceLakeId = lid, lceBitmask = bitmaskAt is }
        | (lid, (_, is)) ← zip [0 ..] bodies
        ]
    , wlCarveDelta = HM.empty
    }

-- | A river table whose only chunk is 'coordT'. Each entry is a list
--   of @(tile, surface)@ claims.
mkRivers ∷ [[(Int, Int)]] → WorldRivers
mkRivers bodies = WorldRivers
    { wrRivers     = V.empty
    , wrByChunk    = HM.singleton coordT $ V.fromList
        [ RiverChunkEntry
            { rceRiverId      = rid
            , rceBitmask      = bitmaskAt (map fst claims)
            , rcePerTileSurfZ = surfsAt claims
            , rceWidthRadius  = VU.replicate chunkAreaT 0
            }
        | (rid, claims) ← zip [0 ..] bodies
        ]
    , wrCarveDelta = HM.empty
    }

paramsWith ∷ WorldLakes → WorldRivers → WorldGenParams
paramsWith lakes rivers = defaultWorldGenParams
    { wgpGeoTimeline = (wgpGeoTimeline defaultWorldGenParams)
        { gtWorldLakes  = lakes
        , gtWorldRivers = rivers
        }
    }

-- Tiles used across the merge cases.
lakeOnlyT, riverOnlyT, lakeLowerT, riverLowerT, unclaimedT ∷ Int
lakeOnlyT   = 10
riverOnlyT  = 20
lakeLowerT  = 30
riverLowerT = 40
unclaimedT  = 50

spec ∷ Spec
spec = do
    describe "the lake-keyed fold" $ do
        it "leaves an unclaimed tile at the absent sentinel" $ do
            let v = lakeSurfaceMap (mkLakes [(7, [lakeOnlyT])]) coordT
            v VU.! unclaimedT `shouldBe` absent
            VU.length v `shouldBe` chunkAreaT

        it "writes a body's surface on every tile its bitmask claims" $ do
            let v = lakeSurfaceMap (mkLakes [(7, [3, 4])]) coordT
            (v VU.! 3, v VU.! 4) `shouldBe` (7, 7)

        it "keeps the lower surface where two lakes overlap, in either \
           \declaration order" $ do
            let lowFirst  = mkLakes [(2, [5]), (9, [5])]
                highFirst = mkLakes [(9, [5]), (2, [5])]
            lakeSurfaceMap lowFirst  coordT VU.! 5 `shouldBe` 2
            lakeSurfaceMap highFirst coordT VU.! 5 `shouldBe` 2

        it "answers for a chunk with no entries at all" $
            lakeSurfaceMap (mkLakes []) coordT
                `shouldBe` VU.replicate chunkAreaT absent

    describe "the river-keyed fold" $ do
        it "leaves an unclaimed tile at the absent sentinel" $ do
            let v = riverSurfaceMap (mkRivers [[(riverOnlyT, 7)]]) coordT
            v VU.! unclaimedT `shouldBe` absent

        it "reads each claimed tile's own surface" $ do
            let v = riverSurfaceMap (mkRivers [[(3, 7), (4, 11)]]) coordT
            (v VU.! 3, v VU.! 4) `shouldBe` (7, 11)

        it "keeps the lower surface where two rivers overlap, in either \
           \declaration order" $ do
            let lowFirst  = mkRivers [[(5, 2)], [(5, 9)]]
                highFirst = mkRivers [[(5, 9)], [(5, 2)]]
            riverSurfaceMap lowFirst  coordT VU.! 5 `shouldBe` 2
            riverSurfaceMap highFirst coordT VU.! 5 `shouldBe` 2

    describe "chunkWaterSurfMap merges the two sources by the sentinel \
             \rule" $ do
        let lakes = mkLakes [(4, [lakeOnlyT, lakeLowerT, riverLowerT])]
            rivers = mkRivers
                [[ (riverOnlyT, 6), (lakeLowerT, 8), (riverLowerT, 1) ]]
            -- lakeLowerT: lake 4 vs river 8. riverLowerT: lake 4 vs
            -- river 1.
            merged = chunkWaterSurfMap (paramsWith lakes rivers) coordT

        it "leaves a tile neither source claims absent" $
            merged VU.! unclaimedT `shouldBe` absent

        it "keeps a lake-only tile, which a plain min would blank" $
            merged VU.! lakeOnlyT `shouldBe` 4

        it "keeps a river-only tile, which a plain min would blank" $
            merged VU.! riverOnlyT `shouldBe` 6

        it "takes the lower of two real surfaces, whichever source it \
           \came from" $
            (merged VU.! lakeLowerT, merged VU.! riverLowerT)
                `shouldBe` (4, 1)

    describe "the two water-surface consumers share one fold" $
        it "chunkWaterSurfMap is exactly the sentinel merge of the \
           \per-source folds, tile for tile" $ do
            let lakes  = mkLakes
                    [ (4, [0 .. 40]), (9, [30 .. 80]), (2, [200]) ]
                rivers = mkRivers
                    [ [ (i, 3 + i `mod` 5) | i ← [20 .. 60] ]
                    , [ (i, 12 - i `mod` 7) | i ← [55 .. 90] ]
                    ]
                params = paramsWith lakes rivers
                expected = VU.zipWith mergeSurfaces
                    (lakeSurfaceMap lakes coordT)
                    (riverSurfaceMap rivers coordT)
            chunkWaterSurfMap params coordT `shouldBe` expected

    describe "sharing the fold does not merge the two downstream \
             \policies" $
        it "composeFluidMap still classifies River over Lake where \
           \chunkWaterSurfMap reports the lower lake surface" $ do
            -- One tile claimed by a LOW lake and a HIGH river, terrain
            -- below both. The classifier answers "which fluid is
            -- this"; the surface map answers "how high is the water".
            -- They are allowed to differ, and must keep differing.
            let terrZ    = seaLevel + 10
                lakeSurf = seaLevel + 12
                riverSurf = seaLevel + 20
                lakes  = mkLakes [(lakeSurf, [lakeLowerT])]
                rivers = mkRivers [[(lakeLowerT, riverSurf)]]
                params = paramsWith lakes rivers
                terrain = VU.replicate chunkAreaT terrZ
                cells = composeFluidMap params coordT terrain
            cells V.! lakeLowerT
                `shouldBe` Just (FluidCell River riverSurf)
            chunkWaterSurfMap params coordT VU.! lakeLowerT
                `shouldBe` lakeSurf
