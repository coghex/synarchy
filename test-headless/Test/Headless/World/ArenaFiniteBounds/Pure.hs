{-# LANGUAGE OverloadedStrings #-}
-- | "arena finite bounds" — the pure half (#2708, SCN-08 of epic #2698):
--   the rectangle, the dimension domain, tiles and positions at every
--   edge and corner, partial chunks, the generator's treatment of the
--   columns outside the map, the shared clipping and footprint results,
--   and a location's all-or-nothing allocation.
--
--   Every expectation is a LITERAL written from the issue's own examples
--   and formulae (20×10 is X −10…9, Y −4…5; 21×11 is X −10…10, Y −5…5;
--   1×1 is only (0, 0)), never computed with the code under test.
module Test.Headless.World.ArenaFiniteBounds.Pure (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Location.ArenaPlacement
    ( LocationPlacementError(..), allocateLocationInstanceIn )
import Location.Bounds (AbsBounds(..), RelBounds(..))
import Location.Instance
    ( LocationInstance(..), LocationInstances(..), emptyLocationInstances )
import Location.Types (LocationDef(..), LocationNaming(..))
import Language.Semantic.Types (ConceptId(..))
import Scenario.Bounds (TileBounds(..))
import Scenario.Types (Footprint(..), TileRegion(..))
import World.Arena.Bounds
import World.Render (surfaceHeadroom, trackedSliceTarget)
import World.Tile.Types (emptyWorldTileData, insertChunk)
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Constants (seaLevel)
import World.Generate.Arena
    ( absentColumn, absentSurfaceZ, arenaFootprint, arenaStartupChunks
    , clipArenaChunk, generateArenaChunk, generateArenaChunkIn
    , generateArenaChunksIn )

-- | The rectangle of each size, as literals: (width, height) →
--   (minX, maxX, minY, maxY). Even, odd and mixed parity, the minimum,
--   smaller than a chunk, exactly a chunk, and the schema's maximum.
rectTable ∷ [((Int, Int), (Int, Int, Int, Int))]
rectTable =
    [ ((20, 10),   (-10, 9, -4, 5))
    , ((21, 11),   (-10, 10, -5, 5))
    , ((20, 11),   (-10, 9, -5, 5))
    , ((21, 10),   (-10, 10, -4, 5))
    , ((1, 1),     (0, 0, 0, 0))
    , ((2, 1),     (-1, 0, 0, 0))
    , ((1, 2),     (0, 0, 0, 1))
    , ((3, 3),     (-1, 1, -1, 1))
    , ((5, 5),     (-2, 2, -2, 2))
    , ((16, 16),   (-8, 7, -7, 8))
    , ((31, 31),   (-15, 15, -15, 15))
    , ((32, 32),   (-16, 15, -15, 16))
    , ((100000, 100000), (-50000, 49999, -49999, 50000))
    ]

bounds ∷ Int → Int → ArenaBounds
bounds w h = either (error . show) id (mkArenaBounds w h)

rectOf ∷ ArenaBounds → (Int, Int, Int, Int)
rectOf ab = let TileBounds x0 y0 x1 y1 = arenaRect ab in (x0, x1, y0, y1)

-- | Every column index of a chunk whose tile is inside a literal
--   rectangle, computed without the module under test.
presentIndices ∷ (Int, Int, Int, Int) → ChunkCoord → [Int]
presentIndices (x0, x1, y0, y1) (ChunkCoord cx cy) =
    [ ly * chunkSize + lx
    | ly ← [0 .. chunkSize - 1], lx ← [0 .. chunkSize - 1]
    , let gx = cx * chunkSize + lx
    , let gy = cy * chunkSize + ly
    , gx ≥ x0, gx ≤ x1, gy ≥ y0, gy ≤ y1 ]

spec ∷ Spec
spec = describe "the rectangle and its pure consumers" $ do
    rectangleSpec
    domainSpec
    tileAndPositionSpec
    chunkSpec
    generatorSpec
    apiSpec
    locationSpec
    cameraTrackingSpec

rectangleSpec ∷ Spec
rectangleSpec = describe "dimensions to rectangle" $ do

    it "is exactly the issue's formula for even, odd, mixed, 1×1, sub-chunk and maximum sizes" $
        forM_ rectTable $ \((w, h), expected) →
            (w, h, rectOf (bounds w h)) `shouldBe` (w, h, expected)

    it "holds exactly width × height tiles" $
        forM_ rectTable $ \((w, h), (x0, x1, y0, y1)) →
            ((x1 - x0 + 1), (y1 - y0 + 1)) `shouldBe` (w, h)

    it "keeps the width and height it was built from" $
        forM_ rectTable $ \((w, h), _) → do
            arenaWidth (bounds w h) `shouldBe` w
            arenaHeight (bounds w h) `shouldBe` h

domainSpec ∷ Spec
domainSpec = describe "the dimension domain" $ do

    it "refuses zero, negative, over-maximum and overflowing dimensions" $
        forM_ [ (0, 5), (5, 0), (0, 0), (-1, 3), (3, -1), (minBound, 3)
              , (3, minBound), (100001, 1), (1, 100001), (maxBound, maxBound)
              , (maxBound, 1), (1, maxBound) ] $ \(w, h) →
            mkArenaBounds w h `shouldBe` Left (DimensionOutOfDomain w h)

    it "accepts the first and last value of the domain on both axes" $
        forM_ [ (1, 1), (100000, 1), (1, 100000), (100000, 100000) ] $ \(w, h) →
            either (const False) (const True) (mkArenaBounds w h) `shouldBe` True

    it "names the offending size in an actionable message" $
        arenaBoundsErrorText (DimensionOutOfDomain 0 7)
            `shouldSatisfy` T.isInfixOf "0x7"

tileAndPositionSpec ∷ Spec
tileAndPositionSpec = describe "tiles and positions at every edge and corner" $ do

    it "admits each corner and edge tile and refuses the tile just beyond each" $
        forM_ rectTable $ \((w, h), (x0, x1, y0, y1)) → do
            let mb = Just (bounds w h)
                inside = [ (x0, y0), (x1, y0), (x0, y1), (x1, y1)
                         , (x0, 0 `max` y0 `min` y1), (x1, 0 `max` y0 `min` y1)
                         , (0 `max` x0 `min` x1, y0), (0 `max` x0 `min` x1, y1) ]
                outside = [ (x0 - 1, y0), (x1 + 1, y1), (x0, y0 - 1), (x1, y1 + 1)
                          , (x0 - 1, y0 - 1), (x1 + 1, y1 + 1)
                          , (x0 - 1, y1 + 1), (x1 + 1, y0 - 1) ]
            forM_ inside $ \(x, y) → (w, h, x, y, arenaAdmitsTile mb x y)
                `shouldBe` (w, h, x, y, True)
            forM_ outside $ \(x, y) → (w, h, x, y, arenaAdmitsTile mb x y)
                `shouldBe` (w, h, x, y, False)

    it "admits every tile of an expandable arena, extreme coordinates included" $
        forM_ [ (0, 0), (minBound, maxBound), (maxBound, minBound), (-9999999, 12345678) ] $ \(x, y) →
            arenaAdmitsTile Nothing x y `shouldBe` True

    it "does not wrap an overflowing tile coordinate into the rectangle" $ do
        let mb = Just (bounds 20 10)
        arenaAdmitsTile mb maxBound 0 `shouldBe` False
        arenaAdmitsTile mb 0 minBound `shouldBe` False

    it "places a continuous position by the tile each coordinate FLOORS to" $ do
        let mb = Just (bounds 20 10)   -- X -10..9, Y -4..5
        -- 9.999 is still tile 9; 10.0 is tile 10, outside.
        arenaAdmitsPosition mb 9.999 0.5 `shouldBe` True
        arenaAdmitsPosition mb 10.0 0.5 `shouldBe` False
        -- -10.0 is tile -10, inside; just below it is tile -11, outside.
        arenaAdmitsPosition mb (-10.0) 0.5 `shouldBe` True
        arenaAdmitsPosition mb (-10.001) 0.5 `shouldBe` False
        arenaAdmitsPosition mb 0.5 5.999 `shouldBe` True
        arenaAdmitsPosition mb 0.5 6.0 `shouldBe` False
        arenaAdmitsPosition mb 0.5 (-4.0) `shouldBe` True
        arenaAdmitsPosition mb 0.5 (-4.001) `shouldBe` False

    it "refuses non-finite and enormous positions on a finite arena only" $ do
        let mb = Just (bounds 20 10)
            nan = 0 / 0 ∷ Float
            inf = 1 / 0 ∷ Float
        forM_ [ (nan, 0), (0, nan), (inf, 0), (0, -inf), (1.0e30, 0), (0, -1.0e30) ] $ \(x, y) →
            arenaAdmitsPosition mb x y `shouldBe` False
        arenaAdmitsPosition Nothing 1.0e30 (-1.0e30) `shouldBe` True

    it "clamps an outside position to a point that floors INSIDE, at any magnitude" $ do
        forM_ [ bounds 20 10, bounds 1 1, bounds 100000 100000, bounds 21 11 ] $ \ab →
            forM_ [ (1.0e9, 1.0e9), (-1.0e9, -1.0e9), (50000.5, -50000.5)
                  , (123456.7, 0.2), (0.2, -987654.3), (10.0, 6.0) ] $ \(x, y) → do
                let (cx, cy) = clampPositionIntoArena ab x y
                (ab, cx, cy, arenaAdmitsPosition (Just ab) cx cy)
                    `shouldBe` (ab, cx, cy, True)

    it "leaves an inside position where it is" $ do
        let ab = bounds 20 10
        clampPositionIntoArena ab 3.25 (-2.5) `shouldBe` (3.25, -2.5)

chunkSpec ∷ Spec
chunkSpec = describe "partial and outside chunks" $ do

    it "agrees with the literal column set at every chunk around every rectangle" $
        forM_ rectTable $ \((w, h), rect) →
            when (w ≤ 100) $
              forM_ [ ChunkCoord cx cy | cx ← [-4 .. 3], cy ← [-4 .. 3] ] $ \c → do
                let ab = bounds w h
                    expected = presentIndices rect c
                    mask = chunkColumnMask ab c
                    got = [ i | i ← [0 .. 255], mask VU.! i ]
                (w, h, c, got) `shouldBe` (w, h, c, expected)
                chunkIntersectsArena ab c `shouldBe` (not (null expected))
                chunkInsideArena ab c `shouldBe` (length expected ≡ 256)

    it "gives 20×10 four partial chunks with 40, 40, 60 and 60 columns" $ do
        let ab = bounds 20 10
        [ length (presentIndices (rectOf ab) c)
          | c ← [ChunkCoord (-1) (-1), ChunkCoord 0 (-1), ChunkCoord (-1) 0, ChunkCoord 0 0] ]
            `shouldBe` [40, 40, 60, 60]
        [ c | cx ← [-3 .. 2], cy ← [-3 .. 2], let c = ChunkCoord cx cy
            , chunkIntersectsArena ab c ]
            `shouldBe` [ ChunkCoord (-1) (-1), ChunkCoord (-1) 0
                       , ChunkCoord 0 (-1), ChunkCoord 0 0 ]

    it "gives a 1×1 arena exactly one chunk with exactly one column" $ do
        let ab = bounds 1 1
        [ c | cx ← [-3 .. 3], cy ← [-3 .. 3], let c = ChunkCoord cx cy
            , chunkIntersectsArena ab c ] `shouldBe` [ChunkCoord 0 0]
        [ i | i ← [0 .. 255], chunkColumnMask ab (ChunkCoord 0 0) VU.! i ] `shouldBe` [0]

    it "filters demand to the intersecting chunks and keeps order and duplicates" $ do
        let ab = bounds 20 10
        boundedChunkCoords (Just ab)
            [ ChunkCoord 5 5, ChunkCoord 0 0, ChunkCoord (-1) 0, ChunkCoord 0 0
            , ChunkCoord (-2) 0, ChunkCoord 0 (-2) ]
            `shouldBe` [ ChunkCoord 0 0, ChunkCoord (-1) 0, ChunkCoord 0 0 ]
        boundedChunkCoords Nothing [ChunkCoord 9999 (-9999)] `shouldBe` [ChunkCoord 9999 (-9999)]

    it "reports the chunk range that intersects the rectangle" $ do
        arenaChunkRange (bounds 20 10) `shouldBe` ((-1, -1), (0, 0))
        arenaChunkRange (bounds 1 1) `shouldBe` ((0, 0), (0, 0))
        arenaChunkRange (bounds 100 100) `shouldBe` ((-4, -4), (3, 3))

    it "starts a finite arena with the footprint restricted to what intersects" $ do
        arenaStartupChunks Nothing `shouldBe` arenaFootprint
        length (arenaStartupChunks (Just (bounds 100 100))) `shouldBe` 25
        arenaStartupChunks (Just (bounds 1 1)) `shouldBe` [ChunkCoord 0 0]
        length (arenaStartupChunks (Just (bounds 20 10))) `shouldBe` 4
        length (arenaStartupChunks (Just (bounds 5 5))) `shouldBe` 4
        map lcCoord (generateArenaChunksIn (Just (bounds 1 1)) 0) `shouldBe` [ChunkCoord 0 0]

-- | The columns outside the rectangle, and the ones inside.
generatorSpec ∷ Spec
generatorSpec = describe "the generator on a partial chunk" $ do

    it "builds the in-rectangle columns exactly as the expandable arena does" $
        forM_ [ (20, 10, ChunkCoord 0 0), (20, 10, ChunkCoord (-1) (-1))
              , (5, 5, ChunkCoord (-1) 0), (1, 1, ChunkCoord 0 0), (21, 11, ChunkCoord 0 (-1)) ] $
            \(w, h, c) → do
                let ab = bounds w h
                    finite = generateArenaChunkIn (Just ab) 0 c
                    open = generateArenaChunk 0 c
                    present = presentIndices (rectOf ab) c
                forM_ present $ \i → do
                    lcTiles finite V.! i `shouldBe` lcTiles open V.! i
                    lcSurfaceMap finite VU.! i `shouldBe` seaLevel
                    lcTerrainSurfaceMap finite VU.! i `shouldBe` seaLevel

    it "leaves every other column absent: no cells, no surface, no vegetation, no fluid slot" $
        forM_ [ (20, 10, ChunkCoord 0 0), (5, 5, ChunkCoord (-1) (-1)), (1, 1, ChunkCoord 0 0) ] $
            \(w, h, c) → do
                let ab = bounds w h
                    lc = generateArenaChunkIn (Just ab) 0 c
                    present = presentIndices (rectOf ab) c
                forM_ [ i | i ← [0 .. 255], i `notElem` present ] $ \i → do
                    lcTiles lc V.! i `shouldBe` absentColumn
                    VU.length (ctMats (lcTiles lc V.! i)) `shouldBe` 0
                    VU.length (ctVeg (lcTiles lc V.! i)) `shouldBe` 0
                    lcSurfaceMap lc VU.! i `shouldBe` absentSurfaceZ
                    lcTerrainSurfaceMap lc VU.! i `shouldBe` absentSurfaceZ
                    lcFluidMap lc V.! i `shouldBe` Nothing
                    lcSideDeco lc VU.! i `shouldBe` 0
                absentSurfaceZ `shouldBe` minBound

    it "is deterministic: the same seed and rectangle give the same chunk in any order" $ do
        let ab = Just (bounds 20 10)
            cs = [ChunkCoord 0 0, ChunkCoord (-1) 0, ChunkCoord 0 (-1), ChunkCoord (-1) (-1)]
        map (generateArenaChunkIn ab 0) cs
            `shouldBe` reverse (map (generateArenaChunkIn ab 0) (reverse cs))

    it "is unchanged by an expandable page: no rectangle, the #2707 chunk" $
        forM_ arenaFootprint $ \c →
            generateArenaChunkIn Nothing 0 c `shouldBe` generateArenaChunk 0 c

    it "clips a replayed edit that landed outside the rectangle back to absent" $ do
        let ab = bounds 20 10
            c = ChunkCoord 0 0
            raised = (generateArenaChunkIn (Just ab) 0 c)
                { lcTiles = V.imap (\i col → if i ≡ 15 * 16 + 15
                      then col { ctMats = VU.snoc (VU.replicate 16 1) 1
                               , ctSlopes = VU.replicate 17 0
                               , ctVeg = VU.replicate 17 0 }
                      else col) (lcTiles (generateArenaChunkIn (Just ab) 0 c))
                , lcSurfaceMap = lcSurfaceMap (generateArenaChunkIn (Just ab) 0 c)
                    VU.// [(15 * 16 + 15, seaLevel + 1)] }
        lcTiles (clipArenaChunk (Just ab) raised) V.! (15 * 16 + 15) `shouldBe` absentColumn
        lcSurfaceMap (clipArenaChunk (Just ab) raised) VU.! (15 * 16 + 15) `shouldBe` absentSurfaceZ
        clipArenaChunk Nothing raised `shouldBe` raised

-- | The clipping and footprint results later adapters consume.
apiSpec ∷ Spec
apiSpec = describe "shared clipping and footprint validation" $ do

    it "keeps the in-bounds tiles of a patch and reports the rejected ones in order" $ do
        let mb = Just (bounds 20 10)   -- X -10..9, Y -4..5
            tiles = [ (0, 0), (10, 0), (-10, -4), (-11, -4), (9, 5), (9, 6), (3, -5) ]
        clipTiles mb tiles `shouldBe` TileClip
            { tcKept = [ (0, 0), (-10, -4), (9, 5) ]
            , tcRejected = [ (10, 0), (-11, -4), (9, 6), (3, -5) ] }
        clipTiles Nothing tiles `shouldBe` TileClip tiles []

    it "clips a rectangle patch to its in-bounds part and counts what it dropped" $ do
        let mb = Just (bounds 20 10)
        clipTileRegion mb (RegionRect (-12) (-6) 12 8)
            `shouldBe` (Just (RegionRect (-10) (-4) 9 5), 25 * 15 - 20 * 10)
        clipTileRegion mb (RegionRect 20 20 30 30) `shouldBe` (Nothing, 121)
        clipTileRegion mb (RegionRect 0 0 2 2) `shouldBe` (Just (RegionRect 0 0 2 2), 0)
        clipTileRegion mb (RegionTiles [(0, 0), (50, 50)])
            `shouldBe` (Just (RegionTiles [(0, 0)]), 1)
        clipTileRegion Nothing (RegionRect (-9999) (-9999) 9999 9999)
            `shouldBe` (Just (RegionRect (-9999) (-9999) 9999 9999), 0)

    it "fits a footprint only when its COMPLETE extent is inside, on every side" $ do
        let mb = Just (bounds 20 10)
            fp = Footprint 0 0 2 1          -- 3 wide, 2 tall from the anchor
        forM_ [ ((-10, -4), True), ((7, 4), True), ((8, 4), False), ((7, 5), False)
              , ((-11, -4), False), ((-10, -5), False), ((9, 5), False), ((7, 3), True) ] $
            \(anchor, fits) → (anchor, footprintWithin mb anchor fp) `shouldBe` (anchor, fits)
        footprintVerdict mb (8, 4) fp `shouldBe` FootprintCrosses [(10, 4), (10, 5)]
        footprintVerdict mb (7, 4) fp `shouldBe` FootprintFits
        footprintWithin Nothing (123456, -654321) fp `shouldBe` True

    it "judges a footprint without overflowing at the Int limits" $
        footprintWithin (Just (bounds 20 10)) (maxBound, maxBound) (Footprint 0 0 5 5)
            `shouldBe` False

-- | A location is all or nothing.
locationSpec ∷ Spec
locationSpec = describe "a location against the rectangle" $ do

    it "is added when its whole box fits" $ do
        let ab = bounds 40 40         -- X -20..19, Y -19..20
        case allocateLocationInstanceIn (Just ab) Nothing (ChunkCoord 0 0) ruin emptyLocationInstances of
            Left e → expectationFailure (show e)
            Right (_, lis) → do
                lisNextId lis `shouldBe` lisNextId emptyLocationInstances + 1
                map liBounds (HM.elems (lisById lis)) `shouldBe` [AbsBounds 6 6 10 10]

    it "is rejected whole, spending no id and storing nothing, when any part crosses" $ do
        let before = emptyLocationInstances
        -- (1,0) on 40×40, and (0,0) on 10×10, lie wholly outside; (0,0) on
        -- 17×17 (X, Y = −8…8, box 6…10) STRADDLES the edge
        forM_ [ (bounds 40 40, ChunkCoord 1 0), (bounds 10 10, ChunkCoord 0 0)
              , (bounds 17 17, ChunkCoord 0 0) ] $ \(b, c) →
            case allocateLocationInstanceIn (Just b) Nothing c ruin before of
                Right _ → expectationFailure ("accepted a crossing location at " ⧺ show c)
                Left (PlacementCrossesArena corners) → null corners `shouldBe` False
                Left other → expectationFailure (show other)
        -- the table the caller already holds is exactly as it was
        before `shouldBe` emptyLocationInstances

    it "is added unchanged on an expandable page" $
        case allocateLocationInstanceIn Nothing Nothing (ChunkCoord 1000 (-1000)) ruin emptyLocationInstances of
            Left e → expectationFailure (show e)
            Right (_, lis) → lisNextId lis `shouldBe` lisNextId emptyLocationInstances + 1

ruin ∷ LocationDef
ruin = LocationDef
    { ldId = "ruin", ldLabel = "Small Ruin", ldType = "ruin"
    , ldBuilder = "room_small", ldAnchor = [], ldMaxCount = 0
    , ldMinSpacing = 0, ldContents = []
    , ldBounds = RelBounds (-2) (-2) 2 2
    , ldMapIcon = Nothing
    , ldNaming = LocationNaming [ConceptId "KEEP"] [ConceptId "ASH"]
    }

-- | Camera z-tracking over a finite arena's partial chunks (#2708). The
--   edge chunks are resident WITH their outside padding, so the track
--   reads real chunks whose absent columns hold 'minBound'.
cameraTrackingSpec ∷ Spec
cameraTrackingSpec = describe "camera z-tracking across a finite edge" $ do
    let ab = bounds 20 10
        (x0, x1, y0, y1) = rectOf ab
        tileData = foldr (insertChunk . generateArenaChunkIn (Just ab) 0) emptyWorldTileData
                         (arenaStartupChunks (Just ab))
        target gx gy = trackedSliceTarget tileData
                           (ChunkCoord (gx `div` chunkSize) (gy `div` chunkSize))
                           (gx `mod` chunkSize, gy `mod` chunkSize)
        inside gx gy = gx >= x0 && gx <= x1 && gy >= y0 && gy <= y1

    it "follows the surface over every tile of the map" $
        forM_ [ (gx, gy) | gx ← [x0 .. x1], gy ← [y0 .. y1] ] $ \(gx, gy) →
            target gx gy `shouldBe` Just (seaLevel + surfaceHeadroom)

    it "never sends the slice to the floor of the z domain: a column the map does not have leaves it alone" $
        forM_ [ (gx, gy) | gx ← [x0 - 6 .. x1 + 6], gy ← [y0 - 6 .. y1 + 6] ] $ \(gx, gy) → do
            -- a pan across the edge, sampled every tile, inside resident
            -- partial chunks
            target gx gy `shouldBe`
                (if inside gx gy then Just (seaLevel + surfaceHeadroom) else Nothing)

    it "has no answer at all for a chunk that is not resident" $
        trackedSliceTarget tileData (ChunkCoord 40 40) (0, 0) `shouldBe` Nothing
