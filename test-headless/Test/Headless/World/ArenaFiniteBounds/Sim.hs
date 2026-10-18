{-# LANGUAGE OverloadedStrings #-}
-- | "arena finite bounds" — closed fluid edges (#2708 requirement 5).
--
--   No fluid may flow into, across or out through the outside of a
--   finite arena. The fixtures take their terrain from the PRODUCTION
--   generator ('generateArenaChunkIn'), so the padding of a partial
--   chunk is exactly what it is in a running arena — a column LOWER than
--   the map, which is the worst case: if the simulation treated it as a
--   neighbouring column, water at the edge would pour into it.
--
--   Every example drives the real tick ('simulateActiveTick') or the
--   real synchronous settle ('fastSettleWorld'). Each control runs the
--   SAME fixture on an unbounded topology and shows the water DOES leave
--   there, so a passing example proves the closure and not an inert
--   fixture.
module Test.Headless.World.ArenaFiniteBounds.Sim (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Sim.Chunk (loadedChunkState)
import Sim.Fluid.Active (simulateActiveTick)
import Sim.Fluid.Types (ActiveFluidCell(..))
import Sim.State.Types (SimChunkState(..), SimWorldState(..), emptySimWorldState)
import Sim.Thread (fastSettleWorld)
import Sim.Topology
    ( SimTopology(..), simCanonChunk, simCellLive, simChunkLiveMask
    , simTopologyBounds, simTopologyForParams )
import World.Arena.Bounds (ArenaBounds, mkArenaBounds)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Exact (fluidUnitsPerZ)
import World.Fluid.Types (FluidType(..))
import World.Generate.Arena (generateArenaChunkIn)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Types (LoadedChunk(..))

n ∷ Int
n = chunkSize * chunkSize

bounds ∷ Int → Int → ArenaBounds
bounds w h = either (error . show) id (mkArenaBounds w h)

arenaParams ∷ Maybe ArenaBounds → WorldGenParams
arenaParams mb = defaultWorldGenParams
    { wgpSeed = 0, wgpWorldSize = 100000, wgpArenaBounds = mb }

-- | An ACTIVE sim chunk over the PRODUCTION terrain of one finite-arena
--   chunk, holding the given volumes.
chunkWith ∷ Maybe ArenaBounds → ChunkCoord → [(Int, Word16)] → SimChunkState
chunkWith = chunkWithPadding Nothing

-- | The same, optionally standing the padding up as an ordinary LOWER
--   column (the control: what an unaware simulation would see).
chunkWithPadding ∷ Maybe Int → Maybe ArenaBounds → ChunkCoord → [(Int, Word16)] → SimChunkState
chunkWithPadding padding mb coord vols =
    let raw = lcTerrainSurfaceMap (generateArenaChunkIn mb 0 coord)
        terrain = maybe raw (\z → VU.map (\t → if t ≡ minBound then z else t) raw) padding
        base = loadedChunkState (V.replicate n Nothing) terrain
        grid = V.replicate n Nothing
            V.// [ (i, Just (ActiveFluidCell Lake v 0)) | (i, v) ← vols ]
    in base { scsSettleTicks = 0, scsActive = True, scsActiveFluid = grid }

world ∷ SimTopology → [(ChunkCoord, SimChunkState)] → SimWorldState
world topo chunks = emptySimWorldState
    { swsChunks = HM.fromList chunks, swsActive = True, swsTopology = topo }

total ∷ SimWorldState → Int
total sws = sum
    [ fromIntegral (afcVolume c)
    | scs ← HM.elems (swsChunks sws), Just c ← V.toList (scsActiveFluid scs) ]

-- | Active volume at every column, by chunk and index.
volumes ∷ SimWorldState → [(ChunkCoord, Int, Int)]
volumes sws =
    [ (cc, i, fromIntegral (afcVolume c))
    | (cc, scs) ← HM.toList (swsChunks sws)
    , (i, Just c) ← zip [0 ..] (V.toList (scsActiveFluid scs)) ]

ticks ∷ Int → SimWorldState → SimWorldState
ticks k sws = iterate simulateActiveTick sws !! k

-- | The global tile of a column.
tileOf ∷ ChunkCoord → Int → (Int, Int)
tileOf (ChunkCoord cx cy) i = (cx * chunkSize + i `mod` chunkSize, cy * chunkSize + i `div` chunkSize)

inside ∷ (Int, Int, Int, Int) → (Int, Int) → Bool
inside (x0, x1, y0, y1) (x, y) = x ≥ x0 ∧ x ≤ x1 ∧ y ≥ y0 ∧ y ≤ y1

idx ∷ Int → Int → Int
idx lx ly = ly * chunkSize + lx

lakeUnits ∷ Int → Word16
lakeUnits z = fromIntegral (z * fluidUnitsPerZ)

spec ∷ Spec
spec = describe "closed fluid edges" $ do
    topologySpec
    insideChunkSpec
    cornersSpec
    seamSpec
    settleSpec

topologySpec ∷ Spec
topologySpec = describe "the simulation's view of the page" $ do

    it "is flat for an arena with no bounds and finite for one with them" $ do
        simTopologyForParams (arenaParams Nothing) `shouldBe` SimFlatTopology
        simTopologyForParams (arenaParams (Just (bounds 20 10)))
            `shouldBe` SimFiniteFlat (bounds 20 10)
        simTopologyBounds (SimFiniteFlat (bounds 20 10)) `shouldBe` Just (bounds 20 10)
        simTopologyBounds SimFlatTopology `shouldBe` Nothing

    it "derives the bounds from the page's own state, not from the arena sentinel" $ do
        -- the sentinel world size is 100000 on every arena; only the
        -- recorded bounds make one finite
        wgpWorldSize (arenaParams Nothing) `shouldBe` wgpWorldSize (arenaParams (Just (bounds 3 3)))
        simTopologyForParams (arenaParams Nothing) `shouldNotBe`
            simTopologyForParams (arenaParams (Just (bounds 3 3)))

    it "leaves a generated world's topology alone, whatever its params carry" $ do
        simTopologyForParams defaultWorldGenParams { wgpWorldSize = 64 }
            `shouldBe` SimCylindricalU 64

    it "keeps chunk keys as they are (flat) and tells live columns from padding" $ do
        let topo = SimFiniteFlat (bounds 20 10)
        simCanonChunk topo (ChunkCoord 5 (-5)) `shouldBe` ChunkCoord 5 (-5)
        simChunkLiveMask topo (ChunkCoord 0 0) `shouldSatisfy` (\m → fmap VU.length m ≡ Just 256)
        simCellLive topo (ChunkCoord 0 0) (idx 9 5) `shouldBe` True
        simCellLive topo (ChunkCoord 0 0) (idx 10 5) `shouldBe` False
        simCellLive topo (ChunkCoord 0 0) (idx 9 6) `shouldBe` False
        simCellLive topo (ChunkCoord (-1) (-1)) (idx 6 12) `shouldBe` True    -- (-10,-4)
        simCellLive topo (ChunkCoord (-1) (-1)) (idx 5 12) `shouldBe` False    -- (-11,-4)
        simCellLive topo (ChunkCoord (-1) (-1)) (idx 6 11) `shouldBe` False    -- (-10,-5)
        simCellLive SimFlatTopology (ChunkCoord 0 0) (idx 15 15) `shouldBe` True

-- | Inside one chunk: an edge cell beside absent padding must not leak.
insideChunkSpec ∷ Spec
insideChunkSpec = describe "inside a partial chunk" $ do

    let ab = bounds 20 10                 -- X -10..9, Y -4..5
        rect = (-10, 9, -4, 5)
        c00 = ChunkCoord 0 0              -- live x 0..9, y 0..5
        water = [ (idx 9 2, lakeUnits 6)  -- east edge: x=10 is padding
                , (idx 4 5, lakeUnits 6)  -- north edge: y=6 is padding
                , (idx 9 5, lakeUnits 6) ] -- corner
        mk topo = world topo [(c00, chunkWith (Just ab) c00 water)]
        -- the control: the padding stood up as an ordinary LOWER column
        mkOpen = world SimFlatTopology
            [(c00, chunkWithPadding (Just (seaLevel - 16)) (Just ab) c00 water)]
        initial = 3 * fromIntegral (lakeUnits 6) ∷ Int

    it "keeps water out of the padding, so none leaves and none is lost" $ do
        let after = ticks 150 (mk (SimFiniteFlat ab))
        total after `shouldBe` initial
        [ v | (cc, i, v) ← volumes after, not (inside rect (tileOf cc i)), v > 0 ]
            `shouldBe` []

    it "does let the same water drain into the lower padding when the edge is open" $ do
        -- the control: identical chunk, unbounded topology
        let after = ticks 150 mkOpen
        [ () | (cc, i, v) ← volumes after, not (inside rect (tileOf cc i)), v > 0 ]
            `shouldSatisfy` (not . null)

    it "still spreads water between live columns, the ordinary physics untouched" $ do
        let after = ticks 150 (mk (SimFiniteFlat ab))
            liveCells = [ (i, v) | (_, i, v) ← volumes after ]
        length liveCells `shouldSatisfy` (> 3)

    it "keeps a 1×1 arena's single column closed on every side" $ do
        let one = bounds 1 1
            c = ChunkCoord 0 0
            st = world (SimFiniteFlat one) [(c, chunkWith (Just one) c [(0, lakeUnits 8)])]
            after = ticks 100 st
        volumes after `shouldBe` [(c, 0, 8 * fluidUnitsPerZ)]

-- | All four corners and edges of a four-chunk arena.
cornersSpec ∷ Spec
cornersSpec = describe "every corner and edge of the rectangle" $ do

    it "contains water placed at each of the four corners" $ do
        let ab = bounds 20 10
            rect = (-10, 9, -4, 5)
            chunks = [ChunkCoord (-1) (-1), ChunkCoord 0 (-1), ChunkCoord (-1) 0, ChunkCoord 0 0]
            corners = [(-10, -4), (9, -4), (-10, 5), (9, 5)]
            place cc = [ (idx (gx - cx * 16) (gy - cy * 16), lakeUnits 5)
                       | (gx, gy) ← corners
                       , let ChunkCoord cx cy = cc
                       , gx `div` 16 ≡ cx, gy `div` 16 ≡ cy ]
            mk topo = world topo [ (cc, chunkWith (Just ab) cc (place cc)) | cc ← chunks ]
            initial = 4 * fromIntegral (lakeUnits 5) ∷ Int
            closed = ticks 150 (mk (SimFiniteFlat ab))
        total closed `shouldBe` initial
        [ v | (cc, i, v) ← volumes closed, not (inside rect (tileOf cc i)), v > 0 ] `shouldBe` []
        -- the water is still there, inside the map
        [ () | (_, _, v) ← volumes closed, v > 0 ] `shouldSatisfy` (not . null)

-- | Seams between loaded chunks of a larger arena, and a neighbour that
--   is not loaded.
seamSpec ∷ Spec
seamSpec = describe "chunk seams" $ do

    let ab = bounds 40 40                 -- X -20..19, Y -19..20
        rect = (-20, 19, -19, 20)
        west = ChunkCoord (-1) 0          -- fully live (x -16..-1)
        east = ChunkCoord 0 0             -- fully live (x 0..15)
        far  = ChunkCoord 1 0             -- partial: live x 16..19

    it "still exchanges water across a seam between two live chunks" $ do
        let st = world (SimFiniteFlat ab)
                [ (west, chunkWith (Just ab) west [])
                , (east, chunkWith (Just ab) east [(idx 0 8, lakeUnits 8)]) ]
            after = ticks 150 st
        total after `shouldBe` 8 * fluidUnitsPerZ
        [ v | (cc, _, v) ← volumes after, cc ≡ west, v > 0 ] `shouldSatisfy` (not . null)

    it "closes the far edge of a partial chunk that is joined to a live one" $ do
        let st = world (SimFiniteFlat ab)
                [ (east, chunkWith (Just ab) east [])
                , (far, chunkWith (Just ab) far [(idx 3 8, lakeUnits 8)]) ]   -- x = 19
            after = ticks 150 st
        total after `shouldBe` 8 * fluidUnitsPerZ
        [ v | (cc, i, v) ← volumes after, not (inside rect (tileOf cc i)), v > 0 ] `shouldBe` []

    it "does not drain through a chunk that is not loaded" $ do
        let c = ChunkCoord 3 3             -- a live chunk of a bigger arena, no neighbours loaded
            big = bounds 200 200
            st = world (SimFiniteFlat big)
                [ (c, chunkWith (Just big) c [(idx 15 15, lakeUnits 6), (idx 0 0, lakeUnits 6)]) ]
            after = ticks 150 st
        total after `shouldBe` 2 * fromIntegral (lakeUnits 6)

settleSpec ∷ Spec
settleSpec = describe "the synchronous settle" $ do

    it "settles with the edge closed: nothing in the padding, something left in the map" $ do
        let ab = bounds 20 10
            rect = (-10, 9, -4, 5)
            c = ChunkCoord 0 0
            st = world (SimFiniteFlat ab)
                [ (c, (chunkWith (Just ab) c [(idx 9 2, lakeUnits 6), (idx 4 5, lakeUnits 6)])
                        { scsSettleTicks = 5 }) ]
            settled = fastSettleWorld 1000 st
            wet = [ (cc, i) | (cc, scs) ← HM.toList (swsChunks settled)
                            , (i, Just _) ← zip [0 ∷ Int ..] (V.toList (scsFluid scs)) ]
        [ () | (cc, i) ← wet, not (inside rect (tileOf cc i)) ] `shouldBe` []
        wet `shouldSatisfy` (not . null)

    it "lets the open-edge control lose water into the padding" $ do
        let ab = bounds 20 10
            rect = (-10, 9, -4, 5)
            c = ChunkCoord 0 0
            st = world SimFlatTopology
                [ (c, (chunkWithPadding (Just (seaLevel - 16)) (Just ab) c [(idx 9 2, lakeUnits 6), (idx 4 5, lakeUnits 6)])
                        { scsSettleTicks = 5 }) ]
            settled = fastSettleWorld 1000 st
            wet = [ (cc, i) | (cc, scs) ← HM.toList (swsChunks settled)
                            , (i, Just _) ← zip [0 ∷ Int ..] (V.toList (scsFluid scs)) ]
        [ () | (cc, i) ← wet, not (inside rect (tileOf cc i)) ] `shouldSatisfy` (not . null)
