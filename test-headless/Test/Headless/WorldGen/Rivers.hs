module Test.Headless.WorldGen.Rivers (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef)
import Control.Concurrent (threadDelay)
import Control.Monad (foldM)
import qualified Data.Text as T
import Test.Headless.Harness
import World.Types
import World.Grid (gridToWorld)
import World.Geology.Timeline.Types (GeoTimeline(..), PersistentFeature(..)
                                    , FeatureShape(..))
import World.Hydrology.Types (HydroFeature(..))
import Engine.Graphics.Camera (CameraFacing(..))

spec ∷ Spec
spec = around withHeadlessEngine $ do

    describe "River existence" $ do

        it "generates worlds with geological periods" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "rivers") 42 128 5)
            ws ← waitForWorldInit env (WorldPageId "rivers") 180
            mParams ← getWorldGenParams ws
            case mParams of
                Just params → do
                    let timeline = wgpGeoTimeline params
                        periods  = gtPeriods timeline
                    length periods `shouldSatisfy` (> 0)
                Nothing → expectationFailure "params should exist"

        it "some chunks have river fluid across sampled areas" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "rivExist") 42 128 5)
            ws ← waitForWorldInit env (WorldPageId "rivExist") 180
            -- Sample several areas to find river fluid
            let samplePoints = [(0, 0), (4, 4), (-4, -4), (6, -3), (-3, 6)]
            totalRiverChunks ← foldM (\acc (cx, cy) → do
                let (wx, wy) = gridToWorld FaceSouth (cx * chunkSize) (cy * chunkSize)
                moveCamera env wx wy
                threadDelay 500000
                tiles ← getWorldTileData ws
                let chunks = HM.elems (wtdChunks tiles)
                    riverChunks = filter hasRiverFluid chunks
                pure (acc + length riverChunks)
                ) 0 samplePoints
            totalRiverChunks `shouldSatisfy` (> 0)

        it "timeline contains river features across multiple seeds" $ \env → do
            -- Every generated world should have at least one river feature
            let seeds = [42, 123, 999, 2024, 7777]
            forM_ seeds $ \seed → do
                let pid = WorldPageId (T.pack ("multiSeed" ⧺ show seed))
                sendWorldCommand env (WorldInit pid seed 128 5)
                ws ← waitForWorldInit env pid 180
                mParams ← getWorldGenParams ws
                case mParams of
                    Nothing → expectationFailure $
                        "seed " ⧺ show seed ⧺ ": no params"
                    Just params → do
                        let features = gtFeatures (wgpGeoTimeline params)
                            rivers = filter isRiverFeature features
                        length rivers `shouldSatisfy` (> 0)
                sendWorldCommand env (WorldDestroy pid)
                threadDelay 200000

    describe "River water physics" $ do

        it "river water surface is never below terrain" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "noFloat") 42 128 5)
            ws ← waitForWorldInit env (WorldPageId "noFloat") 180
            tiles ← getWorldTileData ws
            let violations = concatMap findFloatingWater (HM.toList (wtdChunks tiles))
            case violations of
                [] → pure ()
                vs → expectationFailure $
                    "Found " ⧺ show (length vs) ⧺ " floating water tiles "
                    ⧺ "(water surface below terrain):\n"
                    ⧺ unlines (take 10 (map showFloating vs))

        it "river tiles have no exposed unbounded sides (multi-seed)" $ \env → do
            let seeds = [42, 123, 999, 2024, 7777]
            forM_ seeds $ \seed → do
                let pid = WorldPageId (T.pack ("noBound" ⧺ show seed))
                sendWorldCommand env (WorldInit pid seed 128 5)
                ws ← waitForWorldInit env pid 180
                -- Check initial chunks
                tiles0 ← getWorldTileData ws
                let violations0 = findAllExposedSides tiles0
                -- Also move camera to a few spots and check new chunks
                let samplePoints = [(3, 3), (-3, -3), (5, -2), (-2, 5)]
                allViolations ← foldM (\acc (cx, cy) → do
                    let (wx, wy) = gridToWorld FaceSouth (cx * chunkSize) (cy * chunkSize)
                    moveCamera env wx wy
                    threadDelay 500000
                    tiles ← getWorldTileData ws
                    let vs = findAllExposedSides tiles
                    pure (acc ⧺ vs)
                    ) violations0 samplePoints
                case allViolations of
                    [] → pure ()
                    vs → expectationFailure $
                        "seed " ⧺ show seed ⧺ ": Found "
                        ⧺ show (length vs)
                        ⧺ " river tiles with exposed unbounded sides:\n"
                        ⧺ unlines (take 20 (map showExposed vs))
                sendWorldCommand env (WorldDestroy pid)
                threadDelay 200000

        it "adjacent river tiles within a chunk do not jump more than 2 levels (multi-seed)" $ \env → do
            let seeds = [42, 123, 999, 2024, 7777]
            forM_ seeds $ \seed → do
                let pid = WorldPageId (T.pack ("noUphill" ⧺ show seed))
                sendWorldCommand env (WorldInit pid seed 128 5)
                ws ← waitForWorldInit env pid 180
                -- Check with camera at multiple positions
                let samplePoints = [(0, 0), (3, 3), (-3, -3), (5, -2)]
                allViolations ← foldM (\acc (cx, cy) → do
                    let (wx, wy) = gridToWorld FaceSouth (cx * chunkSize) (cy * chunkSize)
                    moveCamera env wx wy
                    threadDelay 500000
                    tiles ← getWorldTileData ws
                    let vs = concatMap findUphillWater (HM.toList (wtdChunks tiles))
                    pure (acc ⧺ vs)
                    ) [] samplePoints
                case allViolations of
                    [] → pure ()
                    vs → expectationFailure $
                        "seed " ⧺ show seed ⧺ ": Found " ⧺ show (length vs)
                        ⧺ " adjacent river pairs with surface jump > "
                        ⧺ show maxSurfaceJump ⧺ ":\n"
                        ⧺ unlines (take 10 (map showUphill vs))
                sendWorldCommand env (WorldDestroy pid)
                threadDelay 200000

        it "adjacent river tiles across chunk boundaries do not jump more than 2 levels (multi-seed)" $ \env → do
            let seeds = [42, 123, 999, 2024, 7777]
            forM_ seeds $ \seed → do
                let pid = WorldPageId (T.pack ("noCrossUp" ⧺ show seed))
                sendWorldCommand env (WorldInit pid seed 128 5)
                ws ← waitForWorldInit env pid 180
                let samplePoints = [(0, 0), (3, 3), (-3, -3), (5, -2)]
                allViolations ← foldM (\acc (cx, cy) → do
                    let (wx, wy) = gridToWorld FaceSouth (cx * chunkSize) (cy * chunkSize)
                    moveCamera env wx wy
                    threadDelay 500000
                    tiles ← getWorldTileData ws
                    let vs = findCrossChunkUphill (wtdChunks tiles)
                    pure (acc ⧺ vs)
                    ) [] samplePoints
                case allViolations of
                    [] → pure ()
                    vs → expectationFailure $
                        "seed " ⧺ show seed ⧺ ": Found " ⧺ show (length vs)
                        ⧺ " cross-chunk river pairs with surface jump > "
                        ⧺ show maxSurfaceJump ⧺ ":\n"
                        ⧺ unlines (take 10 (map showCross vs))
                sendWorldCommand env (WorldDestroy pid)
                threadDelay 200000

-- | Maximum allowed surface elevation jump between adjacent river tiles
maxSurfaceJump ∷ Int
maxSurfaceJump = 2

-- ──────────────────────────────────────────────────────────
-- Floating water: water surface below terrain
-- ──────────────────────────────────────────────────────────

data FloatingViolation = FloatingViolation
    { fvChunk ∷ ChunkCoord, fvLX ∷ Int, fvLY ∷ Int
    , fvWaterZ ∷ Int, fvTerrainZ ∷ Int }

findFloatingWater ∷ (ChunkCoord, LoadedChunk) → [FloatingViolation]
findFloatingWater (coord, lc) =
    [ FloatingViolation coord lx ly waterZ terrZ
    | ly ← [0..chunkSize-1], lx ← [0..chunkSize-1]
    , let idx = ly * chunkSize + lx
    , Just fc ← [lcFluidMap lc V.! idx]
    , fcType fc ≡ River
    , let waterZ = fcSurface fc
          terrZ  = lcTerrainSurfaceMap lc VU.! idx
    , waterZ < terrZ
    ]

showFloating ∷ FloatingViolation → String
showFloating v = "  chunk=" ⧺ showCoord (fvChunk v)
    ⧺ " (" ⧺ show (fvLX v) ⧺ "," ⧺ show (fvLY v) ⧺ ")"
    ⧺ " waterZ=" ⧺ show (fvWaterZ v)
    ⧺ " terrainZ=" ⧺ show (fvTerrainZ v)

-- ──────────────────────────────────────────────────────────
-- Exposed unbounded sides
-- ──────────────────────────────────────────────────────────

data ExposedViolation = ExposedViolation
    { evChunk    ∷ ChunkCoord
    , evLX       ∷ Int
    , evLY       ∷ Int
    , evWaterZ   ∷ Int
    , evDir      ∷ String
    , evNbrTerr  ∷ Int
    , evNbrChunk ∷ ChunkCoord
    }

findAllExposedSides ∷ WorldTileData → [ExposedViolation]
findAllExposedSides tiles =
    let chunkMap = wtdChunks tiles
    in concatMap (findExposedSides chunkMap) (HM.toList chunkMap)

findExposedSides ∷ HM.HashMap ChunkCoord LoadedChunk
                 → (ChunkCoord, LoadedChunk) → [ExposedViolation]
findExposedSides chunkMap (coord@(ChunkCoord cx cy), lc) =
    [ violation
    | ly ← [0..chunkSize-1], lx ← [0..chunkSize-1]
    , let idx = ly * chunkSize + lx
    , Just fc ← [lcFluidMap lc V.! idx]
    , fcType fc ≡ River
    , let waterZ = fcSurface fc
    , (dir, nx, ny, nChunkCoord) ← neighbors lx ly
    , violation ← checkNeighbor waterZ dir coord nChunkCoord nx ny
    ]
  where
    neighbors ∷ Int → Int → [(String, Int, Int, ChunkCoord)]
    neighbors lx ly =
        [ ("N", lx, ly-1, coord)   | ly > 0 ] ⧺
        [ ("S", lx, ly+1, coord)   | ly < chunkSize-1 ] ⧺
        [ ("W", lx-1, ly, coord)   | lx > 0 ] ⧺
        [ ("E", lx+1, ly, coord)   | lx < chunkSize-1 ] ⧺
        -- Cross-chunk neighbors
        [ ("N", lx, chunkSize-1, ChunkCoord cx (cy-1)) | ly ≡ 0 ] ⧺
        [ ("S", lx, 0, ChunkCoord cx (cy+1))           | ly ≡ chunkSize-1 ] ⧺
        [ ("W", chunkSize-1, ly, ChunkCoord (cx-1) cy) | lx ≡ 0 ] ⧺
        [ ("E", 0, ly, ChunkCoord (cx+1) cy)           | lx ≡ chunkSize-1 ]

    checkNeighbor ∷ Int → String → ChunkCoord → ChunkCoord
                  → Int → Int → [ExposedViolation]
    checkNeighbor waterZ dir myChunk nbrChunk nx ny =
        case HM.lookup nbrChunk chunkMap of
            Nothing → []  -- chunk not loaded, edge of world — acceptable
            Just nbrLC →
                let nIdx = ny * chunkSize + nx
                    nbrFluid = lcFluidMap nbrLC V.! nIdx
                    nbrTerrZ = lcTerrainSurfaceMap nbrLC VU.! nIdx
                in case nbrFluid of
                    Just nfc | fcType nfc ≡ River → []
                    Just nfc | fcType nfc ≡ Lake  → []
                    Just nfc | fcType nfc ≡ Ocean → []
                    _ →
                        if nbrTerrZ ≥ waterZ
                            then []
                            else [ExposedViolation myChunk nx ny
                                    waterZ dir nbrTerrZ nbrChunk]

showExposed ∷ ExposedViolation → String
showExposed v = "  chunk=" ⧺ showCoord (evChunk v)
    ⧺ " (" ⧺ show (evLX v) ⧺ "," ⧺ show (evLY v) ⧺ ")"
    ⧺ " waterZ=" ⧺ show (evWaterZ v)
    ⧺ " side=" ⧺ evDir v
    ⧺ " nbrTerrain=" ⧺ show (evNbrTerr v)
    ⧺ " (gap=" ⧺ show (evWaterZ v - evNbrTerr v) ⧺ ")"
    ⧺ " nbrChunk=" ⧺ showCoord (evNbrChunk v)

-- ──────────────────────────────────────────────────────────
-- Intra-chunk uphill: adjacent river tiles with big surface jump
-- ──────────────────────────────────────────────────────────

data UphillViolation = UphillViolation
    { uvChunk ∷ ChunkCoord
    , uvFrom  ∷ (Int, Int), uvTo ∷ (Int, Int)
    , uvFromZ ∷ Int, uvToZ ∷ Int }

-- | Check ALL water-to-water adjacencies (river-river, river-lake, lake-lake, etc.)
--   for surface jumps exceeding the threshold.
findUphillWater ∷ (ChunkCoord, LoadedChunk) → [UphillViolation]
findUphillWater (coord, lc) =
    [ UphillViolation coord (lx, ly) (nx, ny) z1 z2
    | ly ← [0..chunkSize-1], lx ← [0..chunkSize-1]
    , let idx1 = ly * chunkSize + lx
    , Just fc1 ← [lcFluidMap lc V.! idx1]
    , fcType fc1 ≢ Ocean  -- skip ocean (infinite body, always level)
    , let z1 = fcSurface fc1
    , (nx, ny) ← [(lx+1, ly), (lx, ly+1)]
    , nx < chunkSize, ny < chunkSize
    , let idx2 = ny * chunkSize + nx
    , Just fc2 ← [lcFluidMap lc V.! idx2]
    , fcType fc2 ≢ Ocean
    , let z2 = fcSurface fc2
    , abs (z1 - z2) > maxSurfaceJump
    ]

showUphill ∷ UphillViolation → String
showUphill v = "  chunk=" ⧺ showCoord (uvChunk v)
    ⧺ " " ⧺ show (uvFrom v) ⧺ "(z=" ⧺ show (uvFromZ v) ⧺ ")"
    ⧺ " → " ⧺ show (uvTo v) ⧺ "(z=" ⧺ show (uvToZ v) ⧺ ")"
    ⧺ " jump=" ⧺ show (abs (uvFromZ v - uvToZ v))

-- ──────────────────────────────────────────────────────────
-- Cross-chunk uphill: river tiles at chunk borders with big jump
-- ──────────────────────────────────────────────────────────

data CrossViolation = CrossViolation
    { ccChunk1 ∷ ChunkCoord, ccLocal1 ∷ (Int, Int), ccZ1 ∷ Int
    , ccChunk2 ∷ ChunkCoord, ccLocal2 ∷ (Int, Int), ccZ2 ∷ Int }

findCrossChunkUphill ∷ HM.HashMap ChunkCoord LoadedChunk → [CrossViolation]
findCrossChunkUphill chunkMap =
    concatMap checkBorders (HM.toList chunkMap)
  where
    checkBorders (coord@(ChunkCoord cx cy), lc) =
        checkEdge coord lc (chunkSize-1) True  (ChunkCoord (cx+1) cy) 0 True
      ⧺ checkEdge coord lc (chunkSize-1) False (ChunkCoord cx (cy+1)) 0 False

    checkEdge coord lc edgeVal isXEdge nbrCoord nbrVal _ =
        case HM.lookup nbrCoord chunkMap of
            Nothing → []
            Just nbrLC →
                [ CrossViolation coord p1 z1 nbrCoord p2 z2
                | i ← [0..chunkSize-1]
                , let (lx1, ly1) = if isXEdge then (edgeVal, i) else (i, edgeVal)
                      (lx2, ly2) = if isXEdge then (nbrVal, i)  else (i, nbrVal)
                      idx1 = ly1 * chunkSize + lx1
                      idx2 = ly2 * chunkSize + lx2
                      p1 = (lx1, ly1)
                      p2 = (lx2, ly2)
                , Just fc1 ← [lcFluidMap lc V.! idx1]
                , fcType fc1 ≢ Ocean, let z1 = fcSurface fc1
                , Just fc2 ← [lcFluidMap nbrLC V.! idx2]
                , fcType fc2 ≢ Ocean, let z2 = fcSurface fc2
                , abs (z1 - z2) > maxSurfaceJump
                ]

showCross ∷ CrossViolation → String
showCross v = "  " ⧺ showCoord (ccChunk1 v)
    ⧺ show (ccLocal1 v) ⧺ "(z=" ⧺ show (ccZ1 v) ⧺ ")"
    ⧺ " → " ⧺ showCoord (ccChunk2 v)
    ⧺ show (ccLocal2 v) ⧺ "(z=" ⧺ show (ccZ2 v) ⧺ ")"
    ⧺ " jump=" ⧺ show (abs (ccZ1 v - ccZ2 v))

-- ──────────────────────────────────────────────────────────
-- Helpers
-- ──────────────────────────────────────────────────────────

showCoord ∷ ChunkCoord → String
showCoord (ChunkCoord cx cy) = "(" ⧺ show cx ⧺ "," ⧺ show cy ⧺ ")"

hasRiverFluid ∷ LoadedChunk → Bool
hasRiverFluid lc = any isRiver (V.toList (lcFluidMap lc))
  where isRiver (Just (FluidCell River _)) = True
        isRiver _ = False

isRiverFeature ∷ PersistentFeature → Bool
isRiverFeature pf = case pfFeature pf of
    HydroShape (RiverFeature _) → True
    _ → False
