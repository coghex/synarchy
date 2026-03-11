module Test.Headless.WorldGen.Rivers (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Headless.Harness
import World.Types

spec ∷ Spec
spec = around withHeadlessEngine $ do

    describe "River generation" $ do

        it "generates worlds with geological periods" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "rivers") 42 64 5)
            ws ← waitForWorldInit env (WorldPageId "rivers") 180
            mParams ← getWorldGenParams ws
            case mParams of
                Just params → do
                    let timeline = wgpGeoTimeline params
                        periods  = gtPeriods timeline
                    length periods `shouldSatisfy` (> 0)
                Nothing → expectationFailure "params should exist"

        it "some chunks have river fluid" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "rivfluid") 42 64 5)
            ws ← waitForWorldInit env (WorldPageId "rivfluid") 180
            tiles ← getWorldTileData ws
            let chunks = HM.elems (wtdChunks tiles)
                riverChunks = filter hasRiverFluid chunks
            length chunks `shouldSatisfy` (> 0)
            length riverChunks `shouldSatisfy` (> 0)

    describe "River water physics" $ do

        it "river water surface is never below terrain" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "noFloat") 42 64 5)
            ws ← waitForWorldInit env (WorldPageId "noFloat") 180
            tiles ← getWorldTileData ws
            let violations = concatMap findFloatingWater (HM.toList (wtdChunks tiles))
            case violations of
                [] → pure ()
                vs → expectationFailure $
                    "Found " ⧺ show (length vs) ⧺ " floating water tiles "
                    ⧺ "(water surface below terrain):\n"
                    ⧺ unlines (take 10 (map showFloating vs))

        it "river tiles have no exposed unbounded sides" $ \env → do
            -- For every river tile, each cardinal neighbor must either:
            --   (a) also have water (river continues)
            --   (b) have terrain >= the water surface (land bounds the water)
            --   (c) be outside the loaded chunk set (edge of world — acceptable)
            -- If a neighbor has no water AND terrain below the water surface,
            -- the water tile would render with a visible floating side face.
            sendWorldCommand env (WorldInit (WorldPageId "noBoundless") 42 64 5)
            ws ← waitForWorldInit env (WorldPageId "noBoundless") 180
            tiles ← getWorldTileData ws
            let chunkMap = wtdChunks tiles
                violations = concatMap (findExposedSides chunkMap)
                                       (HM.toList chunkMap)
            case violations of
                [] → pure ()
                vs → expectationFailure $
                    "Found " ⧺ show (length vs)
                    ⧺ " river tiles with exposed unbounded sides:\n"
                    ⧺ unlines (take 20 (map showExposed vs))

        it "adjacent river tiles within a chunk do not jump more than 4 levels" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "noUphill") 42 64 5)
            ws ← waitForWorldInit env (WorldPageId "noUphill") 180
            tiles ← getWorldTileData ws
            let violations = concatMap findUphillWater (HM.toList (wtdChunks tiles))
            case violations of
                [] → pure ()
                vs → expectationFailure $
                    "Found " ⧺ show (length vs)
                    ⧺ " adjacent river pairs with surface jump > "
                    ⧺ show maxSurfaceJump ⧺ ":\n"
                    ⧺ unlines (take 10 (map showUphill vs))

        it "adjacent river tiles across chunk boundaries do not jump more than 4 levels" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "noCrossUphill") 42 64 5)
            ws ← waitForWorldInit env (WorldPageId "noCrossUphill") 180
            tiles ← getWorldTileData ws
            let violations = findCrossChunkUphill (wtdChunks tiles)
            case violations of
                [] → pure ()
                vs → expectationFailure $
                    "Found " ⧺ show (length vs)
                    ⧺ " cross-chunk river pairs with surface jump > "
                    ⧺ show maxSurfaceJump ⧺ ":\n"
                    ⧺ unlines (take 10 (map showCross vs))

-- | Maximum allowed surface elevation jump between adjacent river tiles
maxSurfaceJump ∷ Int
maxSurfaceJump = 4

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
-- Exposed unbounded sides: the core "visible floating water" test
--
-- A river tile has an "exposed unbounded side" if a cardinal neighbor:
--   - has NO water (Nothing or non-river fluid)
--   - AND has terrain BELOW the water surface of the river tile
-- That means you'd see the side face of the water with air underneath.
-- ──────────────────────────────────────────────────────────

data ExposedViolation = ExposedViolation
    { evChunk    ∷ ChunkCoord
    , evLX       ∷ Int
    , evLY       ∷ Int
    , evWaterZ   ∷ Int
    , evDir      ∷ String    -- which side is exposed
    , evNbrTerr  ∷ Int       -- neighbor's terrain Z
    , evNbrChunk ∷ ChunkCoord
    }

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
    -- Produce (direction, neighborLocalX, neighborLocalY, neighborChunkCoord)
    -- wrapping across chunk boundaries
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
                    Just nfc | fcType nfc ≡ River → []  -- neighbor has river water — bounded
                    Just nfc | fcType nfc ≡ Lake  → []  -- lake water also bounds
                    Just nfc | fcType nfc ≡ Ocean → []  -- ocean bounds
                    _ →
                        -- No water on neighbor. Is terrain high enough to bound?
                        if nbrTerrZ ≥ waterZ
                            then []  -- land walls off the water — bounded
                            else [ExposedViolation myChunk
                                    (if nbrChunk ≡ myChunk then nx else nx)
                                    (if nbrChunk ≡ myChunk then ny else ny)
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

findUphillWater ∷ (ChunkCoord, LoadedChunk) → [UphillViolation]
findUphillWater (coord, lc) =
    [ UphillViolation coord (lx, ly) (nx, ny) z1 z2
    | ly ← [0..chunkSize-1], lx ← [0..chunkSize-1]
    , let idx1 = ly * chunkSize + lx
    , Just fc1 ← [lcFluidMap lc V.! idx1]
    , fcType fc1 ≡ River, let z1 = fcSurface fc1
    , (nx, ny) ← [(lx+1, ly), (lx, ly+1)]
    , nx < chunkSize, ny < chunkSize
    , let idx2 = ny * chunkSize + nx
    , Just fc2 ← [lcFluidMap lc V.! idx2]
    , fcType fc2 ≡ River, let z2 = fcSurface fc2
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

    -- isXEdge=True: compare lx=edgeVal in lc vs lx=nbrVal in neighbor (same ly)
    -- isXEdge=False: compare ly=edgeVal in lc vs ly=nbrVal in neighbor (same lx)
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
                , fcType fc1 ≡ River, let z1 = fcSurface fc1
                , Just fc2 ← [lcFluidMap nbrLC V.! idx2]
                , fcType fc2 ≡ River, let z2 = fcSurface fc2
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
