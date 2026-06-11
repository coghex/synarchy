{-# LANGUAGE Strict, UnicodeSyntax #-}
module Test.Headless.WorldGen.Parity (spec) where

-- | Parity test for the two terrain-application paths.
--
-- `applyTimelineChunk` is the authoritative path used during chunk
-- generation. `applyTimelineFast` is a per-tile approximation used
-- at world-init time before chunks exist (ocean flood-fill, zoom
-- cache, Lua `camera.goToTile`). Audit #1 brought their semantics
-- close together (matching hardness, matching event set, lockstep
-- 5-tile neighbors for slope-based erosion). They still diverge by
-- a few tile-units in practice because the fast path has no cliff
-- smoothing or spike removal (those are chunk-scope only).
--
-- This test samples tiles from generated chunks and queries the
-- fast path for the same coordinates, asserting the divergence is
-- bounded — catches future regressions if either path is changed.

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Data.List (sort)
import Data.IORef (readIORef)
import Test.Headless.Harness
import Engine.Core.State (EngineEnv(..))
import World.Types
import World.Plate (elevationAtGlobal)
import World.Generate (applyTimelineFast)

spec ∷ SpecWith EngineEnv
spec = do

    describe "Chunk path vs fast path parity" $

        it "tile elevations agree within tolerance for sampled coords" $ \env → do
            -- Own (seed, size, plates) — the thresholds below were
            -- calibrated on 42/64/5; don't converge this onto the
            -- canonical 42/64/3 world without recalibrating.
            ws ← sharedWorld env 42 64 5
            mParams ← getWorldGenParams ws
            tiles ← getWorldTileData ws
            registry ← readIORef (materialRegistryRef env)

            case mParams of
                Nothing → expectationFailure "gen params should exist"
                Just params → do
                    let timeline  = wgpGeoTimeline params
                        plates    = wgpPlates params
                        worldSize = wgpWorldSize params
                        seed      = gtSeed timeline
                        -- Take up to 12 chunks; sample 4×4 interior tiles each
                        -- (≈ 192 samples) so the per-tile distribution
                        -- statistics are stable across seeds.
                        chunks    = take 12 (HM.elems (wtdChunks tiles))
                        sampleOffsets = [(lx, ly) | lx ← [2, 6, 10, 14]
                                                  , ly ← [2, 6, 10, 14]]

                        sampleChunk lc =
                            let ChunkCoord cx cy = lcCoord lc
                                terrSurf = lcTerrainSurfaceMap lc
                            in [ (chunkZ, fastZ)
                               | (lx, ly) ← sampleOffsets
                               , let idx    = ly * chunkSize + lx
                                     chunkZ = terrSurf VU.! idx
                                     gx     = cx * chunkSize + lx
                                     gy     = cy * chunkSize + ly
                                     (baseE, baseM) =
                                         elevationAtGlobal seed plates worldSize gx gy
                                     (fastZ, _) = applyTimelineFast timeline plates
                                         worldSize gx gy registry (baseE, baseM)
                               ]

                        allPairs = concatMap sampleChunk chunks
                        diffs    = map (\(c, f) → abs (c - f)) allPairs
                        total    = length diffs
                        within5  = length (filter (≤ 5)  diffs)
                        within15 = length (filter (≤ 15) diffs)
                        within40 = length (filter (≤ 40) diffs)
                        maxDiff  = if null diffs then 0 else maximum diffs
                        sorted   = sort diffs
                        topN     = drop (max 0 (total - 5)) sorted
                        -- 95th percentile: robust to single outliers
                        -- that come from cliff-smoothing of tall pillars.
                        p95 = if null sorted then 0
                              else sorted !! min (total - 1) (total * 95 `div` 100)

                    -- Diagnostic so a future test failure has context.
                    putStrLn $ "PARITY: total=" ⧺ show total
                            ⧺ " within±5=" ⧺ show within5
                            ⧺ " within±15=" ⧺ show within15
                            ⧺ " within±40=" ⧺ show within40
                            ⧺ " p95=" ⧺ show p95
                            ⧺ " max=" ⧺ show maxDiff
                            ⧺ " top5=" ⧺ show topN

                    -- Sanity: we actually sampled some tiles.
                    total `shouldSatisfy` (> 0)

                    -- Thresholds are a regression net, not a tight
                    -- physical bound — the fast path lacks cliff
                    -- smoothing and spike removal, so tiles adjacent
                    -- to river-carved channels can collapse by 100+
                    -- tile-units in the chunk path. The 95th-percentile
                    -- bound ignores these outliers while still catching
                    -- a systematic regression in the broad distribution.
                    -- Calibrated 2026-05-22; relaxed to 0.40 on
                    -- 2026-06-06 for the volcanism default bump; then
                    -- RE-TIGHTENED 2026-06-07 when the fast path
                    -- gained the global coastal table (save v25). The
                    -- old windowed coastal pass was too expensive for
                    -- the fast path, so it skipped coastal entirely —
                    -- a ~20z systematic divergence in coastal zones.
                    -- With the per-tile table lookup, seed 42 w64
                    -- measures 73% within ±5, p95 = 34:
                    --   • ≥55% within ±5  (measured 73%)
                    --   • ≥85% within ±40 (measured 98%)
                    --   • p95 < 80        (measured 34)
                    -- Drift beyond means one of the two paths has
                    -- meaningfully changed semantics.
                    let frac5  = fromIntegral within5  / fromIntegral total ∷ Double
                        frac40 = fromIntegral within40 / fromIntegral total ∷ Double
                    frac5  `shouldSatisfy` (≥ 0.55)
                    frac40 `shouldSatisfy` (≥ 0.85)
                    p95    `shouldSatisfy` (< 80)
