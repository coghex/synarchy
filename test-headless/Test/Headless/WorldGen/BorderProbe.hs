{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Cross-window pipeline consistency at a chunk seam.
--
--   Replays the bordered chunk pipeline stage by stage for two
--   adjacent chunks and asserts every SHARED global tile gets the
--   same value from both windows at every stage. Regression net for
--   the coastline-divergence class fixed in save v25: the old
--   per-window 'applyCoastalErosion' (12-pass smoother + chained
--   BFS, information horizon ≈ 36 tiles vs a 14-tile shared border)
--   computed coastlines that disagreed by up to 18z between windows,
--   manufacturing cliffs at chunk seams (first seen as the
--   muck-on-cliff audit canary, seed 7 w64 plates 4 at (-79,127)).
--   Any window-dependent operation reintroduced into the pipeline
--   (a neighborhood pass whose reach exceeds the shared border)
--   fails this spec.
module Test.Headless.WorldGen.BorderProbe (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef)
import Control.Monad (forM_)
import Test.Headless.Harness
import Engine.Core.State (EngineEnv(..))
import World.Types
import World.Material (matGlacier)
import World.Plate (elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Constants (seaLevel)
import World.Scale (computeWorldScale)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)
import World.Generate.Timeline (applyTimelineChunk, removeElevationSpikes)
import World.Geology.Coastal (applyCoastalTable)
import World.Geology.Timeline.Types (GeoTimeline(..))
import World.Fluid.River.Types (wrCarveDelta)
import World.Fluid.Lake.Types (wlCarveDelta)

spec ∷ Spec
spec = aroundAll withHeadlessEngine $
    describe "Border divergence probe" $
        it "stage-by-stage A-window vs B-window at the seam" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "probe") 7 64 4)
            ws ← waitForWorldInit env (WorldPageId "probe") 240
            mParams ← getWorldGenParams ws
            registry ← readIORef (materialRegistryRef env)
            case mParams of
                Nothing → expectationFailure "gen params should exist"
                Just params → do
                    let seed      = wgpSeed params
                        worldSize = wgpWorldSize params
                        timeline  = wgpGeoTimeline params
                        plates    = wgpPlates params
                        wsc       = computeWorldScale worldSize
                        borderSize = chunkSize + 2 * chunkBorder
                        borderArea = borderSize * borderSize

                        carveAt gx gy =
                            let cx = gx `div` chunkSize
                                cy = gy `div` chunkSize
                                ilx = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                                ily = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                                li  = ily * chunkSize + ilx
                                dr  = case HM.lookup (ChunkCoord cx cy)
                                                     (wrCarveDelta (gtWorldRivers timeline)) of
                                        Just dv → dv VU.! li
                                        Nothing → 0
                                dl  = case HM.lookup (ChunkCoord cx cy)
                                                     (wlCarveDelta (gtWorldLakes timeline)) of
                                        Just dv → dv VU.! li
                                        Nothing → 0
                            in max dr dl

                        -- Replicate generateChunk's bordered pipeline for
                        -- one chunk; return each stage's bordered vector.
                        stagesFor coord =
                            let toIndex lx ly =
                                    (ly + chunkBorder) * borderSize + (lx + chunkBorder)
                                fromIndex idx =
                                    let (by, bx) = idx `divMod` borderSize
                                    in (bx - chunkBorder, by - chunkBorder)
                                (baseElevVec, baseMatVec) =
                                    let pairs = [ if isBeyondGlacier worldSize gx' gy'
                                                  then (seaLevel + 100, matGlacier)
                                                  else elevationAtGlobal seed plates worldSize gx' gy'
                                                | idx ← [0 .. borderArea - 1]
                                                , let (lx, ly) = fromIndex idx
                                                      (gx, gy) = chunkToGlobal coord lx ly
                                                      (gx', gy') = wrapGlobalU worldSize gx gy
                                                ]
                                    in ( VU.fromList (map fst pairs)
                                       , VU.fromList (map snd pairs) )
                                (timelineElev, timelineMat) =
                                    applyTimelineChunk timeline worldSize registry wsc coord
                                        (baseElevVec, baseMatVec)
                                (coastElev, coastMat) =
                                    applyCoastalTable (gtCoastal timeline)
                                        coord (timelineElev, timelineMat)
                                (despiked1, _) =
                                    removeElevationSpikes 12 4 borderSize (coastElev, coastMat)
                                carved = VU.generate borderArea $ \idx →
                                    let z = despiked1 VU.! idx
                                    in if z ≡ minBound then z
                                       else let (lx, ly) = fromIndex idx
                                                (gx, gy) = chunkToGlobal coord lx ly
                                            in z - carveAt gx gy
                                (despiked2, _) =
                                    removeElevationSpikes 12 4 borderSize (carved, coastMat)
                                lk vec gx gy =
                                    let ChunkCoord cx cy = coord
                                        lx = gx - cx * chunkSize
                                        ly = gy - cy * chunkSize
                                    in if lx ≥ negate chunkBorder ∧ lx < chunkSize + chunkBorder
                                        ∧ ly ≥ negate chunkBorder ∧ ly < chunkSize + chunkBorder
                                       then Just (vec VU.! toIndex lx ly)
                                       else Nothing
                            in [ ("base",     lk baseElevVec)
                               , ("timeline", lk timelineElev)
                               , ("coastal",  lk coastElev)
                               , ("despike1", lk despiked1)
                               , ("carved",   lk carved)
                               , ("despike2", lk despiked2)
                               ]

                        chunkA = ChunkCoord (-5) 7   -- sees row 128 as border
                        chunkB = ChunkCoord (-5) 8   -- owns row 128
                        stA = stagesFor chunkA
                        stB = stagesFor chunkB

                        diverged =
                            [ (gx, gy, nm, a, b)
                            | gy ← [124 .. 131 ∷ Int]
                            , gx ← [-82 .. -76 ∷ Int]
                            , ((nm, fA), (_, fB)) ← zip stA stB
                            , Just a ← [fA gx gy]
                            , Just b ← [fB gx gy]
                            , a ≠ b
                            ]

                    -- Every shared tile must get the SAME value from
                    -- both windows at EVERY pipeline stage. Before the
                    -- global coastal pass (save v25) the coastal stage
                    -- disagreed by up to 18z here — the muck-on-cliff
                    -- canary and the seam cliff itself.
                    diverged `shouldBe` []
