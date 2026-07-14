{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Real-engine coverage for issue #811 — Tiny(32)/Small(64) worlds
--   must no longer bypass the inland-origin river-source extension
--   (issue #221) wholesale merely because of their size, and the
--   caldera-breach/lava-blow-up regression that motivated the old
--   size gate (seed 13579 @ w32) must stay fixed.
module Test.Headless.River.InlandSources (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import Engine.Core.State (EngineEnv)
import Test.Headless.Harness
import World.Types
import World.Geology.Timeline.Helpers (isActiveRiver, getRiverParamsFromPf)
import World.Fluid.Lake.Types (Lake(..), WorldLakes(..))
import World.Geology.Hash (wrappedDeltaUV)

-- | Every currently-active river's 'RiverParams' on a generated page.
activeRivers ∷ WorldGenParams → [RiverParams]
activeRivers p =
    [ getRiverParamsFromPf pf
    | pf ← gtFeatures (wgpGeoTimeline p)
    , isActiveRiver (pfFeature pf)
    ]

-- | Chunk-distance from a river's source to the nearest ocean tile,
--   wrap-aware. Old (pre-#221, pre-#811) coastal headwaters sit right
--   where precipitation first crosses the river threshold — "typically
--   LOW, near the wet coastal mountains" per Flow.hs's own docs, i.e.
--   within a couple of chunks of the coast. A genuinely inland,
--   catchment-divide source sits materially farther out.
sourceOceanDist ∷ WorldGenParams → RiverParams → Int
sourceOceanDist p river =
    let GeoCoord sx sy = rpSourceRegion river
        cc = ChunkCoord (sx `div` chunkSize) (sy `div` chunkSize)
    in oceanDistAt (wgpOceanDist p) (wrapChunkCoordU (wgpWorldSize p) cc)

-- | Total lava-pool tile count on a generated page — the same table
--   'World.Magma.Pool.identifyLavaPools' builds at timeline-build time,
--   read directly rather than via a chunk dump.
totalLavaTiles ∷ WorldGenParams → Int
totalLavaTiles p =
    sum (map lkArea (V.toList (wlLakes (gtWorldLavaPools (wgpGeoTimeline p)))))

-- | Wrap-aware source-to-mouth span, the same per-axis Chebyshev
--   measure 'World.Geology.Timeline.RiverTrace's @pathTooLong@ cap
--   uses (@abs (mx - sx) > maxSpan ∨ abs (my - sy) > maxSpan@) —
--   computed here via 'wrappedDeltaUV' so it matches across the
--   u-axis wrap seam instead of a naive (and misleading) raw delta.
riverSpan ∷ Int → RiverParams → Int
riverSpan worldSize river =
    let GeoCoord sx sy = rpSourceRegion river
        GeoCoord mx my = rpMouthRegion river
        (dx, dy) = wrappedDeltaUV worldSize sx sy mx my
    in max (abs dx) (abs dy)

spec ∷ SpecWith EngineEnv
spec = do
    describe "Inland-origin river sources on Tiny/Small worlds (issue #811)" $ do

        -- Reuses the canonical shared worlds (Test.Headless.Harness)
        -- rather than paying for fresh WorldInits.
        it "extends at least one worldSize=32 river source materially inland" $ \env → do
            ws ← sharedWorld env 42 32 3
            Just p ← getWorldGenParams ws
            let rivers = activeRivers p
            rivers `shouldSatisfy` (not . null)
            maximum (map (sourceOceanDist p) rivers) `shouldSatisfy` (≥ 4)

        it "extends at least one worldSize=64 river source materially inland" $ \env → do
            ws ← sharedWorld env 42 64 3
            Just p ← getWorldGenParams ws
            let rivers = activeRivers p
            rivers `shouldSatisfy` (not . null)
            maximum (map (sourceOceanDist p) rivers) `shouldSatisfy` (≥ 4)

        -- Regression guard for the maxSpan/extension coupling (issue
        -- review round 1): checking only that SOME river survives
        -- tracing doesn't prove the maxSpan relaxation is what let it
        -- survive — that assertion would still pass even if the old
        -- per-size worldTiles/3 cap were silently reinstated for these
        -- sizes and simply discarded (via RiverTrace's `pathTooLong` /
        -- Reconcile's `catMaybes`) every extended source long enough to
        -- need the fix, leaving only shorter, always-legal survivors.
        -- Instead, assert a survives river's OWN span exceeds the OLD
        -- (worldSize<128) worldTiles/3 cap: a river that long could only
        -- have reached 'gtFeatures' by surviving the relaxed
        -- worldTiles/2 cap, so its mere presence proves the coupling
        -- fix — not just the source extension — is load-bearing here.
        it "has a river beyond the OLD worldSize<128 maxSpan cap for worldSize=32" $ \env → do
            ws ← sharedWorld env 42 32 3
            Just p ← getWorldGenParams ws
            let rivers = activeRivers p
                oldCap = wgpWorldSize p * chunkSize `div` 3
            rivers `shouldSatisfy` (not . null)
            maximum (map (riverSpan (wgpWorldSize p)) rivers)
                `shouldSatisfy` (> oldCap)

        it "has a river beyond the OLD worldSize<128 maxSpan cap for worldSize=64" $ \env → do
            ws ← sharedWorld env 42 64 3
            Just p ← getWorldGenParams ws
            let rivers = activeRivers p
                oldCap = wgpWorldSize p * chunkSize `div` 3
            rivers `shouldSatisfy` (not . null)
            maximum (map (riverSpan (wgpWorldSize p)) rivers)
                `shouldSatisfy` (> oldCap)

        it "keeps every extended river's traced course non-degenerate (survives tracing)" $ \env → do
            -- Every returned river reaching 'gtFeatures' at all implies
            -- a successful trace (a failed trace never becomes a
            -- PersistentFeature — Reconcile.hs's catMaybes drops it
            -- first), but a degenerate one (no real segments) would
            -- still slip through a bare non-null check.
            ws ← sharedWorld env 42 64 3
            Just p ← getWorldGenParams ws
            let rivers = activeRivers p
            rivers `shouldSatisfy` (not . null)
            map (V.length . rpSegments) rivers `shouldSatisfy` all (> 0)

    describe "seed 13579 w32 caldera/lava regression (issue #811 / #221)" $ do

        it "does not breach a caldera / explode lava once sources extend" $ \env → do
            -- Not a memoized/shared world (only 42/32/3 and 42/64/3 are)
            -- — this seed legitimately needs its own WorldInit.
            let pid = WorldPageId "issue811_seed13579_w32"
            sendWorldCommand env (WorldInit pid 13579 32 3 Nothing)
            ws ← waitForWorldInit env pid 120
            Just p ← getWorldGenParams ws
            -- Measured baselines for this exact seed/size (whole-world
            -- gtWorldLavaPools tile sum, not a rendered/composited
            -- count): master (pre-#811, extension disabled for w32) is
            -- 410; this fix's caldera-guarded extension is 602 — a
            -- real but modest shift from different rivers being
            -- selected, nowhere near the "100s of new lava tiles" a
            -- single breached caldera produced in the pre-size-gate
            -- #221 prototype this issue guards against. Threshold
            -- gives headroom above the measured value while still
            -- catching a regression back to an unguarded breach.
            totalLavaTiles p `shouldSatisfy` (< 1000)
            -- The extension is still active on this world (not silently
            -- reverted to the old short-coastal behaviour to dodge the
            -- regression).
            let rivers = activeRivers p
            rivers `shouldSatisfy` (not . null)
