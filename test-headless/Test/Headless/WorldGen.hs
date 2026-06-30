module Test.Headless.WorldGen (spec) where

-- | Basic worldgen smoke tests. Read-only assertions run against the
--   shared canonical world (see 'sharedWorld' — worldgen is the whole
--   cost of this suite, so specs that can share a generation do).
--   The determinism and destroy tests need worlds of their own; they
--   use size 32 (cheapest gen that still runs the full pipeline).

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as Cereal
import Control.Concurrent (threadDelay)
import Engine.Core.State (EngineEnv)
import Test.Headless.Harness
import World.Generate.Config
    ( WorldGenConfig(..)
    , applyConfigToParams
    , defaultWorldGenConfig
    , minimumWorldSize
    , normalizePlateCount
    , normalizeWorldSize
    )
import World.Plate (generatePlates)
import World.Types
import qualified Data.Vector as V
import World.Ocean.Types (oceanDistAt)
import World.Fluid.Lake.Types (lakesInChunk)
import World.Fluid.River.Types (riversInChunk)
import Location.Types (LocationDef(..))
import Location.Overlay (computeLocationOverlay, chunkMetricsAt, ChunkMetrics(..))

spec ∷ SpecWith EngineEnv
spec = do

    describe "Basic terrain generation" $ do

        it "generates a small world with chunks" $ \env → do
            ws ← sharedWorld env 42 64 3
            tiles ← getWorldTileData ws
            let numChunks = HM.size (wtdChunks tiles)
            numChunks `shouldSatisfy` (> 0)

        it "stores generation params after init" $ \env → do
            ws ← sharedWorld env 42 64 3
            mParams ← getWorldGenParams ws
            mParams `shouldSatisfy` isJust
            case mParams of
                Just params → do
                    wgpSeed params `shouldBe` 42
                    wgpWorldSize params `shouldBe` 64
                Nothing → expectationFailure "params should exist"

        it "generates tectonic plates" $ \env → do
            ws ← sharedWorld env 42 64 3
            mParams ← getWorldGenParams ws
            case mParams of
                Just params →
                    length (wgpPlates params) `shouldSatisfy` (> 0)
                Nothing → expectationFailure "params should exist"

        it "keeps plate centers inside canonical world tile bounds" $ \_env → do
            let cases = [ (seed, worldSize)
                        | seed ← [0 .. 128]
                        , worldSize ← [32, 64, 128]
                        ]
            forM_ cases $ \(seed, worldSize) → do
                let halfTiles = (worldSize * chunkSize) `div` 2
                    inBounds plate =
                           plateCenterX plate ≥ negate halfTiles
                        ∧ plateCenterX plate < halfTiles
                        ∧ plateCenterY plate ≥ negate halfTiles
                        ∧ plateCenterY plate < halfTiles
                forM_ (generatePlates (fromIntegral seed) worldSize 8) $ \plate →
                    plate `shouldSatisfy` inBounds

    describe "Worldgen input normalization" $ do
        it "snaps world size to a minimum region multiple" $ \_env → do
            let m = minimumWorldSize
            map normalizeWorldSize [negate m, 0, 1, m - 1, m, m + 1, 2 * m - 1, 2 * m]
                `shouldBe` [m, m, m, m, m, 2 * m, 2 * m, 2 * m]

        it "snaps plate count to at least 1" $ \_env →
            map normalizePlateCount [-3, 0, 1, 8] `shouldBe` [1, 1, 1, 8]

        it "normalizes config-derived worldgen params" $ \_env → do
            let params = applyConfigToParams defaultWorldGenConfig
                    { wgcWorldSize = 3
                    , wgcPlateCount = 0
                    }
            wgpWorldSize params `shouldBe` minimumWorldSize
            wgpPlateCount params `shouldBe` 1

    describe "Determinism" $ do

        -- Two independent inits of the same params, comparing the
        -- full per-chunk surface maps — a stronger signal than the
        -- old plate-only comparison (plates are a pure function of
        -- the seed; the surface maps exercise the whole pipeline).
        -- Size 32 keeps the double generation cheap.
        it "same seed produces identical chunk surface maps" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "det1") 123 32 3)
            ws1 ← waitForWorldInit env (WorldPageId "det1") 120
            sendWorldCommand env (WorldInit (WorldPageId "det2") 123 32 3)
            ws2 ← waitForWorldInit env (WorldPageId "det2") 120

            t1 ← getWorldTileData ws1
            t2 ← getWorldTileData ws2
            HM.size (wtdChunks t1) `shouldSatisfy` (> 0)
            HM.map lcSurfaceMap (wtdChunks t1)
                `shouldBe` HM.map lcSurfaceMap (wtdChunks t2)

    describe "World lifecycle" $ do

        -- Private world: this test destroys its page, so it must not
        -- touch the shared one.
        it "can destroy a world" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "destroy") 42 32 3)
            _ ← waitForWorldInit env (WorldPageId "destroy") 120
            sendWorldCommand env (WorldDestroy (WorldPageId "destroy"))
            threadDelay 500000
            mWs ← getWorldState env (WorldPageId "destroy")
            isNothing mWs `shouldBe` True

    describe "Location overlay (#89)" $ do

        -- The headless harness boots no Lua, so the location registry is
        -- empty and the stored overlay is empty — these specs exercise
        -- the pure placement pass directly against the shared world's
        -- real plates / ocean data, with synthetic defs. The full
        -- load-defs → init → listPlaced integration lives in the python
        -- probe (tools/location_overlay_probe.py).
        let mkDef lid anchors = LocationDef
                { ldId = lid, ldLabel = lid, ldType = "test"
                , ldBuilder = "noop", ldAnchor = anchors
                , ldMaxCount = 8, ldMinSpacing = 3, ldContents = [] }
            flatDef = mkDef "flat_test"     ["flat"]
            mtnDef  = mkDef "mountain_test" ["mountain"]
            overlayFor p defs = computeLocationOverlay
                (wgpSeed p) (wgpWorldSize p) (wgpPlates p)
                (wgpOceanMap p) (wgpOceanDist p)
                (gtWorldLakes (wgpGeoTimeline p)) (gtWorldRivers (wgpGeoTimeline p))
                defs

        it "world init wires a serializable overlay field" $ \env → do
            ws ← sharedWorld env 42 64 3
            mp ← getWorldGenParams ws
            case mp of
                Just p  → HM.size (wgpLocationOverlay p) `shouldSatisfy` (≥ 0)
                Nothing → expectationFailure "params should exist"

        it "places flat-anchored locations on land" $ \env → do
            ws ← sharedWorld env 42 64 3
            Just p ← getWorldGenParams ws
            HM.size (overlayFor p [flatDef]) `shouldSatisfy` (> 0)

        it "is deterministic — same seed yields the same overlay" $ \env → do
            ws ← sharedWorld env 42 64 3
            Just p ← getWorldGenParams ws
            -- recompute the plates independently from the seed: a fresh
            -- plate list with the same seed must give the same overlay.
            let plates2 = generatePlates (wgpSeed p) (wgpWorldSize p) (wgpPlateCount p)
                ov2 = computeLocationOverlay (wgpSeed p) (wgpWorldSize p) plates2
                                             (wgpOceanMap p) (wgpOceanDist p)
                                             (gtWorldLakes (wgpGeoTimeline p))
                                             (gtWorldRivers (wgpGeoTimeline p))
                                             [flatDef, mtnDef]
            ov2 `shouldBe` overlayFor p [flatDef, mtnDef]

        it "never places a location on an ocean chunk" $ \env → do
            ws ← sharedWorld env 42 64 3
            Just p ← getWorldGenParams ws
            HM.keys (overlayFor p [flatDef, mtnDef])
                `shouldSatisfy` all (\c → not (HS.member c (wgpOceanMap p)))

        it "keeps locations clear of lakes, rivers, and the ocean shore (#414)" $ \env → do
            ws ← sharedWorld env 42 64 3
            Just p ← getWorldGenParams ws
            let lakes  = gtWorldLakes  (wgpGeoTimeline p)
                rivers = gtWorldRivers (wgpGeoTimeline p)
                dry coord@(ChunkCoord cx cy) =
                    oceanDistAt (wgpOceanDist p) coord ≥ 2
                    ∧ all (\c → V.null (lakesInChunk lakes c)
                              ∧ V.null (riversInChunk rivers c))
                          [ ChunkCoord (cx + dx) (cy + dy)
                          | dx ← [-1, 0, 1], dy ← [-1, 0, 1] ]
            HM.keys (overlayFor p [flatDef, mtnDef]) `shouldSatisfy` all dry

        it "respects anchor tags — mountain picks higher ground than flat" $ \env → do
            ws ← sharedWorld env 42 64 3
            Just p ← getWorldGenParams ws
            let med c = cmMedianElev
                    (chunkMetricsAt (wgpSeed p) (wgpPlates p) (wgpWorldSize p)
                                    (wgpOceanDist p) c)
                mtn  = HM.keys (overlayFor p [mtnDef])
                flat = HM.keys (overlayFor p [flatDef])
                avg xs = sum xs `div` max 1 (length xs)
            mtn  `shouldSatisfy` (not . null)
            flat `shouldSatisfy` (not . null)
            avg (map med mtn) `shouldSatisfy` (> avg (map med flat))

        it "overlay survives a WorldGenParams serialize round-trip" $ \_env → do
            let sample = HM.fromList [ (ChunkCoord 1 2, "ruin_small" ∷ Text)
                                     , (ChunkCoord (-3) 4, "camp") ]
                p = defaultWorldGenParams { wgpLocationOverlay = sample }
                back = Cereal.decode (Cereal.encode p) ∷ Either String WorldGenParams
            fmap wgpLocationOverlay back `shouldBe` Right sample
