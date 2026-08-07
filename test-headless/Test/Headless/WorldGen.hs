module Test.Headless.WorldGen (spec) where

-- | Basic worldgen smoke tests. Read-only assertions run against the
--   shared canonical world (see 'sharedWorld' — worldgen is the whole
--   cost of this suite, so specs that can share a generation do).
--   The determinism and destroy tests need worlds of their own; they
--   use size 32 (cheapest gen that still runs the full pipeline).

import UPrelude
import Test.Hspec
import Data.List (isInfixOf, sortOn)
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
import World.Fluid.Lake.Types (lakesInChunk)
import World.Fluid.River.Types (riversInChunk)
import Location.Types
    ( LocationDef(..), LocationNaming(..), emptyLocationRegistry
    , registerLocation )
import Location.Instance
    ( LocationInstance(..), LocationLifecycle(..)
    , buildLocationInstances, instancesToList )
import Location.Bounds (RelBounds(..), translateBounds)
import Location.Overlay
    ( computeLocationOverlay, computeLocationPlacement, LocationPlacement(..)
    , PlacementOutcome(..), chunkMetricsAt, ChunkMetrics(..) )
import Test.Headless.Location.Bounds (decodeDef, rejectedNaming, isRight')
import Language.Semantic.Types (ConceptId(..))

-- | The naming scheme every 'LocationDef' fixture in this module
--   carries (#1101). One concept per pool is enough: these specs are
--   about geometry, lifecycle, and identity, and every one of them
--   builds instances with NO namer, so the pools are never drawn from.
testNaming ∷ LocationNaming
testNaming = LocationNaming
    { lnHeads     = [ConceptId "KEEP"]
    , lnModifiers = [ConceptId "ASH"]
    }

-- chunkSeamChebyshev comes in via World.Types (World.Chunk.Types, #423)

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
            sendWorldCommand env (WorldInit (WorldPageId "det1") 123 32 3 Nothing)
            ws1 ← waitForWorldInit env (WorldPageId "det1") 120
            sendWorldCommand env (WorldInit (WorldPageId "det2") 123 32 3 Nothing)
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
            sendWorldCommand env (WorldInit (WorldPageId "destroy") 42 32 3 Nothing)
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
                , ldMaxCount = 8, ldMinSpacing = 3, ldContents = []
                , ldBounds = RelBounds (-2) (-2) 2 2, ldDiscoveryMargin = 6
                , ldMapIcons = Nothing, ldNaming = testNaming }
            flatDef = mkDef "flat_test"     ["flat"]
            mtnDef  = mkDef "mountain_test" ["mountain"]
            -- A chunk's anchor tile — mirrors Location.Instance's
            -- 'locationAnchorTile', spelled out here so the expectation
            -- is independent of the code under test.
            chunkCentre (ChunkCoord cx cy) =
                ( cx * chunkSize + chunkSize `div` 2
                , cy * chunkSize + chunkSize `div` 2 )
            overlayFor p defs = computeLocationOverlay
                (wgpSeed p) (wgpWorldSize p) (wgpPlates p)
                (wgpOceanMap p) (wgpOceanDist p)
                (gtWorldLakes (wgpGeoTimeline p)) (gtWorldRivers (wgpGeoTimeline p))
                defs
            placementFor p defs = computeLocationPlacement
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
                wrap   = wrapChunkCoordU (wgpWorldSize p)
                dry coord@(ChunkCoord cx cy) =
                    oceanDistAt (wgpOceanDist p) (wrap coord) ≥ 2
                    ∧ all (\c → let cc = wrap c
                                in V.null (lakesInChunk lakes cc)
                                 ∧ V.null (riversInChunk rivers cc))
                          [ ChunkCoord (cx + dx) (cy + dy)
                          | dx ← [-1, 0, 1], dy ← [-1, 0, 1] ]
            -- Scoped to the STRICT pass on purpose (#997): the #414
            -- filter is what the guarantee is allowed to violate, so
            -- asserting dryness over every overlay key would become
            -- self-contradictory the moment the guarantee fires. Pinning
            -- the outcome first keeps this a statement about the strict
            -- pass — and fails loudly if this world ever stops placing.
            lpOutcome (placementFor p [flatDef, mtnDef]) `shouldBe` PlacedStrict
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

        it "geometry-stamp flag survives a WorldGenParams serialize round-trip (#424)" $ \_env → do
            let sample = HS.fromList [ChunkCoord 1 2, ChunkCoord (-3) 4]
                p = defaultWorldGenParams { wgpLocationStamped = sample }
                back = Cereal.decode (Cereal.encode p) ∷ Either String WorldGenParams
            fmap wgpLocationStamped back `shouldBe` Right sample

        it "geometry-stamp flag is independent of the content-spawn flag (#424)" $ \_env → do
            -- The chunk-keyed stamp flag and the per-instance content-spawn
            -- flag (#911) must not alias: a chunk marked stamped is not
            -- thereby marked content-spawned, and vice versa.
            let coord = ChunkCoord 5 (-2)
                overlay = HM.singleton coord ("flat_test" ∷ Text)
                registry = registerLocation flatDef emptyLocationRegistry
                p = defaultWorldGenParams
                        { wgpLocationStamped = HS.singleton coord
                        , wgpLocationInstances =
                            buildLocationInstances Nothing registry overlay
                        }
            HS.member coord (wgpLocationStamped p) `shouldBe` True
            map liContentsSpawned
                (instancesToList (wgpLocationInstances p)) `shouldBe` [False]

        it "chunkSeamChebyshev measures across the U seam (#422)" $ \_env → do
            -- worldSize 8 → halfW 4, canonical u = cx − cy ∈ [−4, 4).
            -- (2, −1) and (−2, 3) are u-alias images of ONE physical chunk.
            chunkSeamChebyshev 8 (ChunkCoord 2 (-1)) (ChunkCoord (-2) 3)
                `shouldBe` 0
            -- Physically adjacent across the seam; raw Chebyshev says 4.
            chunkSeamChebyshev 8 (ChunkCoord 2 (-1)) (ChunkCoord (-2) 2)
                `shouldBe` 1
            -- Interior pairs keep the raw distance.
            chunkSeamChebyshev 8 (ChunkCoord 0 0) (ChunkCoord 2 1)
                `shouldBe` 2
            -- Non-wrapping (arena / zero-size) world: raw distance.
            chunkSeamChebyshev 0 (ChunkCoord 2 (-1)) (ChunkCoord (-2) 3)
                `shouldBe` 4

        it "enforces minSpacing across the U seam and never places aliases (#422)" $ \_env → do
            -- Pure pass at worldSize 8 over water-free synthetic tables;
            -- [] anchors leave every land chunk a candidate, so the
            -- placements crowd the seam. Pre-#422 nearly every seed here
            -- placed a same-def pair whose alias images touch — several
            -- (e.g. seed 6) placed the SAME physical chunk twice, once
            -- under its canonical coord and once under its u-alias.
            let ws = 8
                def = mkDef "seam_test" []
                noLakes  = gtWorldLakes  emptyTimeline
                noRivers = gtWorldRivers emptyTimeline
                placedFor seed =
                    let plates = generatePlates seed ws 3
                    in HM.keys (computeLocationOverlay seed ws plates
                                    HS.empty HM.empty noLakes noRivers [def])
                allPlaced = map placedFor [0 .. 15]
            -- The scenario must actually exercise placement (a mostly
            -- submerged seed may legitimately place nothing).
            sum (map length allPlaced) `shouldSatisfy` (≥ 8)
            forM_ allPlaced $ \placed → do
                forM_ placed $ \c → wrapChunkCoordU ws c `shouldBe` c
                forM_ [ (a, b) | (i, a) ← zip [0 ∷ Int ..] placed
                               , (j, b) ← zip [0 ∷ Int ..] placed
                               , i < j ] $ \(a, b) →
                    chunkSeamChebyshev ws a b
                        `shouldSatisfy` (≥ ldMinSpacing def)

        -- #997: the strict pass can reject EVERY land chunk, leaving a
        -- generated world with no locations at all and the expedition
        -- arc unplayable on that save. These run on synthetic tables at
        -- worldSize 8 (the pattern the #422 seam spec above uses) so
        -- they stay in the always-blocking tier without a real w128 /
        -- w256 generation; the large-tuple end of the matrix lives in
        -- tools/location_overlay_probe.py.
        describe "guaranteed placement (#997)" $ do
            let gws      = 8
                gseed    = 3 ∷ Word64
                gplates  = generatePlates gseed gws 3
                noLakes  = gtWorldLakes  emptyTimeline
                noRivers = gtWorldRivers emptyTimeline
                gcoords  = [ ChunkCoord cx cy
                           | cx ← [-(gws `div` 2) .. gws `div` 2 - 1]
                           , cy ← [-(gws `div` 2) .. gws `div` 2 - 1] ]
                -- Every chunk one hop from the ocean, so the #414
                -- dryEnough filter rejects the whole world — the exact
                -- filter-exhaustion route that produces the reported
                -- zero-placement world.
                allWet   = HM.fromList [ (c, 1 ∷ Int) | c ← gcoords ]
                -- No ocean anywhere: `oceanDistAt` defaults to maxBound
                -- for a missing key, so every land chunk reads as dry.
                allDry   = HM.empty ∷ HM.HashMap ChunkCoord Int
                placeWith oceanMap oceanDist defs =
                    computeLocationPlacement gseed gws gplates
                        oceanMap oceanDist noLakes noRivers defs
                wetPlacement = placeWith HS.empty allWet [flatDef]
                -- The land oracle: with every chunk dry and an
                -- anchor-free, densely-placing def, the STRICT pass
                -- itself enumerates the world's land chunks.
                denseDef = (mkDef "dense_test" [])
                    { ldMaxCount = 100000, ldMinSpacing = 1 }
                landChunks = HM.keys (lpOverlay (placeWith HS.empty allDry [denseDef]))

            it "the fixture really is a zero-placement world for the strict pass" $ \_env → do
                -- Guards the rest of the group: `PlacedGuaranteed` can
                -- only be reached from an empty strict result, so this
                -- pins the reproducer itself, and `landChunks` proves
                -- the world is not merely landless.
                landChunks `shouldSatisfy` (not . null)
                lpOutcome wetPlacement `shouldBe` PlacedGuaranteed

            it "guarantees a location in a land world the strict pass rejects" $ \_env →
                -- The pre-change assertion: `computeLocationOverlay` on
                -- this fixture returned an EMPTY overlay before #997.
                HM.size (lpOverlay wetPlacement) `shouldBe` 1

            it "is deterministic — two evaluations of the fixture agree" $ \_env → do
                let again = placeWith HS.empty allWet [flatDef]
                lpOverlay again `shouldBe` lpOverlay wetPlacement
                lpOutcome again `shouldBe` lpOutcome wetPlacement
                -- and again from independently regenerated plates, the
                -- same way the strict determinism spec above re-derives.
                let plates2 = generatePlates gseed gws 3
                    third = computeLocationPlacement gseed gws plates2
                                HS.empty allWet noLakes noRivers [flatDef]
                lpOverlay third `shouldBe` lpOverlay wetPlacement

            it "places on real, canonical land — never ocean or an alias coord" $ \_env → do
                let placed = HM.keys (lpOverlay wetPlacement)
                placed `shouldSatisfy` all (`elem` landChunks)
                forM_ placed $ \c → wrapChunkCoordU gws c `shouldBe` c

            it "names a registered definition, and only the first in id order" $ \_env → do
                -- `defsSorted` is `sortOn ldId`, so "aaa_test" wins over
                -- "flat_test" regardless of the argument order.
                let firstDef = mkDef "aaa_test" ["flat"]
                    ov = lpOverlay (placeWith HS.empty allWet [flatDef, firstDef])
                HM.elems ov `shouldBe` ["aaa_test"]

            it "prefers a dry chunk over the score order when one exists" $ \_env → do
                -- The world stays wet except for ONE land chunk given a
                -- real ocean distance. That chunk is the world's lowest
                -- ground, so a [mountain] def still can't anchor there
                -- and the strict pass still rejects everything — the
                -- guarantee has to break the anchor either way. Which
                -- chunk it breaks it ON is the point: the dry one, not
                -- the one the score order alone would reach.
                let mtnOnly = mkDef "mtn_only" ["mountain"]
                    med c = cmMedianElev (chunkMetricsAt gseed gplates gws allDry c)
                    wetOnly = placeWith HS.empty allWet [mtnOnly]
                    scorePick = HM.keys (lpOverlay wetOnly)
                    -- Excluding the pure-score pick, so the assertion
                    -- can only pass if dryness outranked the score.
                    lowFirst = filter (\c → [c] ≠ scorePick) (sortOn med landChunks)
                lpOutcome wetOnly `shouldBe` PlacedGuaranteed
                case lowFirst of
                    [] → expectationFailure "fixture has no alternative land chunk"
                    dryOne : _ → do
                        let res = placeWith HS.empty
                                    (HM.insert dryOne 5 allWet) [mtnOnly]
                        lpOutcome res `shouldBe` PlacedGuaranteed
                        HM.keys (lpOverlay res) `shouldBe` [dryOne]

            it "does not fire when no definitions are registered" $ \_env → do
                -- Load-bearing for the tracked world-gen baselines: the
                -- headless dump path registers no location YAML, so a
                -- def-less world must still produce an empty overlay.
                let res = placeWith HS.empty allWet []
                lpOverlay res `shouldBe` HM.empty
                lpOutcome res `shouldBe` NoPlaceableDefinitions

            it "does not resurrect a definition authored max_count 0" $ \_env → do
                -- An authored "do not place" is not a generation failure.
                let res = placeWith HS.empty allWet [flatDef { ldMaxCount = 0 }]
                lpOverlay res `shouldBe` HM.empty
                lpOutcome res `shouldBe` NoPlaceableDefinitions

            it "reports an explicit no-location result for a landless world" $ \_env → do
                -- Every chunk submerged: no placement of any kind is
                -- possible, and that is what the caller is told.
                let allOcean = HS.fromList gcoords
                    res = placeWith allOcean allDry [flatDef]
                lpOverlay res `shouldBe` HM.empty
                lpOutcome res `shouldBe` NoLand

            it "leaves a successful strict pass exactly as it was" $ \env → do
                ws ← sharedWorld env 42 64 3
                Just p ← getWorldGenParams ws
                let res = placementFor p [flatDef, mtnDef]
                lpOutcome res `shouldBe` PlacedStrict
                -- More than the single entry the guarantee would add,
                -- and byte-identical to the overlay-only entry point.
                HM.size (lpOverlay res) `shouldSatisfy` (> 1)
                lpOverlay res `shouldBe` overlayFor p [flatDef, mtnDef]

            it "builds a complete instance at the guaranteed chunk" $ \_env → do
                -- The guaranteed entry is an ordinary overlay entry: it
                -- flows through the SAME `buildLocationInstances` every
                -- strict placement does, so it acquires an id, an
                -- anchor, resolved bounds, a discovery margin and save
                -- coverage with no parallel construction path.
                let registry = registerLocation flatDef emptyLocationRegistry
                    insts = instancesToList
                                (buildLocationInstances Nothing registry (lpOverlay wetPlacement))
                case insts of
                    [i] → do
                        liDefId i `shouldBe` "flat_test"
                        [liChunk i] `shouldBe` HM.keys (lpOverlay wetPlacement)
                        liAnchor i `shouldBe` chunkCentre (liChunk i)
                        liBounds i `shouldBe` translateBounds (liAnchor i) (ldBounds flatDef)
                        liDiscoveryMargin i `shouldBe` ldDiscoveryMargin flatDef
                        liDisplayName i `shouldBe` ldLabel flatDef
                        liLifecycle i `shouldBe` LifecycleUnknown
                        liContentsSpawned i `shouldBe` False
                    _ → expectationFailure
                            ("expected exactly one instance, got " <> show (length insts))

            it "constructs a valid-looking location at a requested chunk coordinate" $ \_env → do
                -- Requirement 3's direct construction check, independent
                -- of any generated world: hand an arbitrary chunk to the
                -- same constructor the placement pass uses.
                let coord = ChunkCoord 5 (-3)
                    registry = registerLocation flatDef emptyLocationRegistry
                    insts = instancesToList
                                (buildLocationInstances Nothing registry
                                    (HM.singleton coord ("flat_test" ∷ Text)))
                map liChunk insts `shouldBe` [coord]
                map liAnchor insts `shouldBe` [chunkCentre coord]
                map liBounds insts
                    `shouldBe` [translateBounds (chunkCentre coord) (ldBounds flatDef)]

        -- #801: an unsupported or misspelled anchor tag must not silently
        -- impose no constraint. Validation lives at the YAML load layer
        -- ('Engine.Asset.YamlLocations'), not in 'anchorOk' above, so
        -- these decode a def straight from a YAML fragment rather than
        -- going through 'computeLocationOverlay'. Nested here (rather
        -- than alongside the #777 YAML tests in
        -- Test.Headless.Location.Bounds) so `--match="Location overlay"`
        -- runs them.
        describe "anchor vocabulary (#801)" $ do
            let anchorDef anchorYaml =
                    "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] }, discovery_margin: 6,\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \  anchor: " <> anchorYaml <> " }"

            it "accepts every tag in the supported vocabulary" $ \_env →
                forM_ [ "flat", "mountain", "highland", "lowland"
                      , "coast", "coastal", "inland", "waterside" ] $ \tag →
                    decodeDef (anchorDef ("[" <> tag <> "]")) `shouldSatisfy` isRight'

            it "accepts a definition with no anchor tags at all" $ \_env →
                decodeDef (anchorDef "[]") `shouldSatisfy` isRight'

            it "rejects an unknown anchor tag, naming the definition" $ \_env →
                decodeDef (anchorDef "[jungle]") `shouldSatisfy` rejectedNaming "t"

            it "rejects a misspelled anchor tag, naming the definition" $ \_env →
                decodeDef (anchorDef "[mountian]") `shouldSatisfy` rejectedNaming "t"

            it "rejects an unsupported climate/biome anchor tag" $ \_env →
                decodeDef (anchorDef "[tundra]") `shouldSatisfy` rejectedNaming "t"

            it "rejects a list mixing valid and invalid anchor tags" $ \_env →
                decodeDef (anchorDef "[flat, jungle]") `shouldSatisfy` rejectedNaming "t"

            it "names the offending tag itself, not just the definition" $ \_env →
                case decodeDef (anchorDef "[jungle]") of
                    Left err → err `shouldSatisfy` ("jungle" `isInfixOf`)
                    Right _  → expectationFailure "expected a decode failure"
