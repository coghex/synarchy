{-# LANGUAGE Strict #-}
-- | One coherent snapshot (#2714, D-51): the examples ACT AS the barrier
--   owners the headless harness never starts, the way
--   @Test.Headless.Save.Barrier@ and @OwnerPark@ do, to put a mutation that
--   crosses owners on either side of the snapshot boundary deterministically
--   — by withholding a stand-in owner's acknowledgement, never by sleeping —
--   and to show that a failure raised inside the capture window releases
--   every owner and the line, and that nothing written after the bytes were
--   fixed can reach them.
module Test.Headless.Scenario.Capture.Coherence (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.MVar
import Data.IORef (atomicModifyIORef', readIORef)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Engine.Core.Queue as Q
import Building.Types
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status (captureInProgress)
import Engine.Save.Barrier
import Item.Types (ItemInstance(..))
import Scenario.Capture.Types
import Scenario.Load.Session (arenaPageId)
import Scenario.Types
import Test.Headless.Scenario.Capture.Support
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import Unit.Types
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Types (ChunkCoord, LoadedChunk(..), columnIndex)
import World.Command.Types
import World.Fluid.Types (FluidCell, FluidType(..), fluidCellAtZ)
import World.Generate.Coordinates (globalToChunk)
import World.State.Types
import World.Tile.Types (lookupChunk)

-- | A unit carrying a tagged tool, and an empty crate to move it into.
transferDoc ∷ [String]
transferDoc =
    [ "map: {width: 20, height: 20}"
    , "units:"
    , "  - id: carrier"
    , "    definition: acolyte"
    , "    x: 1"
    , "    y: 1"
    , "    inventory: [{id: tool, definition: steel_bar, tags: [gear]}]"
    , "buildings:"
    , "  - {id: crate, definition: cargo_hold_S, x: 6, y: 6, storage: []}" ]

-- | The owners the harness stands in for (the world thread is a real one),
--   whose ticks the examples count.
captureOwners ∷ [SaveOwner]
captureOwners = [SaveUnit, SaveBuilding, SaveCombat, SaveSimulation]

-- | Wait (bounded) until the barrier is waiting on EXACTLY the owners named:
--   every other owner has acknowledged and nobody has reached the boundary.
waitingOn ∷ H → [SaveOwner] → IO ()
waitingOn h missing = waitCondition 30 $ do
    cur ← readSaveStatus (slSaveBarrierRef (toSaveLoadCapability (fxEnv (hFx h))))
    pure $ case cur of
        Just s → ssOutcome s ≡ Nothing ∧ ssPhase s ≢ SaveSnapshotBoundary
                 ∧ Set.fromList missing ≡ ssOwners s `Set.difference` ssAcknowledged s
        Nothing → False

spec ∷ SpecWith H
spec = describe "one coherent snapshot" $ do

    it "waits for an owner mid-mutation: an item transfer across two owners is captured whole, tag and all" $ \h → do
        loadDoc h "capture-coh-transfer.yaml" transferDoc
        dir ← freshDir "coh-transfer"
        -- the building owner has not acknowledged: the barrier cannot reach
        -- the boundary however many times the other owners drain
        holdOwner (hOwners h) SaveBuilding True
        withPump h $ do
            n ← requestCapture h (dir </> "transfer.yaml") False
            waitingOn h [SaveBuilding]
            -- the transfer, as the owners would perform it: the unit owner
            -- takes the tool out of the carrier ...
            Just tool ← takeTool h
            -- ... and for a moment the tool is nowhere at all: a capture taken
            -- now would be the incoherent state this protocol exists to prevent
            carried ← carriedTools h
            stored ← storedTools h
            (length carried, length stored) `shouldBe` (0, 0)
            fmap cstStage <$> captureStatusOf h `shouldReturn` Just CaptureCoordinating
            -- ... then the building owner takes it in, and acknowledges
            putTool h tool
            holdOwner (hOwners h) SaveBuilding False
            s ← awaitCapture h n 60
            void (captureOf s)
        sc ← decodeFile h (dir </> "transfer.yaml")
        -- exactly one tool, in the crate, with its identity and its tag, and
        -- none in the carrier
        [carrier] ← pure [ u | u ← scUnits sc, ueId u ≡ ExplicitId "carrier" ]
        fmap length (authored (ueInventory carrier)) `shouldBe` Just 0
        [crate] ← pure [ b | b ← scBuildings sc, beId b ≡ ExplicitId "crate" ]
        [inCrate] ← pure (fromMaybe [] (authored (beStorage crate)))
        (ieId inCrate, ieDefinition inCrate, ieTags inCrate) `shouldBe` (ExplicitId "tool", "steel_bar", [tag "gear"])

    it "captures a simulation writeback that was in flight when the capture was requested" $ \h → do
        loadDoc h "capture-coh-fluid.yaml" ["map: {width: 20, height: 20}"]
        let env = fxEnv (hFx h)
        dir ← freshDir "coh-fluid"
        ws ← liveArena h
        holdOwner (hOwners h) SaveSimulation True
        withPump h $ do
            n ← requestCapture h (dir </> "fluid.yaml") False
            waitingOn h [SaveSimulation]
            -- the simulation's output, still queued for the world thread: a wet
            -- cell the capture must not miss
            let cell = fluidCellAtZ Lake 5
                (coord, (lx, ly)) = globalToChunk 3 3
            wb ← writebackWith ws coord (columnIndex lx ly) (Just cell)
            epoch ← pageIncarnation ws
            Q.writeQueue (wsWorldQueue (toWorldSimCapability env))
                (WorldApplyFluids (FluidWritebackBatch arenaPageId (Just epoch) [wb] [] Nothing))
            -- applied by the world thread while the barrier is still waiting
            waitCondition 30 $ do
                td ← readIORef (wsTilesRef ws)
                pure (maybe False (\lc → lcFluidMap lc V.! columnIndex lx ly ≡ Just cell) (lookupChunk coord td))
            holdOwner (hOwners h) SaveSimulation False
            s ← awaitCapture h n 60
            void (captureOf s)
        sc ← decodeFile h (dir </> "fluid.yaml")
        map (\f → (fpKind f, fpSurfaceEighths f, fpRegion f)) (scFluids sc)
            `shouldBe` [(FluidLake, 40, RegionRect 3 3 3 3)]

    it "parks every owner from its final acknowledgement to the release: nothing mutates the captured state" $ \h → do
        loadDoc h "capture-coh-park.yaml" transferDoc
        dir ← freshDir "coh-park"
        hold ← newHold
        armCaptureHold h CpBoundary hold
        withPump h $ do
            n ← requestCapture h (dir </> "park.yaml") False
            takeMVar (holdReached hold)
            -- inside the capture window: every owner is alive (it keeps
            -- ticking) and parked (it mutates nothing)
            before ← forM captureOwners (ownerCounts (hOwners h))
            waitCondition 30 $ do
                now ← forM captureOwners (ownerCounts (hOwners h))
                pure (and [ pNow ≥ pBefore + 20 | ((_, pBefore), (_, pNow)) ← zip before now ])
            after ← forM captureOwners (ownerCounts (hOwners h))
            map fst after `shouldBe` map fst before
            putMVar (holdRelease hold) ()
            s ← awaitCapture h n 60
            disarmCapture h
            void (captureOf s)
            -- released: they run again
            mutated ← forM captureOwners (ownerCounts (hOwners h))
            waitCondition 30 $ do
                now ← forM captureOwners (ownerCounts (hOwners h))
                pure (and [ mNow > mBefore | ((mBefore, _), (mNow, _)) ← zip mutated now ])

    it "keeps a write made after the bytes were fixed out of the file" $ \h → do
        loadDoc h "capture-coh-late.yaml" transferDoc
        dir ← freshDir "coh-late"
        hold ← newHold
        -- held after the capture lock was released: the owners run again while
        -- the immutable bytes wait to be written
        armCaptureHold h CpReleased hold
        withPump h $ do
            n ← requestCapture h (dir </> "late.yaml") False
            takeMVar (holdReached hold)
            editUnit h "carrier" $ \u → u { uiStats = HM.insert "hydration" 1.0 (uiStats u), uiName = "Renamed" }
            putMVar (holdRelease hold) ()
            s ← awaitCapture h n 60
            disarmCapture h
            void (captureOf s)
        sc ← decodeFile h (dir </> "late.yaml")
        [carrier] ← pure (scUnits sc)
        HM.lookup "hydration" (ueStats carrier) `shouldNotBe` Just 1.0
        ueName carrier `shouldNotBe` Authored "Renamed"
        -- the live arena did change, and the next capture says so
        (_, bytes, _) ← captureLive h "coh-late-next"
        sc2 ← decodeBytes h bytes
        [carrier2] ← pure (scUnits sc2)
        HM.lookup "hydration" (ueStats carrier2) `shouldBe` Just 1.0

    describe "a deferred failure raised inside the capture window" $ do
        let deferred label seams expected = it label $ \h → do
                loadDoc h "capture-coh-fail.yaml" transferDoc
                let env = fxEnv (hFx h)
                    sl = toSaveLoadCapability env
                dir ← freshDir "coh-fail"
                let dest = dir </> "failed.yaml"
                armCaptureSeamsOf h seams
                s ← runCapture h dest False
                disarmCapture h
                case cstOutcome s of
                    Just (CaptureUnsuccessful f) → do
                        (cfKind f, cfPublication f) `shouldBe` expected
                        -- before the lock was released, so before storage began
                        cfStage f `shouldSatisfy` (< CaptureStoring)
                    other → expectationFailure ("expected a failure, got " ⧺ show other)
                -- nothing was published, and nothing is left behind
                doesFileExist dest `shouldReturn` False
                listDir dir `shouldReturn` []
                -- the capture lock is not held, the owners run, the line is free
                captureLocked (slSaveBarrierRef sl) `shouldReturn` False
                saveInProgress (slSaveBarrierRef sl) `shouldReturn` False
                captureInProgress (slLoadStatusRef sl) (slSaveBarrierRef sl) `shouldReturn` False
                mutated ← forM captureOwners (ownerCounts (hOwners h))
                waitCondition 30 $ do
                    now ← forM captureOwners (ownerCounts (hOwners h))
                    pure (and [ mNow > mBefore | ((mBefore, _), (mNow, _)) ← zip mutated now ])
                readIORef (enginePausedRef env) `shouldReturn` True
                -- a later request is admitted and succeeds
                s2 ← runCapture h (dir </> "next.yaml") False
                void (captureOf s2)
        deferred "an exception hidden in the projection surfaces during encoding, not in storage"
            noCaptureSeams { cseamScenario = \sc → if null (scUnits sc) then sc else sc { scUnits = error "deferred projection failure" } }
            (FailEncoding, NotPublished)
        deferred "an exception hidden in one element surfaces while the document is being written out, not in storage"
            noCaptureSeams { cseamScenario = \sc → if null (scUnits sc) then sc else sc { scUnits = map (\u → if T.null (ueDefinition u) then u else error "deferred element") (scUnits sc) } }
            (FailEncoding, NotPublished)
        deferred "an exception hidden in the encoded bytes surfaces during encoding, not in storage"
            noCaptureSeams { cseamBytes = \b → if BS.null b then b else error "deferred encode failure" }
            (FailEncoding, NotPublished)
        deferred "an exception raised after the session was read ends the capture"
            noCaptureSeams { cseamAt = \p → when (p ≡ CpSnapshotted) (ioError (userError "read interrupted")) }
            (FailProjection, NotPublished)
        deferred "an exception raised with the document complete, before release, ends the capture"
            noCaptureSeams { cseamAt = \p → when (p ≡ CpEncoded) (ioError (userError "sealed interrupted")) }
            (FailEncoding, NotPublished)

    describe "the document does not depend on how the arena came to be" $ do
        let two = [ "map: {width: 20, height: 20}", "units:"
                  , "  - {id: a, definition: acolyte, x: 1, y: 1, stats: {hydration: 6.0}}"
                  , "  - {id: b, definition: acolyte, x: 3, y: 3, stats: {hydration: 7.0}}" ]
            swapped = [ "map: {width: 20, height: 20}", "units:"
                      , "  - {id: b, definition: acolyte, x: 3, y: 3, stats: {hydration: 7.0}}"
                      , "  - {id: a, definition: acolyte, x: 1, y: 1, stats: {hydration: 6.0}}" ]
        it "writes the same bytes when the entries were created in another order (allocation numbers differ)" $ \h → do
            loadDoc h "capture-coh-order-a.yaml" two
            idA ← unitIdOf h "a"
            (_, a, _) ← captureLive h "order-a"
            loadDoc h "capture-coh-order-b.yaml" swapped
            idA' ← unitIdOf h "a"
            (_, b, _) ← captureLive h "order-b"
            -- the unit ids really were allocated the other way round
            idA' `shouldNotBe` idA
            a `shouldBe` b

        it "names unreferenced objects by their structure, so creation order never shows" $ \h → do
            let env = fxEnv (hFx h)
                forget = atomicModifyIORef' (unitManagerRef env) $ \m → (m { umScenarioRefs = Map.empty }, ())
            loadDoc h "capture-coh-anon-a.yaml" two
            forget
            (_, a, _) ← captureLive h "anon-a"
            loadDoc h "capture-coh-anon-b.yaml" swapped
            forget
            (_, b, _) ← captureLive h "anon-b"
            a `shouldBe` b
            BS.length a `shouldSatisfy` (> 0)
  where
    authored a = case a of Authored x → Just x; Omitted → Nothing
    armCaptureSeamsOf = armCaptureWith

-- | The carrier's tool, removed from its inventory (the unit owner's half of a
--   transfer).
takeTool ∷ H → IO (Maybe ItemInstance)
takeTool h = do
    let env = fxEnv (hFx h)
    um ← readIORef (unitManagerRef env)
    case [ (u, t) | (u, i) ← HM.toList (umInstances um), t ← uiInventory i, iiDefName t ≡ "steel_bar" ] of
        ((uid, tool) : _) → do
            atomicModifyIORef' (unitManagerRef env) $ \m →
                ( m { umInstances = HM.adjust (\i → i { uiInventory = filter ((≢ iiInstanceId tool) ∘ iiInstanceId) (uiInventory i) }) uid (umInstances m) }
                , () )
            pure (Just tool)
        [] → pure Nothing

-- | The crate's intake (the building owner's half of a transfer).
putTool ∷ H → ItemInstance → IO ()
putTool h tool = do
    let env = fxEnv (hFx h)
    atomicModifyIORef' (buildingManagerRef env) $ \m →
        ( m { bmInstances = HM.map (\b → if biDefName b ≡ "cargo_hold_S" then b { biStorage = biStorage b ⧺ [tool] } else b) (bmInstances m) }
        , () )

carriedTools ∷ H → IO [ItemInstance]
carriedTools h = do
    um ← readIORef (unitManagerRef (fxEnv (hFx h)))
    pure [ t | i ← HM.elems (umInstances um), t ← uiInventory i, iiDefName t ≡ "steel_bar" ]

storedTools ∷ H → IO [ItemInstance]
storedTools h = do
    bm ← readIORef (buildingManagerRef (fxEnv (hFx h)))
    pure [ t | b ← HM.elems (bmInstances bm), t ← biStorage b, iiDefName t ≡ "steel_bar" ]

-- | One chunk's fluid writeback with a single cell replaced and everything
--   else as the chunk holds it, stamped with the generation the world
--   thread would accept.
writebackWith ∷ WorldState → ChunkCoord → Int → Maybe FluidCell → IO FluidWriteback
writebackWith ws coord ix cell = do
    td ← readIORef (wsTilesRef ws)
    gens ← readIORef (wsChunkEditGenRef ws)
    lc ← maybe (fail "the chunk is not resident") pure (lookupChunk coord td)
    pure FluidWriteback
        { fwCoord = coord, fwEditGen = HM.lookupDefault 0 coord gens
        , fwFluid = lcFluidMap lc V.// [(ix, cell)]
        , fwTerrain = lcTerrainSurfaceMap lc, fwSurf = lcSurfaceMap lc
        , fwSideDeco = lcSideDeco lc }
