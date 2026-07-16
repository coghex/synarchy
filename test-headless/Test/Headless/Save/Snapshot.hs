{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Pure tests for the immutable session-snapshot model (#758,
--   save-overhaul A3): 'World.Save.Snapshot' (capture + validation)
--   and 'World.Save.Snapshot.Adapter' (the temporary bridge to the
--   unchanged v88 'SaveData' format). No engine, no IO, no Lua — every
--   manager/page value below is a synthetic literal, the same pattern
--   'Test.Headless.Craft.Bills'/'Test.Headless.Power.Network' use. The
--   real, multi-thread save/load path is gated by
--   'tools/save_barrier_probe.py'.
module Test.Headless.Save.Snapshot (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (evaluate)
import qualified Data.HashMap.Strict as HM
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
import World.Save.Serialize (encodeSaveData)
import World.Save.Types
    ( BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..)
    , SaveData(..), WorldPageSave(..), SaveMetadata(..) )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (emptyTexPalette)
import Item.Ground (emptyGroundItems)
import Item.Types (ItemInstance(..))
import World.Spoil.Types (emptySpoilPiles)
import World.Flora.Harvest (emptyFloraHarvests)
import World.Flora.CropPlot (emptyCropPlots)
import World.Edit.Types (WorldEdit, emptyWorldEdits)
import World.Chunk.Types (ChunkCoord(..))
import Craft.Bills (emptyCraftBills)
import Power.Types (emptyPowerNodes)
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState(..), Pose(..), UnitActivity(..))
import Unit.Direction (Direction(..))

page1, page2 ∷ WorldPageId
page1 = WorldPageId "page1"
page2 = WorldPageId "page2"

-- | A minimal, otherwise-valid page: no entities, no designations.
minimalPage ∷ WorldPageId → PageSnapshot
minimalPage pid = PageSnapshot
    { pgsPageId       = pid
    , pgsGenParams    = defaultWorldGenParams
    , pgsCameraX      = 0
    , pgsCameraY      = 0
    , pgsTimeHour     = 12
    , pgsTimeMinute   = 0
    , pgsDateYear     = 1
    , pgsDateMonth    = 1
    , pgsDateDay      = 1
    , pgsMapMode      = ZMDefault
    , pgsEdits        = emptyWorldEdits
    , pgsMineDesignations      = HM.empty
    , pgsConstructDesignations = HM.empty
    , pgsGroundItems  = emptyGroundItems
    , pgsSpoilPiles   = emptySpoilPiles
    , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 1 }
    , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 1 }
    , pgsUnitSimStates = HM.empty
    , pgsFloraHarvests = emptyFloraHarvests
    , pgsChopDesignations = HM.empty
    , pgsCraftBills   = emptyCraftBills
    , pgsPowerNodes   = emptyPowerNodes
    , pgsTillDesignations = HM.empty
    , pgsCropPlots    = emptyCropPlots
    , pgsPlantDesignations = HM.empty
    , pgsIdentity     = Nothing
    }

-- | A minimal, otherwise-valid globals record: one Lua module blob
--   (representative of a real session; an empty map is also legal —
--   see the "no Lua modules captured" test), camera attributed to
--   'page1'.
minimalGlobals ∷ SessionGlobals
minimalGlobals = SessionGlobals
    { sgGameTime       = 0
    , sgTexPalette     = emptyTexPalette
    , sgNextItemId     = 1
    , sgNextBuildingId = 1
    , sgNextUnitId     = 1
    , sgLuaModules     = HM.singleton "pause" "placeholder-blob"
    , sgActivePage     = page1
    , sgVisiblePages   = [page1]
    , sgLiveCamera     = LiveCameraSnapshot
        { lcsOwnerPage = Just page1
        , lcsX = 10, lcsY = 20, lcsZoom = 2, lcsFacing = FaceSouth
        }
    }

minimalItem ∷ Word64 → ItemInstance
minimalItem iid = ItemInstance
    { iiDefName = "test_item", iiCurrentFill = 0, iiQuality = 100
    , iiCondition = 100, iiWeight = 1, iiSharpness = 100, iiContents = []
    , iiInstanceId = iid, iiTemp = Nothing
    }

minimalUnitInstance ∷ [ItemInstance] → UnitInstanceSnapshot
minimalUnitInstance inv = UnitInstanceSnapshot
    { uisDefName = "test_unit", uisBaseWidth = 1, uisGridX = 0, uisGridY = 0
    , uisGridZ = 0, uisFacing = DirS, uisCurrentAnim = "", uisAnimStart = 0
    , uisAnimReverse = False, uisActivity = "idle", uisPose = "standing"
    , uisAnimStride = 0, uisStats = HM.empty, uisModifiers = HM.empty
    , uisSkills = HM.empty, uisKnowledge = HM.empty, uisInventory = inv
    , uisEquipped = HM.empty, uisAccessories = [], uisFactionId = ""
    , uisWounds = [], uisScars = [], uisImmuneResponse = 0
    , uisImmunities = HM.empty, uisBlood = 5, uisName = ""
    }

minimalBuildingInstance ∷ [ItemInstance] → BuildingInstanceSnapshot
minimalBuildingInstance storage = BuildingInstanceSnapshot
    { bisDefName = "test_building", bisAnchorX = 0, bisAnchorY = 0
    , bisGridZ = 0, bisSpawnedAt = 0, bisTileW = 1, bisTileH = 1
    , bisSpawnRemaining = 0, bisBuildProgress = 100
    , bisMaterialsDelivered = HM.empty, bisStorage = storage
    }

minimalSimState ∷ UnitSimState
minimalSimState = UnitSimState
    { usRealX = 0, usRealY = 0, usGridZ = 0, usRealZ = 0
    , usTarget = Nothing, usPose = Standing, usState = Idle, usFacing = DirS
    , usLocalPath = []
    , usDrinkUntil = Nothing, usEatUntil = Nothing, usPickupUntil = Nothing
    , usTransitionUntil = Nothing, usTransitionStride = 0, usPostTransition = []
    , usClimbFromTile = Nothing, usClimbToTile = Nothing, usClimbStartTime = Nothing
    , usClimbSlipAt = Nothing, usFallFromTile = Nothing, usFallToTile = Nothing
    , usPendingClimbXP = 0, usGetUpAt = Nothing, usPendingFallDrop = Nothing
    , usJumpApex = Nothing, usMoveGrade = 0
    }

spec ∷ Spec
spec = do
    describe "capture" $ do
        it "builds a snapshot from multiple pages and global managers" $ do
            let snap = buildSessionSnapshot minimalGlobals
                    [minimalPage page1, minimalPage page2]
            HM.keys (snapPages snap) `shouldMatchList` [page1, page2]
            snapActivePage snap `shouldBe` page1
            snapGameTime snap `shouldBe` 0

        it "captureSessionSnapshot accepts a well-formed multi-page session" $
            case captureSessionSnapshot minimalGlobals
                    [minimalPage page1, minimalPage page2] of
                Right _  → pure ()
                Left errs → expectationFailure (show errs)

        it "a later value built from a mutated manager does not alter an \
           \already-captured snapshot (value semantics: nothing in this \
           \pure model can retroactively change a previously-returned \
           \'SessionSnapshot')" $ do
            let building1 = BuildingSnapshot
                    { bsnInstances = HM.singleton (BuildingId 1)
                                         (minimalBuildingInstance [])
                    , bsnNextId = 2 }
                pageWithBuilding = (minimalPage page1) { pgsBuildings = building1 }
                snap = buildSessionSnapshot minimalGlobals [pageWithBuilding]
                -- A "later mutation" of the same live data -- a
                -- DIFFERENT value, never touching 'snap'.
                building2 = building1 { bsnNextId = 999 }
            bsnNextId (pgsBuildings (snapPages snap HM.! page1))
                `shouldBe` 2
            bsnNextId building2 `shouldNotBe`
                bsnNextId (pgsBuildings (snapPages snap HM.! page1))

    describe "validation" $ do
        let capture globals pages = captureSessionSnapshot globals pages

        it "rejects an empty session (no persistable pages)" $
            case capture minimalGlobals [] of
                Left errs → errs `shouldContain` [NoPersistablePages]
                Right _   → expectationFailure "expected rejection"

        it "rejects duplicate page ids" $
            case capture minimalGlobals [minimalPage page1, minimalPage page1] of
                Left errs → errs `shouldBe` [DuplicatePageIds [page1]]
                Right _   → expectationFailure "expected rejection"

        it "rejects a missing active page" $
            case capture minimalGlobals { sgActivePage = page2 }
                    [minimalPage page1] of
                Left errs → errs `shouldContain` [ActivePageMissing page2]
                Right _   → expectationFailure "expected rejection"

        it "rejects a missing visible page" $
            case capture minimalGlobals { sgVisiblePages = [page2] }
                    [minimalPage page1] of
                Left errs → errs `shouldContain` [VisiblePageMissing page2]
                Right _   → expectationFailure "expected rejection"

        it "rejects a live-camera owner page that isn't captured" $
            let cam = (sgLiveCamera minimalGlobals) { lcsOwnerPage = Just page2 }
            in case capture minimalGlobals { sgLiveCamera = cam }
                    [minimalPage page1] of
                Left errs → errs `shouldContain` [VisiblePageMissing page2]
                Right _   → expectationFailure "expected rejection"

        it "rejects an orphaned unit sim state" $
            let page = (minimalPage page1)
                    { pgsUnitSimStates = HM.singleton (UnitId 99) minimalSimState }
            in case capture minimalGlobals [page] of
                Left errs → errs `shouldContain`
                    [OrphanedUnitSimState page1 (UnitId 99)]
                Right _   → expectationFailure "expected rejection"

        it "accepts a unit sim state that DOES match a restored unit" $
            let units = UnitSnapshot
                    { usnInstances = HM.singleton (UnitId 1) (minimalUnitInstance [])
                    , usnNextId = 2 }
                page = (minimalPage page1)
                    { pgsUnits = units
                    , pgsUnitSimStates = HM.singleton (UnitId 1) minimalSimState }
            in case capture minimalGlobals { sgNextUnitId = 2 } [page] of
                Right _   → pure ()
                Left errs → expectationFailure (show errs)

        it "rejects an item instance id at or above the item allocator" $
            let units = UnitSnapshot
                    { usnInstances = HM.singleton (UnitId 1)
                        (minimalUnitInstance [minimalItem 5])
                    , usnNextId = 2 }
                page = (minimalPage page1) { pgsUnits = units }
            in case capture minimalGlobals { sgNextItemId = 5 } [page] of
                Left errs → errs `shouldContain` [ItemInstanceIdNotBelowAllocator 5]
                Right _   → expectationFailure "expected rejection"

        it "rejects a duplicate item instance id across pages" $
            let unitsWith iid = UnitSnapshot
                    { usnInstances = HM.singleton (UnitId 1)
                        (minimalUnitInstance [minimalItem iid])
                    , usnNextId = 2 }
                p1 = (minimalPage page1) { pgsUnits = unitsWith 5 }
                p2 = (minimalPage page2) { pgsUnits = unitsWith 5 }
            in case capture minimalGlobals
                    { sgNextItemId = 100, sgVisiblePages = [page1] } [p1, p2] of
                Left errs → errs `shouldContain` [DuplicateItemInstanceId 5]
                Right _   → expectationFailure "expected rejection"

        it "rejects a building id at or above the building allocator" $
            let buildings = BuildingSnapshot
                    { bsnInstances = HM.singleton (BuildingId 3)
                        (minimalBuildingInstance [])
                    , bsnNextId = 3 }
                page = (minimalPage page1) { pgsBuildings = buildings }
            in case capture minimalGlobals { sgNextBuildingId = 3 } [page] of
                Left errs → errs `shouldContain` [BuildingAllocatorTooLow (BuildingId 3)]
                Right _   → expectationFailure "expected rejection"

        it "rejects a unit id at or above the unit allocator" $
            let units = UnitSnapshot
                    { usnInstances = HM.singleton (UnitId 4) (minimalUnitInstance [])
                    , usnNextId = 4 }
                page = (minimalPage page1) { pgsUnits = units }
            in case capture minimalGlobals { sgNextUnitId = 4 } [page] of
                Left errs → errs `shouldContain` [UnitAllocatorTooLow (UnitId 4)]
                Right _   → expectationFailure "expected rejection"

        it "accepts a session with no Lua modules captured (a Lua-less \
           \engine-only save is legitimate -- see World.Save.Snapshot's \
           \validateSessionSnapshot haddock for why this isn't checked)" $
            case capture minimalGlobals { sgLuaModules = HM.empty }
                    [minimalPage page1] of
                Right _   → pure ()
                Left errs → expectationFailure (show errs)

    describe "camera representation (contract requirement 5)" $ do
        it "represents the live camera exactly once, attributed to its owner page" $ do
            let snap = buildSessionSnapshot minimalGlobals
                    [minimalPage page1, minimalPage page2]
            lcsOwnerPage (snapLiveCamera snap) `shouldBe` Just page1
            lcsX (snapLiveCamera snap) `shouldBe` 10
            lcsZoom (snapLiveCamera snap) `shouldBe` 2

        it "keeps a hidden page's own stored position distinct from the live camera" $ do
            let hiddenPage = (minimalPage page2)
                    { pgsCameraX = 111, pgsCameraY = 222 }
                snap = buildSessionSnapshot minimalGlobals
                    [minimalPage page1, hiddenPage]
            pgsCameraX (snapPages snap HM.! page2) `shouldBe` 111
            pgsCameraY (snapPages snap HM.! page2) `shouldBe` 222

    describe "adapter (temporary bridge to v88 SaveData)" $ do
        let req = SaveRequestMeta { srmSlotName = "my_save", srmTimestamp = "ts" }
            snap = buildSessionSnapshot minimalGlobals
                [minimalPage page1, minimalPage page2]
            sd = snapshotToSaveData req snap

        it "always writes sdEnginePaused = True (load policy, never captured)" $
            sdEnginePaused sd `shouldBe` True

        it "carries the slot name and timestamp straight from the request, \
           \never from the snapshot" $ do
            smName (sdMetadata sd) `shouldBe` "my_save"
            smTimestamp (sdMetadata sd) `shouldBe` "ts"

        it "derives smSeed/smWorldSize/smPlateCount from the active page's \
           \gen params" $ do
            smSeed (sdMetadata sd) `shouldBe` wgpSeed defaultWorldGenParams
            smWorldSize (sdMetadata sd) `shouldBe` wgpWorldSize defaultWorldGenParams
            smPlateCount (sdMetadata sd) `shouldBe` wgpPlateCount defaultWorldGenParams

        it "fabricates wpsTimeScale = 1 and wpsToolMode = DefaultTool on every page" $
            forM_ (sdWorlds sd) $ \wps → do
                wpsTimeScale wps `shouldBe` 1
                wpsToolMode wps `shouldBe` DefaultTool

        it "writes the snapshot's CANONICAL building/unit allocators, never a \
           \page-local BuildingSnapshot/UnitSnapshot's own (possibly stale) \
           \bsnNextId/usnNextId (review round 1 regression: a page-local \
           \counter below the validated session-wide allocator must not \
           \silently persist a lower, id-reuse-permitting value)" $ do
            let staleBuildings = BuildingSnapshot
                    { bsnInstances = HM.empty, bsnNextId = 1 }
                staleUnits = UnitSnapshot
                    { usnInstances = HM.empty, usnNextId = 1 }
                page = (minimalPage page1)
                    { pgsBuildings = staleBuildings, pgsUnits = staleUnits }
                snapStale = buildSessionSnapshot
                    minimalGlobals { sgNextBuildingId = 42, sgNextUnitId = 42 }
                    [page]
                sdStale = snapshotToSaveData req snapStale
                wps = case sdWorlds sdStale of
                    [w]   → w
                    other → error ("expected exactly one WorldPageSave, got "
                                   <> show (length other))
            bsnNextId (wpsBuildings wps) `shouldBe` 42
            usnNextId (wpsUnits wps) `shouldBe` 42

        it "uses the live camera's position only for its owner page, and \
           \duplicates its zoom/facing onto every page (matching v88's \
           \existing single-Camera2D behaviour)" $ do
            let byId = HM.fromList [ (wpsPageId w, w) | w ← sdWorlds sd ]
                owner  = byId HM.! page1
                hidden = byId HM.! page2
            wpsCameraX owner `shouldBe` 10
            wpsCameraY owner `shouldBe` 20
            wpsCameraX hidden `shouldBe` pgsCameraX (minimalPage page2)
            wpsCameraY hidden `shouldBe` pgsCameraY (minimalPage page2)
            wpsCameraZoom owner `shouldBe` 2
            wpsCameraZoom hidden `shouldBe` 2
            wpsCameraFacing owner `shouldBe` FaceSouth
            wpsCameraFacing hidden `shouldBe` FaceSouth

        it "storage/request metadata does not affect the underlying \
           \snapshot's own structural equivalence -- two adapts of the \
           \SAME snapshot with DIFFERENT request metadata still compare \
           \equal at the snapshot level" $ do
            let req2 = SaveRequestMeta { srmSlotName = "different_name"
                                       , srmTimestamp = "different_ts" }
                sd2  = snapshotToSaveData req2 snap
            snap `shouldBe` buildSessionSnapshot minimalGlobals
                [minimalPage page1, minimalPage page2]
            smName (sdMetadata sd) `shouldNotBe` smName (sdMetadata sd2)
            smTimestamp (sdMetadata sd) `shouldNotBe` smTimestamp (sdMetadata sd2)
            sdGameTime sd `shouldBe` sdGameTime sd2
            map wpsPageId (sdWorlds sd) `shouldBe` map wpsPageId (sdWorlds sd2)

    -- #758 review round 2 follow-up: capture/validation must not silently
    -- pass a snapshot that still hides an unevaluated exception somewhere
    -- deep inside its captured data -- 'World.Save.Snapshot'/'.Adapter''s
    -- record fields are only forced to WHNF (via Strict/HashMap.Strict),
    -- never deeply, so a thunk buried inside a LIST stored as a HashMap
    -- VALUE survives every construction step untouched.
    -- 'World.Save.Serialize.encodeSaveData' is what
    -- 'World.Thread.Command.Save.WriteWorld' forces (via 'evaluate') BEFORE
    -- releasing the #757 barrier -- this proves it actually reaches, and
    -- forces, a payload this deeply nested.
    describe "full-encode forcing (review round 2 follow-up)" $ do
        it "captureSessionSnapshot accepts a snapshot with a deferred, \
           \deeply-nested exploding thunk buried in a page's edit log \
           \(capture/validation never touches list ELEMENTS, only \
           \top-level shape)" $ do
            let explodingEdits = HM.singleton (ChunkCoord 0 0)
                    [error "deferred nested payload boom" ∷ WorldEdit]
                page = (minimalPage page1) { pgsEdits = explodingEdits }
            case captureSessionSnapshot minimalGlobals [page] of
                Right _   → pure ()
                Left errs → expectationFailure (show errs)

        it "encodeSaveData forces that same deferred thunk and throws -- \
           \proving the #758 fix catches it BEFORE the barrier would \
           \release, rather than later during the disk write" $ do
            let req = SaveRequestMeta { srmSlotName = "my_save", srmTimestamp = "ts" }
                explodingEdits = HM.singleton (ChunkCoord 0 0)
                    [error "deferred nested payload boom" ∷ WorldEdit]
                page = (minimalPage page1) { pgsEdits = explodingEdits }
                snap = case captureSessionSnapshot minimalGlobals [page] of
                    Right s   → s
                    Left errs → error ("expected acceptance, got " <> show errs)
                sd = snapshotToSaveData req snap
            evaluate (encodeSaveData sd)
                `shouldThrow` errorCall "deferred nested payload boom"
