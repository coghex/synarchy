{-# LANGUAGE Strict #-}
-- | The orchestration "Scenario.Placement" tests drive: materialize a
--   decoded document's placement families (and the units and ground items
--   that bind to them) onto a live arena page through the real owners, and
--   capture the page back into a document. SCN-13 owns the product-level
--   load; this is the order it must follow, exercised end to end:
--
--   1. buildings (footprints judged against the real terrain),
--   2. locations validated, then their would-be occupants excluded when
--      the location is rejected,
--   3. units and ground items installed,
--   4. locations resolved against what installed, structures and
--      locations committed on the world thread in one request.
--
--   Capture is the same order reversed: items, buildings, locations,
--   units (which need the locations' names), structures.
module Test.Headless.Scenario.Placement.Glue
    ( Materialized(..)
    , materialize
    , materializeAllowingFailures
    , Captured'(..)
    , capturePage
    , CaptureSources(..)
    , capturePageFrom
    , saveAndStage
    , encodeSession
    , stageEnvelope
    , sessionSnapshotOf
    , savedEnvelopeFrom
    , capturedYaml
    , roundTrip
    ) where

import Unit.Faction.RelationStore (emptyRelationCauseStore)
import UPrelude
import Data.IORef (readIORef)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Building.Types
import Engine.Core.Capability.RenderHandoff (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.State (EngineEnv(..))
import Gameplay.Tags.Index (categoryForward)
import Location.Instance
import Scenario.Items.Encode (itemEntryFields, floatValue)
import Scenario.Placement.Building
import Scenario.Placement.Encode
import Scenario.Capture.Inputs (SessionParts(..), readSessionInputs)
import Scenario.Capture.Project (Projected(..), projectParts)
import Scenario.Schema (currentScenarioVersion, decodeScenarioBytes)
import Scenario.Types
import Scenario.Units.Encode (unitEntryValue)
import Scenario.Units.Install
import Unit.Sim.Types (UnitThreadState(..))
import World.Save.Types (nameFloraReferences, toBuildingSnapshot, toUnitSnapshot, UnitSnapshot(..))
import Gameplay.Tags.Page (CropPlotStore(..), capturePageTags)
import World.Save.Snapshot (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Component.Page (blankPageSnapshot)
import World.Load.Stage (renderStageError, stageSession)
import World.Load.Types (StagedPage(..), StagedSession(..))
import Item.Knowledge (emptyPortableKnowledge)
import Engine.Graphics.Camera (Camera2D(..))
import Gameplay.Tags.Index (CategoryIndex)
import Gameplay.Tags.Tag (ItemTarget)
import Scenario.Items.Refs (ItemRefs)
import Structure.Palette (TexPalette)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import qualified Data.HashSet as HS
import Scenario.Load.Construct (ConstructEnv(..), Constructed(..), constructScenario)
import Test.Headless.Scenario.Placement.Fixtures
import Unit.Types
import World.Arena.Bounds (ArenaBounds)
import World.Page.Types (WorldPageId)
import World.Render.Camera.Types (WorldCamera(..))
import World.State.Types

-- | What one materialization reports.
data Materialized = Materialized
    { mzStructures  ∷ ![(ScenarioId, (Int, Int, Word8))]
    , mzLocations   ∷ ![(ScenarioId, LocationInstanceId)]
    , mzBuildings   ∷ ![BuildingCompletion]
    , mzUnits       ∷ ![UnitCompletion]
    , mzDiagnostics ∷ ![ScenarioDiagnostic]
    , mzFailures    ∷ ![Text]
      -- ^ accepted content that did not install (empty on success)
    }

-- | Materialize @sc@ on @pid@ through the PRODUCTION construction
--   ('Scenario.Load.Construct.constructScenario'): the one place the
--   adapters are composed, so these tests exercise the order the product
--   load follows rather than a copy of it. A runtime failure to install
--   accepted content fails the example.
materialize ∷ Fx → WorldPageId → WorldState → ArenaBounds → Scenario → IO Materialized
materialize fx pid ws ab sc = do
    mz ← materializeAllowingFailures fx pid ws ab sc
    unless (null (mzFailures mz)) $
        fail ("construction failed: " ⧺ show (mzFailures mz))
    pure mz

-- | 'materialize' for an example that EXPECTS accepted content not to
--   install (a building whose references cannot be held, say): the failures
--   are reported, not raised.
materializeAllowingFailures ∷ Fx → WorldPageId → WorldState → ArenaBounds → Scenario → IO Materialized
materializeAllowingFailures fx pid _ws ab sc = do
    logger ← readIORef (loggerRef (fxEnv fx))
    c ← constructScenario ConstructEnv
        { ceEnv = fxEnv fx, ceLua = fxLua fx, ceLogger = logger, cePage = pid
        , cePaths = noSourcePaths, ceBounds = Just ab, ceSeam = \_ → pure () } sc
    pure Materialized
        { mzStructures = ccStructures c, mzLocations = ccLocations c
        , mzBuildings = ccBuildings c, mzUnits = ccUnits c
        , mzDiagnostics = ccDiagnostics c, mzFailures = ccFailures c }

-- | A captured page.
data Captured' = Captured'
    { cpStructures ∷ ![StructurePiece]
    , cpBuildings  ∷ ![BuildingEntry]
    , cpLocations  ∷ ![LocationEntry]
    , cpUnits      ∷ ![UnitEntry]
    , cpGround     ∷ ![(Float, Float, ItemEntry)]
    , cpDiagnostics ∷ ![ScenarioDiagnostic]
    }

-- | The state a capture reads, taken from one instant: live from the
--   engine, or restored by the real load stage.
data CaptureSources = CaptureSources
    { csUnits    ∷ !UnitManager
    , csBuildings ∷ !BuildingManager
    , csItemRefs ∷ !ItemRefs
    , csItemTags ∷ !(CategoryIndex ItemTarget)
    , csPalette  ∷ !TexPalette
    , csPage     ∷ !WorldPageId
    , csWorld    ∷ !WorldState
    }

-- | Capture the page's placement families, its units and its ground
--   items from one read of the engine's managers.
capturePage ∷ Fx → WorldPageId → WorldState → ArenaBounds → IO Captured'
capturePage fx pid ws ab = do
    let env = fxEnv fx
    um ← readIORef (unitManagerRef env)
    bm ← readIORef (buildingManagerRef env)
    wm ← readIORef (worldManagerRef env)
    palette ← readIORef (rhTexPaletteRef (toRenderHandoffCapability env))
    capturePageFrom fx CaptureSources
        { csUnits = um, csBuildings = bm, csItemRefs = wmItemScenarioRefs wm
        , csItemTags = wmItemTags wm, csPalette = palette, csPage = pid, csWorld = ws } ab

capturePageFrom ∷ Fx → CaptureSources → ArenaBounds → IO Captured'
capturePageFrom fx src _ab = do
    -- the PRODUCTION projection (Scenario.Capture.Project) over the same
    -- inputs the service reads: this is a view of it, not a second copy
    inputs ← readSessionInputs (fxEnv fx) SessionParts
        { spPage = csPage src, spWorld = csWorld src, spUnits = csUnits src
        , spBuildings = csBuildings src, spItemRefs = csItemRefs src
        , spItemTags = csItemTags src, spPalette = csPalette src
        , spLiveCamera = Nothing }
    let parts = projectParts currentScenarioVersion inputs
        sc = pjScenario parts
    pure Captured'
        { cpStructures = scStructures sc, cpBuildings = scBuildings sc
        , cpLocations = scLocations sc, cpUnits = scUnits sc
        , cpGround = [ (giX g, giY g, giItem g) | g ← scGroundItems sc ]
        , cpDiagnostics = pjStructureFindings parts }

-- | The session snapshot an ordinary save of the page holds: every owner's
--   snapshot function over the live managers, the page's own edit log,
--   ground items, power nodes and generation parameters (which carry the
--   location table), and the session-wide palette and item registries.
sessionSnapshotOf ∷ Fx → WorldPageId → WorldState → IO SessionSnapshot
sessionSnapshotOf fx pid ws = do
    let env = fxEnv fx
    um ← readIORef (unitManagerRef env)
    bm ← readIORef (buildingManagerRef env)
    wm ← readIORef (worldManagerRef env)
    uts ← readIORef (ucUtsRef (toUnitCombatCapability env))
    palette ← readIORef (rhTexPaletteRef (toRenderHandoffCapability env))
    params ← maybe (fail "the page has no generation parameters") pure =≪ readIORef (wsGenParamsRef ws)
    edits ← readIORef (wsEditsRef ws)
    ground ← readIORef (wsGroundItemsRef ws)
    powerNodes ← readIORef (wsPowerNodesRef ws)
    nextItem ← readIORef (nextItemInstanceIdRef env)
    -- the page's plants, as a save writes them: species NAMED, the planted
    -- allocator and regrowth timers, the crop plots, the page's tags
    floraCat ← readIORef (floraCatalogRef env)
    plots ← readIORef (wsCropPlotsRef ws)
    harvests ← readIORef (wsFloraHarvestsRef ws)
    pageTags ← readIORef (wsPageTagsRef ws)
    cursor ← readIORef (wsPlantedFloraCursorRef ws)
    -- the page's own camera and the live one, as a save writes them
    WorldCamera wcx wcy ← readIORef (wsCameraRef ws)
    cam ← readIORef (cameraRef env)
    (namedEdits, namedCrops, _, _) ← either (const (fail "a flora species cannot be named")) pure
        (nameFloraReferences floraCat pid edits (cpsPlots plots) HM.empty HM.empty)
    let unitSnap = toUnitSnapshot pid um
        page = (blankPageSnapshot pid params)
            { pgsCameraX = wcx, pgsCameraY = wcy
            , pgsEdits = namedEdits
            , pgsCropPlots = namedCrops
            , pgsFloraHarvests = harvests
            , pgsPlantedFloraCursor = cursor
            , pgsPageTags = capturePageTags pageTags plots
            , pgsGroundItems = ground
            , pgsPowerNodes = powerNodes
            , pgsBuildings = toBuildingSnapshot pid bm
            , pgsUnits = unitSnap
            , pgsUnitSimStates = HM.filterWithKey (\k _ → HM.member k (usnInstances unitSnap)) (utsSimStates uts)
            , pgsGeneratedId = Just (fixtureGeneratedWorldIdForPage pid) }
    pure SessionSnapshot
        { snapGameTime = 0, snapTexPalette = palette
        , snapNextItemId = max 1 nextItem
        , snapNextBuildingId = bmNextId bm, snapNextUnitId = umNextId um
        , snapActivePage = pid, snapVisiblePages = [pid]
        , snapLiveCamera = LiveCameraSnapshot (Just pid) (fst (camPosition cam)) (snd (camPosition cam)) (camZoom cam) (camFacing cam)
        , snapPages = HM.singleton pid page
        , snapItemTags = categoryForward (wmItemTags wm), snapItemRefs = wmItemScenarioRefs wm
        , snapPortableKnowledge = emptyPortableKnowledge
        , snapRelationCauses = emptyRelationCauseStore }

saveMeta ∷ SaveRequestMeta
saveMeta = SaveRequestMeta { srmSlotName = "slot", srmTimestamp = "ts", srmAutosave = False }

-- | An ordinary save of the page through the real codecs and the real
--   load stage, with the sources a capture of the RESTORED state reads:
--   snapshot the page's owners, encode the envelope, decode it, turn it
--   into save data and stage it. No scenario document is involved at any
--   point.
saveAndStage ∷ Fx → WorldPageId → WorldState → IO CaptureSources
saveAndStage fx pid ws = encodeSession fx pid ws ≫= stageEnvelope fx pid

-- | An ordinary save of the page, as the envelope bytes the codec writes.
encodeSession ∷ Fx → WorldPageId → WorldState → IO BS.ByteString
encodeSession fx pid ws = do
    snap ← sessionSnapshotOf fx pid ws
    pure (encodeSessionSnapshot (snapshotSaveMetadata saveMeta snap) snap [])

-- | Load a saved envelope through the real decoder and the real load
--   stage, with the sources a capture of the RESTORED state reads.
stageEnvelope ∷ Fx → WorldPageId → BS.ByteString → IO CaptureSources
stageEnvelope fx pid bytes = do
    let env = fxEnv fx
    back ← either (fail ∘ show) pure $ fmap (\(_, s, _, _) → s) $
        decodeSessionEnvelope HS.empty HS.empty bytes
    logger ← readIORef (loggerRef env)
    matReg ← readIORef (materialRegistryRef env)
    staged ← stageSession env logger (snapshotToSaveData saveMeta back) matReg ≫= \r → case r of
        Left e → fail (T.unpack (renderStageError e))
        Right st → pure st
    sp ← case filter ((≡ pid) ∘ spPageId) (ssPages staged) of
        (p : _) → pure p
        [] → fail "the staged page is missing"
    pure CaptureSources
        { csUnits = ssUnits staged, csBuildings = ssBuildings staged
        , csItemRefs = ssItemRefs staged, csItemTags = ssItemTags staged
        , csPalette = ssTexPalette staged, csPage = pid, csWorld = spWorldState sp }

-- | The save envelope of the page, with the donor save's metadata and Lua
--   components (the page owns none of those): the bytes the retained
--   compatibility fixture is.
savedEnvelopeFrom ∷ Fx → WorldPageId → WorldState → BS.ByteString → IO BS.ByteString
savedEnvelopeFrom fx pid ws donor = do
    snap ← sessionSnapshotOf fx pid ws
    (meta, donorSnap, luaComponents, _) ← either (fail ∘ T.unpack) pure $
        decodeSessionEnvelope (HS.fromList ["unit_ai", "building_spawn", "tutorial_progress"])
                              (HS.fromList ["unit_ai", "building_spawn"]) donor
    -- the donor's metadata lists the page's generated-world identity, so the
    -- page carries the same one
    donorId ← maybe (fail "the donor save has no such page") (pure ∘ pgsGeneratedId)
                    (HM.lookup pid (snapPages donorSnap))
    let snap' = snap { snapPages = HM.adjust (\p → p { pgsGeneratedId = donorId }) pid (snapPages snap) }
    pure (encodeSessionSnapshot meta snap' luaComponents)

-- | The captured page as one document the decoder reads.
capturedYaml ∷ Captured' → BC.ByteString
capturedYaml c = Y.encode $ A.object
    [ "version" A..= (3 ∷ Int)
    , "structures" A..= map structureEntryValue (cpStructures c)
    , "buildings" A..= map buildingEntryValue (cpBuildings c)
    , "locations" A..= map locationEntryValue (cpLocations c)
    , "units" A..= map unitEntryValue (cpUnits c)
    , "ground_items" A..= [ A.object (("x", floatValue x) : ("y", floatValue y) : itemEntryFields e)
                          | (x, y, e) ← cpGround c ] ]

-- | Capture, encode, decode through the real decoder (must be clean), and
--   return the decoded scenario with the document text.
roundTrip ∷ Fx → Captured' → IO (Scenario, BC.ByteString)
roundTrip fx c = do
    let bytes = capturedYaml c
    cat ← scenarioCatalog (fxEnv fx)
    case decodeScenarioBytes cat bytes of
        ScenarioLoaded s [] → pure (s, bytes)
        ScenarioLoaded _ ds → fail ("the captured document decoded with diagnostics: " ⧺ show ds)
        ScenarioFailed f → fail ("the captured document failed: " ⧺ show f)
