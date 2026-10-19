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
    , Captured'(..)
    , capturePage
    , CaptureSources(..)
    , capturePageFrom
    , saveAndStage
    , sessionSnapshotOf
    , savedEnvelopeFrom
    , capturedYaml
    , roundTrip
    ) where

import UPrelude
import Data.IORef (atomicModifyIORef', readIORef)
import Data.List (sortOn)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Building.Types
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.RenderHandoff (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability, withPageLifecycle)
import Engine.Core.State (EngineEnv(..), freshItemInstanceId)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Gameplay.Tags.Index (categoryForward, categoryTagsOf)
import Power.Types (nodeForBuilding)
import Gameplay.Tags.Live (commitItemTreeAssignments)
import Gameplay.Tags.Tag (mkItemTarget)
import Item.Ground (GroundItems(..))
import qualified Item.Ground as G
import Item.Types (ItemInstance(..))
import Location.Instance
import Scenario.Items.Build (ItemBuild(..), ItemBuildEnv(..), itemRefTargets, itemTagTargets)
import Scenario.Items.Capture
    ( CaptureCtx(..), Captured(..), DocKey(..), captureGroundItems, nameDocumentRefs, newCaptureState )
import Scenario.Items.Encode (itemEntryFields, floatValue)
import Scenario.Items.Owners (buildGroundItems, installGroundItems)
import Scenario.Items.Refs (lookupItemRef)
import Scenario.Placement.Building
import Scenario.Placement.Encode
import Scenario.Placement.Install (requestPlacementInstall)
import Scenario.Placement.Location
import Scenario.Placement.Structure
import Scenario.Placement.Types
import Scenario.Schema (decodeScenarioBytes)
import Scenario.Types
import Scenario.Units.Build
import Scenario.Units.Capture
import Scenario.Units.Encode (unitEntryValue)
import Scenario.Units.Install
import Scenario.Units.Ready (luaUnitReadiness)
import Unit.Sim.Types (UnitThreadState(..))
import World.Save.Types (toBuildingSnapshot, toUnitSnapshot, UnitSnapshot(..))
import World.Save.Snapshot (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Component.Page (blankPageSnapshot)
import World.Load.Stage (renderStageError, stageSession)
import World.Load.Types (StagedPage(..), StagedSession(..))
import Item.Knowledge (emptyPortableKnowledge)
import Engine.Graphics.Camera (CameraFacing(..))
import Gameplay.Tags.Index (CategoryIndex)
import Gameplay.Tags.Tag (ItemTarget)
import Scenario.Items.Refs (ItemRefs)
import Structure.Palette (TexPalette)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import qualified Data.HashSet as HS
import Scenario.World.Capture (chunkStatesOf, placementTiles)
import Test.Headless.Scenario.Placement.Fixtures
import Unit.Types
import World.Arena.Bounds (ArenaBounds)
import World.Chunk.Admit (pageIncarnation)
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId)
import World.State.Types
import World.Tile.Types (WorldTileData(..))

-- | What one materialization reports.
data Materialized = Materialized
    { mzStructures  ∷ ![(ScenarioId, (Int, Int, Word8))]
    , mzLocations   ∷ ![(ScenarioId, LocationInstanceId)]
    , mzBuildings   ∷ ![BuildingCompletion]
    , mzUnits       ∷ ![UnitCompletion]
    , mzDiagnostics ∷ ![ScenarioDiagnostic]
    }

-- | Materialize @sc@ on @pid@.
materialize ∷ Fx → WorldPageId → WorldState → ArenaBounds → Scenario → IO Materialized
materialize fx pid ws ab sc = do
    let env = fxEnv fx
        caps = toContentRegistriesCapability env
    epoch ← pageIncarnation ws
    items ← itemsOf env
    now ← readIORef (gameTimeRef env)
    defsB ← bmDefsOf env
    locs ← locationRegistry env
    loot ← lootTables env
    params ← readIORef (wsGenParamsRef ws)
    let seed = maybe 0 wgpSeed params
        worldSize = maybe 0 wgpWorldSize params
        itemEnv = ItemBuildEnv items (freshItemInstanceId env)
    td ← readIORef (wsTilesRef ws)
    edits ← readIORef (wsEditsRef ws)
    let tiles = placementTiles (Just ab) seed edits td (buildingFootprintTiles defsB (scBuildings sc))
    bm0 ← readIORef (buildingManagerRef env)
    let lis0 = maybe emptyLocationInstances wgpLocationInstances params
        pageB = bm0 { bmInstances = HM.filter ((≡ pid) ∘ biPage) (bmInstances bm0) }
    -- 1. buildings
    bbs ← buildBuildings BuildingBuildEnv
              { bbeDefs = defsB, bbeItems = itemEnv, bbeBounds = Just ab
              , bbeWorldSize = worldSize, bbeTiles = tiles, bbeExisting = pageB
              , bbeLocations = lis0, bbeNow = now, bbePage = pid }
        [ (indexPath' "buildings" i, e) | (i, e) ← zip [0 ∷ Int ..] (scBuildings sc) ]
    let brefs = BuildingInstallRefs
            { birBuildings = buildingManagerRef env, birUnits = unitManagerRef env
            , birWorld = worldManagerRef env
            , birLifecycle = withPageLifecycle (toWorldSimCapability env)
            , birWorldSize = worldSize }
    bcs ← installBuildingBuilds brefs pid epoch bbs
    -- 2. locations judged, their occupants decided
    let lenv = LocationBuildEnv { lbeDefs = locs, lbeBounds = Just ab, lbeSeed = seed, lbeNamer = Nothing }
        locEntries = [ (indexPath' "locations" i, e) | (i, e) ← zip [0 ∷ Int ..] (scLocations sc) ]
        lv = validateLocations lenv locEntries
        unitEntries = [ (indexPath' "units" i, e) | (i, e) ← zip [0 ∷ Int ..] (scUnits sc) ]
        (liveUnits, unitCascade) = rejectUnitsOfRejectedLocations (lvRejected lv) unitEntries
    -- 3. units and ground items
    um ← readIORef (unitManagerRef env)
    reg ← readIORef (crPhysiologyRegistryRef caps)
    ecm ← readReadOnlyRef (crvEquipmentClassManagerRef (toContentRegistriesViewCapability env))
    inf ← readIORef (crInfectionManagerRef caps)
    let uenv = UnitBuildEnv
            { ubeDefs = umDefs um, ubeItems = itemEnv, ubeEquipment = ecm
            , ubePhysiology = reg, ubeInfections = inf, ubeNow = now
            , ubePage = pid, ubeSurfaceZ = \_ _ → Just 4 }
    ubs ← forM liveUnits $ \(p, e) → (,) e ⊚ buildUnit uenv p e
    let urefs = UnitInstallRefs
            { uirUnits = unitManagerRef env, uirSims = ucUtsRef (toUnitCombatCapability env)
            , uirBuildings = buildingManagerRef env, uirWorld = worldManagerRef env
            , uirLifecycle = withPageLifecycle (toWorldSimCapability env)
            , uirReady = luaUnitReadiness (lbsLuaState (fxLua fx)) }
    ucs ← installUnitBuilds urefs pid epoch (map snd ubs)
    -- the definition-provided guaranteed content a location omits is
    -- constructed once, from the accepted locations only
    let (acceptedLocs, defaultGround) = expandLocationDefaults locs loot (scGroundItems sc) (lvAccepted lv)
    (placements, gbuild) ← buildGroundItems itemEnv (scGroundItems sc ⧺ defaultGround)
    let (store, _) = installGroundItems [ p | Just p ← placements ] G.emptyGroundItems
    -- a materialization establishes the page's baseline: its ground is replaced
    atomicModifyIORef' (wsGroundItemsRef ws) (const (store, ()))
    _ ← commitItemTreeAssignments (unitManagerRef env) (buildingManagerRef env) (worldManagerRef env)
            (itemTagTargets gbuild) (itemRefTargets gbuild)
    -- 4. locations resolved and everything committed on the world thread
    let joins =
            [ UnitJoin loc uid (usRealHome e)
            | ((e, _), UnitInstalled uid _) ← zip ubs ucs
            , Just loc ← [ueEncounter e] ]
        usRealHome e = (ueX e, ueY e)
        bindings = Map.fromList $
            [ (r, ItemBinding i (iiDefName n) (iiInstanceId n ≡ iiInstanceId root))
            | (placement, tree) ← zip placements (ibTrees gbuild)
            , Just _ ← [placement], Just root ← [tree], n ← flatNodes root
            , (i, r) ← ibRefs gbuild, i ≡ iiInstanceId n ]
            ⧺ [ (r, ItemBinding i (iiDefName n) False)
              | BuildingBuild (Just b) _ ← bbs
              , BuildingInstalled _ ref ← bcs, ref ≡ bbRef b
              , Just root ← ibTrees (bbItems b), n ← flatNodes root
              , (i, r) ← ibRefs (bbItems b), i ≡ iiInstanceId n ]
        (installs, resolveDiags) = resolveLocationInstalls locs joins bindings acceptedLocs
    outcome ← requestPlacementInstall (toWorldSimCapability env) pid epoch
        ScenarioPlacementRequest
            { sprStructures = Just [ (indexPath' "structures" i, p) | (i, p) ← zip [0 ∷ Int ..] (scStructures sc) ]
            , sprLocations = Just installs }
    case outcome of
        PlacementInstalled r → pure Materialized
            { mzStructures = pirStructures r, mzLocations = pirLocations r
            , mzBuildings = bcs, mzUnits = ucs
            , mzDiagnostics = concatMap bbuDiagnostics bbs ⧺ lvDiagnostics lv ⧺ unitCascade
                ⧺ resolveDiags ⧺ pirDiagnostics r }
        other → fail ("placement was not installed: " ⧺ show other)
  where
    flatNodes i = i : concatMap flatNodes (iiContents i)
    indexPath' k i = k <> "[" <> T.pack (show i) <> "]"

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
capturePageFrom fx src ab = do
    let env = fxEnv fx
        caps = toContentRegistriesCapability env
        pid = csPage src
        ws = csWorld src
        um = csUnits src
        bm = csBuildings src
        palette = csPalette src
    params ← readIORef (wsGenParamsRef ws)
    ground ← readIORef (wsGroundItemsRef ws)
    nodes ← readIORef (wsPowerNodesRef ws)
    td ← readIORef (wsTilesRef ws)
    edits ← readIORef (wsEditsRef ws)
    items ← itemsOf env
    defsB ← bmDefsOf env
    locs ← locationRegistry env
    reg ← readIORef (crPhysiologyRegistryRef caps)
    now ← readIORef (gameTimeRef env)
    art ← readIORef (rhStructureArtCatalogRef (toRenderHandoffCapability env))
    let lis = maybe emptyLocationInstances wgpLocationInstances params
        seed = maybe 0 wgpSeed params
        itemCtx = CaptureCtx
            { ccRef = \i → lookupItemRef i (csItemRefs src)
            , ccTags = \i → maybe Set.empty (\t → categoryTagsOf t (csItemTags src)) (mkItemTarget i) }
        pageUnits = sortOn fst [ (u, i) | (u, i) ← HM.toList (umInstances um), uiPage i ≡ pid ]
        pageBuildings = sortOn fst [ (b, i) | (b, i) ← HM.toList (bmInstances bm), biPage i ≡ pid ]
        -- scenario ids share ONE namespace: every referenced unit, building
        -- and location is named together, before anything is captured
        docNames = nameDocumentRefs HS.empty $
            [ (DocUnit u, r) | (u, _) ← pageUnits, Just r ← [Map.lookup u (umScenarioRefs um)] ]
            ⧺ [ (DocBuilding b, r) | (b, _) ← pageBuildings, Just r ← [Map.lookup b (bmScenarioRefs bm)] ]
            ⧺ [ (DocLocation l, r) | (l, r) ← Map.toList (lisScenarioRefs lis) ]
        groundTrees = map G.giInst (HM.elems (gisItems ground))
        buildingTrees = concat [ biStorage b ⧺ concat (HM.elems (biMaterialsDelivered b)) | (_, b) ← pageBuildings ]
        unitItemTrees = concatMap (unitTrees ∘ snd) pageUnits
        -- every item reference in the document is named up front too
        stNames = newCaptureState itemCtx (Map.elems docNames) (groundTrees ⧺ buildingTrees ⧺ unitItemTrees)
        lctx = LocationCaptureCtx
            { lccDefs = locs, lccItemRef = const Nothing
            , lccNameOf = \l → Map.lookup (DocLocation l) docNames }
        (_, locNameMap) = locationNames lctx stNames lis
        foreignIds = Map.elems docNames ⧺ Map.elems locNameMap
        ucap = captureUnits UnitCaptureCtx
            { uccDefs = umDefs um, uccItems = items, uccPhysiology = reg, uccNow = now
            , uccUnitRef = \u → Map.lookup u (umScenarioRefs um)
            , uccUnitTags = \u → categoryTagsOf u (umTags um)
            , uccItemCtx = itemCtx
            , uccOtherTrees = groundTrees ⧺ buildingTrees
            , uccPreNamed = \u → Map.lookup (DocUnit u) docNames
            , uccEncounterOf = \u → lookup u occupantNames }
            foreignIds pageUnits
        occupantNames =
            [ (leoUnitId o, ExplicitId n)
            | inst ← instancesToList lis, Just n ← [Map.lookup (liId inst) locNameMap]
            , Just enc ← [liEncounter inst], o ← leOccupants enc ]
        bcap = captureBuildings BuildingCaptureCtx
            { bccDefs = defsB
            , bccNodes = \b → nodeForBuilding b nodes
            , bccNameOf = \b → Map.lookup (DocBuilding b) docNames
            , bccTags = \b → categoryTagsOf b (bmTags bm)
            , bccItemCtx = itemCtx } (ucapState ucap) pageBuildings
        (gst, groundRows, gcap) = captureGroundItems itemCtx (bcapState bcap) ground
        itemRefs = capRefs gcap ⧺ bcapItems bcap ⧺ ucapItems ucap
        lcap = captureLocations lctx { lccItemRef = \i → lookup i itemRefs
                                     , lccNameOf = \l → Map.lookup l locNameMap }
                                gst lis
        chunks = chunkStatesOf (Just ab) seed edits (HM.elems (wtdChunks td))
        taken = Set.fromList (map (scenarioIdText ∘ beId) (bcapEntries bcap)
                             ⧺ map (scenarioIdText ∘ leId) (lcapEntries lcap)
                             ⧺ map (scenarioIdText ∘ ueId) (ucapEntries ucap))
        scap = captureStructures StructureCaptureCtx
            { sccArt = art, sccPalette = palette, sccBounds = Just ab } chunks taken
    pure Captured'
        { cpStructures = scPieces scap, cpBuildings = bcapEntries bcap
        , cpLocations = lcapEntries lcap, cpUnits = ucapEntries ucap
        , cpGround = groundRows, cpDiagnostics = scDiagnostics scap }

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
    let unitSnap = toUnitSnapshot pid um
        page = (blankPageSnapshot pid params)
            { pgsEdits = edits
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
        , snapLiveCamera = LiveCameraSnapshot (Just pid) 0 0 1 FaceSouth
        , snapPages = HM.singleton pid page
        , snapItemTags = categoryForward (wmItemTags wm), snapItemRefs = wmItemScenarioRefs wm
        , snapPortableKnowledge = emptyPortableKnowledge }

saveMeta ∷ SaveRequestMeta
saveMeta = SaveRequestMeta { srmSlotName = "slot", srmTimestamp = "ts", srmAutosave = False }

-- | An ordinary save of the page through the real codecs and the real
--   load stage, with the sources a capture of the RESTORED state reads:
--   snapshot the page's owners, encode the envelope, decode it, turn it
--   into save data and stage it. No scenario document is involved at any
--   point.
saveAndStage ∷ Fx → WorldPageId → WorldState → IO CaptureSources
saveAndStage fx pid ws = do
    let env = fxEnv fx
    snap ← sessionSnapshotOf fx pid ws
    back ← either (fail ∘ show) pure $ fmap (\(_, s, _, _) → s) $
        decodeSessionEnvelope HS.empty HS.empty
            (encodeSessionSnapshot (snapshotSaveMetadata saveMeta snap) snap [])
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
