{-# LANGUAGE Strict #-}
-- | "Scenario.Placement" (#2712, SCN-12 of epic #2698): buildings,
--   structure pieces and REAL placed locations materialized onto a live
--   arena page and captured back, against the shipped definitions, the
--   shipped structure pack and the world thread.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Scenario.Placement"'@.
module Test.Headless.Scenario.Placement (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (atomicModifyIORef', modifyIORef', readIORef)
import qualified Data.ByteString as BS
import System.Environment (lookupEnv)
import Data.List (intercalate, sort, sortOn)
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Item.Ground (GroundItems(..))
import qualified Item.Ground as G
import Item.Types (ItemInstance(..))
import Scenario.Items.Refs (itemRefsToAscList)
import System.Random (StdGen, mkStdGen, randomR)
import Building.Types
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.State (EngineEnv(..), freshItemInstanceId)
import Gameplay.Tags.Index (categoryTagsOf, emptyCategoryIndex)
import Scenario.Items.Refs (emptyItemRefs)
import Unit.Sim.Types (UnitThreadState(..))
import Unit.Types (UnitManager(..))
import Location.Instance
import Location.ArenaPlacement (allocateLocationInstanceAtIn)
import Location.Types (lookupLocation)
import Unit.Types (UnitId(..))
import Power.Types (nodeForBuilding, PowerNode(..))
import Scenario.Placement.Building (BuildingCompletion(..), BuildingInstallFailure(..))
import Scenario.Types
import qualified Test.Headless.Scenario.Placement.Art as Art
import qualified Test.Headless.Scenario.Placement.Persistence as Persistence
import Test.Headless.Scenario.Placement.Fixtures
import Test.Headless.Scenario.Placement.Glue
import World.Arena.Bounds (ArenaBounds, mkArenaBounds)
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), columnIndex)
import World.Tile.Types (lookupChunk)
import Location.Types (LocationContent(..), LocationDef(..))
import LootTable.Roll (LootRollContext(..), rollLootTableFor)
import LootTable.Types (lookupLootTable)
import Scenario.Items.Refs (stableHash64)
import World.Generate.Types (WorldGenParams(..))
import World.Save.Integrity (IntegrityError(..), sessionIntegrityErrors)
import World.Thread.ChunkLoading (locationStampsFor)
import World.Page.Types (WorldPageId(..))
import World.State.Types
import World.Tile.Types (WorldTileData(..))

spec ∷ Spec
spec = do
  Art.spec
  Persistence.spec
  aroundAll (\act → withFx (\fx → registerFixtures fx ≫ act fx)) $ do
    buildingSpec
    structureSpec
    locationSpec
    containerSpec
    referenceSpec
    lifecycleSpec
    ownerSpec
    defaultsSpec
    noticeSpec
    matrixSpec
    bindingSpec
    storageSpec
    saveSpec
    fixtureSpec
    roundTripSpec
    propertySpec

-- | The two fixture definitions the container and encounter examples need,
--   derived from the shipped @ruin_small@.
registerFixtures ∷ Fx → IO ()
registerFixtures fx = do
    ruinDef ← maybe (fail "ruin_small is not registered") pure
        ∘ lookupLocation "ruin_small" =≪ locationRegistry (fxEnv fx)
    addLocationDef (fxEnv fx) (cryptDef ruinDef)
    addLocationDef (fxEnv fx) (quietDef ruinDef)

-- * Scene

-- | A fresh 40x40 arena: x in [-20, 19], y in [-19, 20] (the map is
--   centred on the origin), so the finite edges fall inside partial
--   chunks on every side. The unit, building and item registries are the
--   engine's and scenario references are unique per session, so every
--   example starts from empty ones (a real load replaces the whole
--   session the same way).
scene ∷ Fx → IO (WorldPageId, WorldState)
scene fx = resetRegistries fx ≫ newArena (fxEnv fx) "placement" 40 40

-- | Empty the engine-wide registries scenario content lives in.
resetRegistries ∷ Fx → IO ()
resetRegistries fx = do
    let env = fxEnv fx
    modifyIORef' (buildingManagerRef env)
        (\bm → bm { bmInstances = HM.empty, bmScenarioRefs = Map.empty, bmTags = emptyCategoryIndex })
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.empty, umScenarioRefs = Map.empty, umTags = emptyCategoryIndex }, ())
    atomicModifyIORef' (ucUtsRef (toUnitCombatCapability env)) $ \uts → (uts { utsSimStates = HM.empty }, ())
    modifyIORef' (worldManagerRef env)
        (\wm → wm { wmItemScenarioRefs = emptyItemRefs, wmItemTags = emptyCategoryIndex })

doc ∷ [String] → String
doc ls = unlines ("version: 3" : ls)

clean ∷ Fx → String → IO Scenario
clean fx yaml = do
    (s, ds) ← decodeDocument (fxEnv fx) yaml
    ds `shouldBe` []
    pure s

arena40 ∷ ArenaBounds
arena40 = either (error ∘ show) id (mkArenaBounds 40 40)

pageLocations ∷ WorldState → IO LocationInstances
pageLocations ws = maybe emptyLocationInstances wgpLocationInstances ⊚ readIORef (wsGenParamsRef ws)

stampedChunks ∷ WorldState → IO (HS.HashSet ChunkCoord)
stampedChunks ws = maybe HS.empty wgpLocationStamped ⊚ readIORef (wsGenParamsRef ws)

paths ∷ [ScenarioDiagnostic] → [Text]
paths = sort ∘ map sdPath

-- | Everything a capture holds, as one comparable value.
data Doc = Doc
    { dStructures ∷ [StructurePiece], dBuildings ∷ [BuildingEntry]
    , dLocations ∷ [LocationEntry], dUnits ∷ [UnitEntry]
    , dGround ∷ [(Float, Float, ItemEntry)] }
    deriving (Show, Eq)

docOf ∷ Captured' → Doc
docOf c = Doc (cpStructures c) (cpBuildings c) (cpLocations c) (cpUnits c) (cpGround c)

-- * Buildings

buildingSpec ∷ SpecWith Fx
buildingSpec = describe "buildings" $ do

    it "places a partly charged battery with its owner, a reconstructed power node and a zero spawn counter" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "buildings:"
            , "  - {id: bat, definition: high_voltage_battery, x: 4, y: 4, power_charge: 2500}" ]
        mz ← materialize fx pid ws arena40 sc
        mzDiagnostics mz `shouldBe` []
        bm ← readIORef (buildingManagerRef (fxEnv fx))
        [(bid, b)] ← pure (HM.toList (bmInstances bm))
        -- the stored counter itself, never a derived "none"
        biSpawnRemaining b `shouldBe` 0
        Map.lookup bid (bmScenarioRefs bm) `shouldBe` Just "bat"
        nodes ← readIORef (wsPowerNodesRef ws)
        fmap pnStoredWh (nodeForBuilding bid nodes) `shouldBe` Just 2500

    it "rejects a footprint at each finite edge (partial chunks included) and keeps the valid entries" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "buildings:"
            , "  - {id: east, definition: solar_panel, x: 20, y: 5}"
            , "  - {id: west, definition: solar_panel, x: -21, y: 5}"
            , "  - {id: north, definition: solar_panel, x: 5, y: 21}"
            , "  - {id: south, definition: solar_panel, x: 5, y: -20}"
            , "  - {id: corner, definition: high_voltage_battery, x: 19, y: 20}"
            , "  - {id: origin, definition: solar_panel, x: -20, y: -19}" ]
        mz ← materialize fx pid ws arena40 sc
        paths (mzDiagnostics mz) `shouldBe` sort [ "buildings[" <> T.pack (show i) <> "]" | i ← [0, 1, 2, 3 ∷ Int] ]
        bm ← readIORef (buildingManagerRef (fxEnv fx))
        sort (Map.elems (bmScenarioRefs bm)) `shouldBe` ["corner", "origin"]
        -- the rejected buildings left no power node
        nodes ← readIORef (wsPowerNodesRef ws)
        length [ () | b ← HM.keys (bmInstances bm), Just _ ← [nodeForBuilding b nodes] ] `shouldBe` 2

    it "keeps defaults private: other RNG and allocator histories give the same page" $ \fx → do
        let env = fxEnv fx
            yaml = doc
                [ "buildings:"
                , "  - {id: crate, definition: cargo_hold_S, x: 6, y: 6, storage: [{id: bars, definition: steel_bar}]}"
                , "  - {id: bat, definition: high_voltage_battery, x: 8, y: 6}" ]
        (pid1, ws1) ← scene fx
        sc ← clean fx yaml
        _ ← materialize fx pid1 ws1 arena40 sc
        c1 ← capturePage fx pid1 ws1 arena40
        -- a different gameplay history: RNG draws and allocated item ids
        replicateM_ 53 (atomicModifyIORef' (statRNGRef env) (\g → let (_, g') = randomR (0 ∷ Int, 9) g in (g', ())))
        replicateM_ 17 (freshItemInstanceId env)
        (pid2, ws2) ← scene fx
        _ ← materialize fx pid2 ws2 arena40 sc
        c2 ← capturePage fx pid2 ws2 arena40
        docOf c2 `shouldBe` docOf c1
        length (cpBuildings c1) `shouldBe` 2

-- * Structures

structureSpec ∷ SpecWith Fx
structureSpec = describe "structures" $ do

    it "places pieces with derived levels, captures every value explicitly and round-trips twice" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "structures:"
            , "  - {id: f1, pack: dungeon_1, piece: floor, x: 5, y: 5}"
            , "  - {id: f2, pack: dungeon_1, piece: floor, x: 6, y: 5, variant: damaged}"
            , "  - {id: w1, pack: dungeon_1, piece: wall_ne, x: 5, y: 5}"
            , "  - {id: p1, pack: dungeon_1, piece: post_n, x: 5, y: 5}"
            , "  - {id: c1, pack: dungeon_1, piece: ceiling, x: 5, y: 5}" ]
        mz ← materialize fx pid ws arena40 sc
        mzDiagnostics mz `shouldBe` []
        length (mzStructures mz) `shouldBe` 5
        c1 ← capturePage fx pid ws arena40
        cpDiagnostics c1 `shouldBe` []
        length (cpStructures c1) `shouldBe` 5
        map spVariant (filter ((≡ 6) ∘ spX) (cpStructures c1)) `shouldBe` [Authored "damaged"]
        -- the derived levels, exactly: floor and wall one above the
        -- column's surface, the ceiling two above, a post at its floor
        td ← readIORef (wsTilesRef ws)
        lc ← maybe (fail "the chunk of (5, 5) is not resident") pure (lookupChunk (ChunkCoord 0 0) td)
        let surface = lcTerrainSurfaceMap lc VU.! columnIndex 5 5
            levelOf piece = listToMaybe [ z | p ← cpStructures c1, spPiece p ≡ piece, spX p ≡ 5, Authored z ← [spZ p] ]
        map levelOf ["floor", "wall_ne", "post_n", "ceiling"]
            `shouldBe` map Just [surface + 1, surface + 1, surface + 1, surface + 2]
        -- every captured piece states its level explicitly
        forM_ (cpStructures c1) $ \p → spZ p `shouldSatisfy` (\z → case z of Authored _ → True; Omitted → False)
        (s1, _) ← roundTrip fx c1
        (pid2, ws2) ← scene fx
        _ ← materialize fx pid2 ws2 arena40 s1
        c2 ← capturePage fx pid2 ws2 arena40
        docOf c2 `shouldBe` docOf c1
        (s2, _) ← roundTrip fx c2
        (pid3, ws3) ← scene fx
        _ ← materialize fx pid3 ws3 arena40 s2
        c3 ← capturePage fx pid3 ws3 arena40
        docOf c3 `shouldBe` docOf c1

    it "tells the shipped variants apart by their own facemaps and keeps each through a round trip" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "structures:"
            , "  - {id: a, pack: dungeon_1, piece: floor, x: 1, y: 1, variant: weathered}"
            , "  - {id: b, pack: dungeon_1, piece: floor, x: 2, y: 1, variant: broken}"
            , "  - {id: c, pack: dungeon_1, piece: floor, x: 3, y: 1, variant: ruined}"
            , "  - {id: d, pack: dungeon_1, piece: post_n, x: 1, y: 1, variant: ruined}"
            , "  - {id: e, pack: dungeon_1, piece: floor, x: 4, y: 1, variant: damaged}"
            , "  - {id: f, pack: dungeon_1, piece: floor, x: 5, y: 1}" ]
        mz ← materialize fx pid ws arena40 sc
        mzDiagnostics mz `shouldBe` []
        c1 ← capturePage fx pid ws arena40
        cpDiagnostics c1 `shouldBe` []
        [ (spX p, spPiece p, spVariant p) | p ← cpStructures c1 ] `shouldBe`
            [ (1, "floor", Authored "weathered"), (1, "post_n", Authored "ruined")
            , (2, "floor", Authored "broken"), (3, "floor", Authored "ruined")
            , (4, "floor", Authored "damaged"), (5, "floor", Omitted) ]
        (s1, _) ← roundTrip fx c1
        (pid2, ws2) ← scene fx
        _ ← materialize fx pid2 ws2 arena40 s1
        c2 ← capturePage fx pid2 ws2 arena40
        docOf c2 `shouldBe` docOf c1

    it "rejects a piece outside the finite map and accepts the partial edge chunk" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "structures:"
            , "  - {id: out, pack: dungeon_1, piece: floor, x: 20, y: 5}"
            , "  - {id: edge, pack: dungeon_1, piece: floor, x: 19, y: 20}" ]
        mz ← materialize fx pid ws arena40 sc
        paths (mzDiagnostics mz) `shouldBe` ["structures[0]"]
        map fst (mzStructures mz) `shouldBe` [ExplicitId "edge"]

    it "replaces the page's pieces on a second materialization and survives chunk eviction" $ \fx → do
        (pid, ws) ← scene fx
        first ← clean fx $ doc
            [ "structures:"
            , "  - {id: a, pack: dungeon_1, piece: floor, x: 3, y: 3}"
            , "  - {id: b, pack: dungeon_1, piece: floor, x: 4, y: 3}" ]
        _ ← materialize fx pid ws arena40 first
        second ← clean fx $ doc
            [ "structures:"
            , "  - {id: only, pack: dungeon_1, piece: floor, x: 9, y: 9}" ]
        _ ← materialize fx pid ws arena40 second
        c ← capturePage fx pid ws arena40
        map (\p → (spX p, spY p)) (cpStructures c) `shouldBe` [(9, 9)]
        -- evict every resident chunk: the durable log alone reproduces it
        atomicModifyIORef' (wsTilesRef ws) (\td → (td { wtdChunks = HM.empty }, ()))
        c' ← capturePage fx pid ws arena40
        docOf c' `shouldBe` docOf c

-- * Locations

ruin ∷ String → Int → Int → String
ruin i x y = "  - {id: " ⧺ i ⧺ ", definition: ruin_small, x: " ⧺ show x ⧺ ", y: " ⧺ show y ⧺ "}"

locationSpec ∷ SpecWith Fx
locationSpec = describe "locations" $ do

    it "makes an empty ruin a real instance at its anchor with a zero roster and no overlay entry" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc ["locations:", "  - {id: empty, definition: ruin_small, x: 10, y: 10, significant_items: {}}"]
        mz ← materialize fx pid ws arena40 sc
        mzDiagnostics mz `shouldBe` []
        lis ← pageLocations ws
        [inst] ← pure (instancesToList lis)
        (liDefId inst, liAnchor inst) `shouldBe` ("ruin_small", (10, 10))
        fmap leRolledCount (liEncounter inst) `shouldBe` Just 0
        fmap (length ∘ leOccupants) (liEncounter inst) `shouldBe` Just 0
        -- the anchor chunk's geometry marker; no implicit spawn can follow
        stampedChunks ws `shouldReturn` HS.singleton (ChunkCoord 0 0)
        -- nothing is owed: the guaranteed item the definition would spawn is
        -- not part of this scenario's physical baseline, so the obligation is
        -- dropped (nothing would ever fill it) and the contents are complete
        (liSignificant inst, liContentsSpawned inst) `shouldBe` ([], True)
        locationClearanceSatisfied inst `shouldBe` True
        Map.lookup (liId inst) (lisScenarioRefs lis) `shouldBe` Just "empty"

    it "fills an occupied ruin from the units that join it and the items it binds" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "locations:"
            , "  - {id: ruin, definition: ruin_small, x: 10, y: 10, significant_items: {1: gem}}"
            , "units:"
            , "  - {id: n1, definition: nomad_primitive, x: 10.5, y: 10.5, encounter: ruin}"
            , "  - {id: n2, definition: nomad_primitive, x: 11.5, y: 9.5, encounter: ruin}"
            , "ground_items:"
            , "  - {id: gem, definition: processing_unit, x: 10.5, y: 10.5}" ]
        mz ← materialize fx pid ws arena40 sc
        mzDiagnostics mz `shouldBe` []
        [inst] ← instancesToList ⊚ pageLocations ws
        fmap (length ∘ leOccupants) (liEncounter inst) `shouldBe` Just 2
        fmap leRolledCount (liEncounter inst) `shouldBe` Just 2
        map lsiInstanceId (liSignificant inst) `shouldSatisfy` all isJust
        liContentsSpawned inst `shouldBe` True

    it "holds two instances in one chunk and a footprint crossing chunks" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc ["locations:", ruin "a" 5 5, ruin "b" 9 9, ruin "cross" 15 15]
        mz ← materialize fx pid ws arena40 sc
        mzDiagnostics mz `shouldBe` []
        lis ← pageLocations ws
        sort (map liAnchor (instancesToList lis)) `shouldBe` [(5, 5), (9, 9), (15, 15)]

    it "rejects a location whose whole footprint is not inside the map, with its would-be occupants" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "locations:"
            , ruin "low" (-19) 0
            , ruin "high" 18 0
            , ruin "ok" 17 0
            , "units:"
            , "  - {id: lost, definition: nomad_primitive, x: -19, y: 0, encounter: low}"
            , "  - {id: kept, definition: nomad_primitive, x: 17, y: 0, encounter: ok}" ]
        mz ← materialize fx pid ws arena40 sc
        paths (mzDiagnostics mz) `shouldBe` ["locations[0]", "locations[1]", "units[0].encounter"]
        lis ← pageLocations ws
        map liAnchor (instancesToList lis) `shouldBe` [(17, 0)]
        concatMap (maybe [] (pure ∘ length ∘ leOccupants) ∘ liEncounter) (instancesToList lis) `shouldBe` [1]

    it "names omitted ids and colliding spellings document-wide, so a recapture keeps every identity" $ \fx → do
        (pid, ws) ← scene fx
        -- the first building and the location carry no id (automatic
        -- references buildings[0] / locations[0]); an explicit building id
        -- and a ground item take the spellings their bracketless forms
        -- would want; a unit without an id takes units.0
        sc ← clean fx $ doc
            [ "buildings:"
            , "  - {definition: high_voltage_battery, x: 4, y: 4}"
            , "  - {id: buildings.0, definition: solar_panel, x: 5, y: 4}"
            , "locations:"
            , "  - {definition: ruin_small, x: 0, y: 0, significant_items: {}, incidental_content: false}"
            , "units:"
            , "  - {definition: nomad_primitive, x: 9.5, y: 9.5}"
            , "ground_items:"
            , "  - {id: locations.0, definition: steel_bar, x: 1.5, y: 1.5}" ]
        mzDiagnostics ⊚ materialize fx pid ws arena40 sc `shouldReturn` []
        c1 ← capturePage fx pid ws arena40
        let ids = map scenarioIdText
        ids (map beId (cpBuildings c1)) `shouldBe` ["buildings.0-2", "buildings.0"]
        -- another entry's id (tier 1) outranks an item's reference: the
        -- location keeps locations.0 and the item takes the next free name
        ids (map leId (cpLocations c1)) `shouldBe` ["locations.0"]
        ids [ ieId e | (_, _, e) ← cpGround c1 ] `shouldBe` ["locations.0-2"]
        ids (map ueId (cpUnits c1)) `shouldBe` ["units.0"]
        -- every id is legal (the document decodes clean) and the identities
        -- survive two further cycles
        (s1, _) ← roundTrip fx c1
        (pid2, ws2) ← scene fx
        _ ← materialize fx pid2 ws2 arena40 s1
        c2 ← capturePage fx pid2 ws2 arena40
        docOf c2 `shouldBe` docOf c1
        (s2, _) ← roundTrip fx c2
        (pid3, ws3) ← scene fx
        _ ← materialize fx pid3 ws3 arena40 s2
        docOf ⊚ capturePage fx pid3 ws3 arena40 `shouldReturn` docOf c1

-- * Round trips

roundTripSpec ∷ SpecWith Fx
roundTripSpec = describe "capture" $ do

    it "round-trips an occupied ruin twice through YAML" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "locations:"
            , "  - {id: ruin, definition: ruin_small, x: 10, y: 10, significant_items: {1: gem}}"
            , "units:"
            , "  - {id: n1, definition: nomad_primitive, x: 10.5, y: 10.5, encounter: ruin}"
            , "ground_items:"
            , "  - {id: gem, definition: processing_unit, x: 10.5, y: 10.5}" ]
        _ ← materialize fx pid ws arena40 sc
        c1 ← capturePage fx pid ws arena40
        (s1, bytes) ← roundTrip fx c1
        BC.length bytes `shouldSatisfy` (> 0)
        (pid2, ws2) ← scene fx
        _ ← materialize fx pid2 ws2 arena40 s1
        c2 ← capturePage fx pid2 ws2 arena40
        docOf c2 `shouldBe` docOf c1

-- * Containers

-- | The document the container examples share: a crypt owing one
--   significant item and two container shells, bound to ground items.
cryptDoc ∷ String → String
cryptDoc containers = doc
    [ "locations:"
    , "  - {id: crypt, definition: crypt_fx, x: 0, y: 0, significant_items: {1: key}, incidental_content: false, containers: " ⧺ containers ⧺ "}"
    , "ground_items:"
    , "  - {id: key, definition: processing_unit, x: 0.5, y: 0.5}"
    , "  - {id: box1, definition: field_toolbox, x: 0.5, y: 1.5}"
    , "  - {id: box2, definition: field_toolbox, x: 1.5, y: 1.5, contents: []}" ]

groundCount ∷ WorldState → IO Int
groundCount ws = HM.size ∘ gisItems ⊚ readIORef (wsGroundItemsRef ws)

containerSpec ∷ SpecWith Fx
containerSpec = describe "containers" $ do

    it "binds a pending and a realized shell to the real items, never opening or rolling one" $ \fx → do
        (pid, ws) ← scene fx
        before ← readIORef (gameTimeRef (fxEnv fx))
        sc ← clean fx (cryptDoc "{1: {item: box1}, 2: {item: box2, realized: true}}")
        mz ← materialize fx pid ws arena40 sc
        mzDiagnostics mz `shouldBe` []
        [inst] ← instancesToList ⊚ pageLocations ws
        wm ← readIORef (worldManagerRef (fxEnv fx))
        let idOf ref = [ i | (i, r) ← itemRefsToAscList (wmItemScenarioRefs wm), r ≡ ref ]
        [pending, realized] ← pure (sortOn lcsSlot (liContainers inst))
        -- pending: the shell's identity and the definition's own profile
        (lcsRealized pending, lcsProfile pending, Just `fmap` listToMaybe (idOf "box1"))
            `shouldBe` (False, Just "ruin_industrial_salvage", Just (lcsInstanceId pending))
        -- realized: latched, its profile discarded, exactly the stated contents
        (lcsRealized realized, lcsProfile realized, Just `fmap` listToMaybe (idOf "box2"))
            `shouldBe` (True, Nothing, Just (lcsInstanceId realized))
        -- nothing was drawn: the three stated items are all there is, no
        -- implicit spawn followed, and the clock did not move
        groundCount ws `shouldReturn` 3
        liContentsSpawned inst `shouldBe` True
        readIORef (gameTimeRef (fxEnv fx)) `shouldReturn` before

    it "keeps a stated profile on a pending slot and round-trips the whole crypt twice" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx (cryptDoc "{1: {item: box1, profile: ruin_industrial_salvage}, 2: {item: box2, realized: true}}")
        _ ← materialize fx pid ws arena40 sc
        c1 ← capturePage fx pid ws arena40
        map leContainers (cpLocations c1) `shouldSatisfy` \cs → case cs of
            [Authored m] → sort (HM.keys m) == [1, 2]
            _ → False
        (s1, _) ← roundTrip fx c1
        (pid2, ws2) ← scene fx
        _ ← materialize fx pid2 ws2 arena40 s1
        c2 ← capturePage fx pid2 ws2 arena40
        docOf c2 `shouldBe` docOf c1
        (s2, _) ← roundTrip fx c2
        (pid3, ws3) ← scene fx
        _ ← materialize fx pid3 ws3 arena40 s2
        docOf ⊚ capturePage fx pid3 ws3 arena40 `shouldReturn` docOf c1

-- * References

referenceSpec ∷ SpecWith Fx
referenceSpec = describe "optional references" $ do

    it "drops a binding to an item of a rejected building, keeping the location and the slot unbound" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "buildings:"
            , "  - {id: gone, definition: cargo_hold_S, x: 99, y: 0, storage: [{id: boxed, definition: processing_unit}]}"
            , "locations:"
            , "  - {id: ruin, definition: ruin_small, x: 0, y: 0, significant_items: {1: boxed}}" ]
        mz ← materialize fx pid ws arena40 sc
        [(p, r, e)] ← pure [ (sdPath d, sdReason d, sdEffect d) | d ← mzDiagnostics mz, sdPath d ≡ "locations[0].significant_items.1" ]
        (p, r, e) `shouldBe` ("locations[0].significant_items.1", RejectedReference "boxed", FieldRejected)
        [inst] ← instancesToList ⊚ pageLocations ws
        (liSignificant inst, liContentsSpawned inst) `shouldBe` ([], True)

    it "drops a binding to an item of the wrong definition" $ \fx → do
        (pid, ws) ← scene fx
        (sc, ds) ← decodeDocument (fxEnv fx) $ doc
            [ "locations:"
            , "  - {id: ruin, definition: ruin_small, x: 0, y: 0, significant_items: {1: bar}}"
            , "ground_items:"
            , "  - {id: bar, definition: steel_bar, x: 0.5, y: 0.5}" ]
        mz ← materialize fx pid ws arena40 sc
        map (\d → (sdPath d, sdReason d)) (ds ⧺ mzDiagnostics mz)
            `shouldSatisfy` any ((≡ "locations[0].significant_items.1") ∘ fst)
        [inst] ← instancesToList ⊚ pageLocations ws
        (liSignificant inst, liContentsSpawned inst) `shouldBe` ([], True)

    it "rejects an unknown definition, and a unit joining a location with no encounter" $ \fx → do
        (_, ds) ← decodeDocument (fxEnv fx) $ doc
            [ "locations:"
            , "  - {id: q, definition: quiet_fx, x: 0, y: 0}"
            , "  - {id: nope, definition: no_such_ruin, x: 4, y: 4}"
            , "units:"
            , "  - {id: u, definition: nomad_primitive, x: 0, y: 0, encounter: q}" ]
        map (\d → (sdPath d, sdReason d, sdEffect d)) ds `shouldBe`
            [ ("locations[1].definition", UnknownDefinition "no_such_ruin", EntryRejected)
            , ("units[0].encounter", NoEncounter "quiet_fx", EntryRejected) ]

-- * Lifecycle

lifecycleSpec ∷ SpecWith Fx
lifecycleSpec = describe "lifecycle" $ do

    it "imports no progress: an unknown, unactivated instance with a seated clearance notice" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "locations:"
            , "  - {id: ruin, definition: ruin_small, x: 0, y: 0}"
            , "units:"
            , "  - {id: n1, definition: nomad_primitive, x: 0.5, y: 0.5, encounter: ruin}" ]
        _ ← materialize fx pid ws arena40 sc
        [inst] ← instancesToList ⊚ pageLocations ws
        liLifecycle inst `shouldBe` LifecycleUnknown
        fmap (\e → (leActivated e, leEpisodeActive e, leRosterComplete e)) (liEncounter inst)
            `shouldBe` Just (False, False, True)

    it "a second initialization replaces the first exactly and the removed ruin stays absent" $ \fx → do
        (pid, ws) ← scene fx
        full ← clean fx $ doc
            [ "structures:"
            , "  - {id: f, pack: dungeon_1, piece: floor, x: 0, y: 0}"
            , "locations:"
            , "  - {id: ruin, definition: ruin_small, x: 0, y: 0}" ]
        _ ← materialize fx pid ws arena40 full
        c1 ← capturePage fx pid ws arena40
        resetRegistries fx
        _ ← materialize fx pid ws arena40 full
        c2 ← capturePage fx pid ws arena40
        docOf c2 `shouldBe` docOf c1
        -- an explicitly empty scenario removes the geometry and the ruin
        -- and they stay gone across eviction and a further initialization
        empty ← clean fx (doc [])
        resetRegistries fx
        _ ← materialize fx pid ws arena40 empty
        atomicModifyIORef' (wsTilesRef ws) (\td → (td { wtdChunks = HM.empty }, ()))
        resetRegistries fx
        _ ← materialize fx pid ws arena40 empty
        c3 ← capturePage fx pid ws arena40
        (cpStructures c3, cpLocations c3) `shouldBe` ([], [])
        pageLocations ws ≫= (`shouldBe` []) ∘ instancesToList
        stampedChunks ws `shouldReturn` HS.empty

-- | Omitted versus stated guaranteed content (requirement 5).
defaultsSpec ∷ SpecWith Fx
defaultsSpec = describe "definition-provided content: omitted versus stated" $ do

    let groundDefs ws = map (iiDefName ∘ G.giInst) ∘ HM.elems ∘ gisItems <$> readIORef (wsGroundItemsRef ws)

    it "constructs the guaranteed item an omitted binding map owes, once, bound and untaken, at a deterministic tile" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc ["locations:", "  - {id: ruin, definition: ruin_small, x: 10, y: 10, incidental_content: false}"]
        mzDiagnostics ⊚ materialize fx pid ws arena40 sc `shouldReturn` []
        [inst] ← instancesToList ⊚ pageLocations ws
        [ (lsiItemDefName e, isJust (lsiInstanceId e), lsiTaken e) | e ← liSignificant inst ]
            `shouldBe` [("processing_unit", True, False)]
        liContentsSpawned inst `shouldBe` True
        groundDefs ws `shouldReturn` ["processing_unit"]
        gi ← readIORef (wsGroundItemsRef ws)
        -- inside the footprint (anchor 10,10 with bounds +-2)
        [ (floor (G.giX g) ∷ Int, floor (G.giY g) ∷ Int) | g ← HM.elems (gisItems gi) ]
            `shouldSatisfy` all (\(x, y) → x ≥ 8 ∧ x ≤ 12 ∧ y ≥ 8 ∧ y ≤ 12)
        -- another history and page: the same tile
        replicateM_ 31 (atomicModifyIORef' (statRNGRef (fxEnv fx)) (\g → let (_, g') = randomR (0 ∷ Int, 9) g in (g', ())))
        (pid2, ws2) ← scene fx
        _ ← materialize fx pid2 ws2 arena40 sc
        gi2 ← readIORef (wsGroundItemsRef ws2)
        sort [ (G.giX g, G.giY g) | g ← HM.elems (gisItems gi2) ] `shouldBe` sort [ (G.giX g, G.giY g) | g ← HM.elems (gisItems gi) ]

    it "recaptures the construction as stated content, so a further cycle adds no second copy" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc ["locations:", "  - {id: ruin, definition: ruin_small, x: 10, y: 10}"]
        _ ← materialize fx pid ws arena40 sc
        c1 ← capturePage fx pid ws arena40
        -- the guaranteed item and the two rolled finds, all stated now
        length (cpGround c1) `shouldBe` 3
        map leSignificant (cpLocations c1) `shouldSatisfy` \ms → case ms of [Authored m] → HM.size m ≡ 1; _ → False
        map leIncidental (cpLocations c1) `shouldBe` [Authored False]
        (s1, _) ← roundTrip fx c1
        (pid2, ws2) ← scene fx
        _ ← materialize fx pid2 ws2 arena40 s1
        c2 ← capturePage fx pid2 ws2 arena40
        docOf c2 `shouldBe` docOf c1
        (s2, _) ← roundTrip fx c2
        (pid3, ws3) ← scene fx
        _ ← materialize fx pid3 ws3 arena40 s2
        docOf ⊚ capturePage fx pid3 ws3 arena40 `shouldReturn` docOf c1
        length ⊚ groundDefs ws3 `shouldReturn` 3

    it "keeps a stated empty map empty, and stays empty through a recapture" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc ["locations:", "  - {id: bare, definition: ruin_small, x: 10, y: 10, significant_items: {}, incidental_content: false}"]
        _ ← materialize fx pid ws arena40 sc
        groundDefs ws `shouldReturn` []
        c1 ← capturePage fx pid ws arena40
        map leSignificant (cpLocations c1) `shouldBe` [Authored HM.empty]
        (s1, _) ← roundTrip fx c1
        (pid2, ws2) ← scene fx
        _ ← materialize fx pid2 ws2 arena40 s1
        groundDefs ws2 `shouldReturn` []

    it "constructs the pending shells an omitted container map owes, unopened, and nothing for a rejected location" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "locations:"
            , "  - {id: ok, definition: crypt_fx, x: 0, y: 0, significant_items: {}, incidental_content: false}"
            , "  - {id: outside, definition: crypt_fx, x: 30, y: 0}" ]
        mz ← materialize fx pid ws arena40 sc
        map sdPath (mzDiagnostics mz) `shouldBe` ["locations[1]"]
        groundDefs ws ≫= (`shouldBe` ["field_toolbox", "field_toolbox"])
        [inst] ← instancesToList ⊚ pageLocations ws
        [ (lcsRealized c, isJust (lcsInstanceId c), lcsProfile c) | c ← sortOn lcsSlot (liContainers inst) ]
            `shouldBe` [ (False, True, Just "ruin_industrial_salvage"), (False, True, Just "ruin_industrial_salvage") ]
        -- unopened: no shell holds anything the loot profile would have rolled
        -- (a toolbox's own starting kit is the item definition's, not loot)
        gi ← readIORef (wsGroundItemsRef ws)
        let held = [ iiDefName c | g ← HM.elems (gisItems gi), c ← iiContents (G.giInst g) ]
        filter (`elem` ["steel_bar", "electric_motor", "steel_hardware"]) held `shouldBe` []

-- | Each definition x omitted / stated-empty / bound / rejected, against an
--   expectation computed from the definition and the SAME private rolls an
--   ordinary placement makes ('rollLootTableFor' over the scenario-folded
--   context), independent of the construction code.
matrixSpec ∷ SpecWith Fx
matrixSpec = describe "definition x omitted / stated empty / bound / rejected" $
    forM_ ["ruin_small", "crypt_fx", "quiet_fx"] $ \defName → describe (T.unpack defName) $ do

        let groundNames ws = sort ∘ map (iiDefName ∘ G.giInst) ∘ HM.elems ∘ gisItems <$> readIORef (wsGroundItemsRef ws)
            expectedFor fx = do
                def' ← locationRegistry (fxEnv fx) ≫= \r → maybe (fail "no definition") pure (lookupLocation defName r)
                tables ← lootTables (fxEnv fx)
                let sig = map lconId (significantContentsFromDef def')
                    shells = map (lconId ∘ fst) (containerContentsFromDef def')
                    fixed = concat [ replicate (lconCount c) (lconId c)
                                   | c ← ldContents def', lconKind c ≡ "item", not (lconSignificant c) ]
                    rolled = [ d | (ix, c) ← zip [1 ∷ Int ..] (ldContents def'), lconKind c ≡ "loot_table"
                                 , r ← [1 .. lconRolls c], Just t ← [lookupLootTable (lconId c) tables]
                                 , Just d ← [rollLootTableFor t (LootRollContext 0 (fromIntegral (stableHash64 "m")) ix r)] ]
                pure (sort sig, sort shells, sort fixed, sort rolled)
            entry extra = "  - {id: m, definition: " ⧺ T.unpack defName ⧺ ", x: 0, y: 0" ⧺ extra ⧺ "}"

        it "omitted: constructs exactly the definition's non-unit content, once, with the ordinary rolls" $ \fx → do
            (pid, ws) ← scene fx
            sc ← clean fx (doc ["locations:", entry ""])
            mzDiagnostics ⊚ materialize fx pid ws arena40 sc `shouldReturn` []
            (sig, shells, fixed, rolled) ← expectedFor fx
            groundNames ws `shouldReturn` sort (sig ⧺ shells ⧺ fixed ⧺ rolled)
            -- the rolled finds are the pinned draw, not a coincidence of the table
            length rolled `shouldSatisfy` (> 0)

        it "stated empty: constructs nothing, and a recapture stays empty" $ \fx → do
            (pid, ws) ← scene fx
            sc ← clean fx (doc ["locations:", entry ", significant_items: {}, containers: {}, incidental_content: false"])
            _ ← materialize fx pid ws arena40 sc
            groundNames ws `shouldReturn` []
            c1 ← capturePage fx pid ws arena40
            (s1, _) ← roundTrip fx c1
            (pid2, ws2) ← scene fx
            _ ← materialize fx pid2 ws2 arena40 s1
            groundNames ws2 `shouldReturn` []

        it "bound: only the stated items exist, bound as stated" $ \fx → do
            (pid, ws) ← scene fx
            sc ← clean fx $ doc
                [ "locations:", entry ", significant_items: {1: gem}, containers: {}, incidental_content: false"
                , "ground_items:", "  - {id: gem, definition: processing_unit, x: 0.5, y: 0.5}" ]
            mzDiagnostics ⊚ materialize fx pid ws arena40 sc `shouldReturn` []
            groundNames ws `shouldReturn` ["processing_unit"]
            [inst] ← instancesToList ⊚ pageLocations ws
            map (isJust ∘ lsiInstanceId) (liSignificant inst) `shouldBe` [True]

        it "rejected: a location outside the map constructs nothing" $ \fx → do
            (pid, ws) ← scene fx
            sc ← clean fx (doc ["locations:", "  - {id: m, definition: " ⧺ T.unpack defName ⧺ ", x: 30, y: 0}"])
            mz ← materialize fx pid ws arena40 sc
            map sdPath (mzDiagnostics mz) `shouldBe` ["locations[0]"]
            groundNames ws `shouldReturn` []

-- | The clearance notice is seated against the FINAL instance: a location
--   already satisfied when initialization completes (nothing owed and no
--   living occupant, or every owed item already taken) starts with it
--   spent, so discovering it announces nothing; one that is not satisfied
--   announces exactly once when it later is.
noticeSpec ∷ SpecWith Fx
noticeSpec = describe "initial clearance notice" $ do

    let discovered inst = inst { liLifecycle = locationDiscoveryLifecycle inst }
        eventOwed lis inst = isJust (resolveLocationClearance (liId inst) (adjustLocationInstance (liId inst) (const (discovered inst)) lis))

    it "is spent for an empty ruin, so first discovery emits no clearance event" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc ["locations:", "  - {id: bare, definition: ruin_small, x: 10, y: 10, significant_items: {}}"]
        _ ← materialize fx pid ws arena40 sc
        lis ← pageLocations ws
        [inst] ← pure (instancesToList lis)
        locationClearanceSatisfied inst `shouldBe` True
        liClearEventEmitted inst `shouldBe` True
        eventOwed lis inst `shouldBe` False

    it "is spent when the owed item was bound already taken, and unspent while an item is still owed" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "buildings:"
            , "  - {id: chest, definition: cargo_hold_S, x: 3, y: 3, storage: [{id: held, definition: processing_unit}]}"
            , "locations:"
            , "  - {id: taken, definition: ruin_small, x: -10, y: 0, significant_items: {1: held}}"
            , "  - {id: owing, definition: ruin_small, x: 10, y: 0}" ]
        mzDiagnostics ⊚ materialize fx pid ws arena40 sc `shouldReturn` []
        lis ← pageLocations ws
        let at x = case [ i | i ← instancesToList lis, fst (liAnchor i) ≡ x ] of
                (i : _) → i
                [] → error "no such location"
            takenI = at (-10)
            owingI = at 10
        (liClearEventEmitted takenI, eventOwed lis takenI) `shouldBe` (True, False)
        (liClearEventEmitted owingI, locationClearanceSatisfied owingI) `shouldBe` (False, False)
        -- once the owed item is picked up, discovery owes exactly one event
        Just gemId ← pure (listToMaybe [ g | e ← liSignificant owingI, Just g ← [lsiInstanceId e] ])
        Just latched ← pure (latchLocationSignificantTaken gemId lis)
        fmap (\i → (locationClearanceSatisfied i, eventOwed latched i)) (lookupLocationInstance (liId owingI) latched)
            `shouldBe` Just (True, True)

-- | Ownership and completion of location bindings, through the save
--   boundary's own integrity rules.
bindingSpec ∷ SpecWith Fx
bindingSpec = describe "location bindings: ownership and completion" $ do

    it "derives a taken significant item for one held in storage, refuses a pending shell that is not on the ground, and accepts a held realized shell" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "buildings:"
            , "  - id: chest"
            , "    definition: cargo_hold_S"
            , "    x: 3"
            , "    y: 3"
            , "    storage:"
            , "      - {id: held, definition: processing_unit}"
            , "      - {id: boxStored, definition: field_toolbox}"
            , "      - {id: boxRealized, definition: field_toolbox}"
            , "locations:"
            , "  - {id: crypt, definition: crypt_fx, x: 0, y: 0, significant_items: {1: held}, containers: {1: {item: boxStored}, 2: {item: boxRealized, realized: true}}}" ]
        mz ← materialize fx pid ws arena40 sc
        [ (sdPath d, sdEffect d) | d ← mzDiagnostics mz ]
            `shouldBe` [("locations[0].containers.1.item", FieldRejected)]
        [inst] ← instancesToList ⊚ pageLocations ws
        -- the held item is bound and latched taken: the one valid state off the ground
        [ (lsiTaken e, isJust (lsiInstanceId e)) | e ← liSignificant inst ] `shouldBe` [(True, True)]
        [ (lcsSlot c, lcsRealized c, isJust (lcsInstanceId c)) | c ← sortOn lcsSlot (liContainers inst) ]
            `shouldBe` [(1, False, False), (2, True, True)]
        liContentsSpawned inst `shouldBe` True
        -- and the page passes the ordinary save's integrity rules
        snap ← sessionSnapshotOf fx pid ws
        [ (ieCode e, iePath e) | e ← sessionIntegrityErrors snap ] `shouldBe` []

    it "refuses a pending shell nested inside a ground item, keeping the physical item" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "locations:"
            , "  - {id: crypt, definition: crypt_fx, x: 0, y: 0, significant_items: {}, incidental_content: false, containers: {1: {item: inner}}}"
            , "ground_items:"
            , "  - {id: outer, definition: field_toolbox, x: 0.5, y: 0.5, contents: [{id: inner, definition: field_toolbox}]}" ]
        mz ← materialize fx pid ws arena40 sc
        map sdPath (mzDiagnostics mz) `shouldBe` ["locations[0].containers.1.item"]
        [inst] ← instancesToList ⊚ pageLocations ws
        [ isJust (lcsInstanceId c) | c ← liContainers inst ] `shouldBe` [False, False]
        gi ← readIORef (wsGroundItemsRef ws)
        HM.size (gisItems gi) `shouldBe` 1
        snap ← sessionSnapshotOf fx pid ws
        sessionIntegrityErrors snap `shouldBe` []

    it "never strands an obligation: clearance follows the accepted occupants, a taken item and a readmission dispatches nothing" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "locations:"
            , "  - {id: bound, definition: ruin_small, x: -10, y: 0, significant_items: {1: gem}}"
            , "  - {id: unbound, definition: ruin_small, x: 10, y: 0, significant_items: {}}"
            , "units:"
            , "  - {id: n1, definition: nomad_primitive, x: -10.5, y: 0.5, encounter: bound}"
            , "  - {id: n2, definition: nomad_primitive, x: 10.5, y: 0.5, encounter: unbound}"
            , "ground_items:"
            , "  - {id: gem, definition: processing_unit, x: -9.5, y: 0.5}" ]
        mzDiagnostics ⊚ materialize fx pid ws arena40 sc `shouldReturn` []
        lis ← pageLocations ws
        let at x = head' [ i | i ← instancesToList lis, fst (liAnchor i) ≡ x ]
            head' (y : _) = y
            head' [] = error "no such location"
            defeated i = i { liEncounter = fmap (\e → e { leCleared = True }) (liEncounter i) }
            unboundI = at 10
            boundI = at (-10)
        -- the location with no bound item owes nothing: its occupants alone decide
        (liSignificant unboundI, locationClearanceSatisfied unboundI) `shouldBe` ([], False)
        locationClearanceSatisfied (defeated unboundI) `shouldBe` True
        -- the bound one also waits for its item, until a pickup latches it
        locationClearanceSatisfied (defeated boundI) `shouldBe` False
        Just gemId ← pure (listToMaybe [ g | e ← liSignificant boundI, Just g ← [lsiInstanceId e] ])
        Just latched ← pure (latchLocationSignificantTaken gemId lis)
        fmap (locationClearanceSatisfied ∘ defeated) (lookupLocationInstance (liId boundI) latched) `shouldBe` Just True
        -- readmitting the anchor chunks dispatches no stamp and no content spawn
        params ← maybe (fail "no generation parameters") pure =≪ readIORef (wsGenParamsRef ws)
        resident ← HM.elems ∘ wtdChunks ⊚ readIORef (wsTilesRef ws)
        length resident `shouldSatisfy` (> 0)
        locationStampsFor params resident `shouldBe` []

-- * Properties

-- | A small seeded generator: the examples below draw their cases from
--   it rather than from a fixed list, and compare against an oracle
--   written from the RECTANGLE alone, never from the code under test.
draw ∷ StdGen → Int → Int → (Int, StdGen)
draw g lo hi = randomR (lo, hi) g

-- | A tile in the band around the 40x40 map (x -20..19, y -19..20) that
--   reaches two tiles beyond every finite edge.
tileOf ∷ StdGen → ((Int, Int), StdGen)
tileOf g0 = let (x, g1) = draw g0 (-22) 21
                (y, g2) = draw g1 (-21) 22
            in ((x, y), g2)

inMap ∷ (Int, Int) → Bool
inMap (x, y) = x ≥ -20 ∧ x ≤ 19 ∧ y ≥ -19 ∧ y ≤ 20

propertySpec ∷ SpecWith Fx
propertySpec = describe "properties (seeded, against a rectangle oracle)" $ do

    it "a building is placed exactly when its tile is inside the map and free" $ \fx → do
        forM_ [1 .. 20 ∷ Int] $ \seed → do
            let tiles = take 12 (unfoldTiles (mkStdGen seed))
                entry i (x, y) = "  - {id: b" ⧺ show (i ∷ Int) ⧺ ", definition: solar_panel, x: "
                                 ⧺ show x ⧺ ", y: " ⧺ show y ⧺ "}"
                expected = foldl' (\acc t → if inMap t ∧ t `notElem` acc then acc ⧺ [t] else acc) [] tiles
            (pid, ws) ← scene fx
            sc ← clean fx (doc ("buildings:" : zipWith entry [0 ..] tiles))
            _ ← materialize fx pid ws arena40 sc
            c ← capturePage fx pid ws arena40
            (seed, sort (map (\b → (beX b, beY b)) (cpBuildings c))) `shouldBe` (seed, sort expected)
            bm ← readIORef (buildingManagerRef (fxEnv fx))
            -- no reservation outlives the commit, and one node per building
            (seed, HM.size (bmReservations bm), HM.size (bmInstances bm)) `shouldBe` (seed, 0, length expected)
            nodes ← readIORef (wsPowerNodesRef ws)
            (seed, length [ () | b ← HM.keys (bmInstances bm), Just _ ← [nodeForBuilding b nodes] ])
                `shouldBe` (seed, length expected)

    it "a location is placed exactly when its whole footprint is inside the map" $ \fx → do
        forM_ [1 .. 20 ∷ Int] $ \seed → do
            let anchors = take 5 (unfoldTiles (mkStdGen (1000 + seed)))
                entry i (x, y) = ruin ("r" ⧺ show (i ∷ Int)) x y
                whole (x, y) = inMap (x - 2, y - 2) ∧ inMap (x + 2, y + 2)
                expected = filter whole anchors
            (pid, ws) ← scene fx
            sc ← clean fx (doc ("locations:" : zipWith entry [0 ..] anchors))
            _ ← materialize fx pid ws arena40 sc
            lis ← pageLocations ws
            (seed, sort (map liAnchor (instancesToList lis))) `shouldBe` (seed, sort expected)
            -- the geometry marker is exactly the set of anchor chunks
            stamped ← stampedChunks ws
            (seed, stamped) `shouldBe` (seed, HS.fromList [ ChunkCoord (x `div` 16) (y `div` 16) | (x, y) ← expected ])

    it "structure pieces round-trip twice from any seeded layout, replacing duplicates by last" $ \fx → do
        forM_ [1 .. 12 ∷ Int] $ \seed → do
            let (tiles, slots) = layout (mkStdGen (2000 + seed))
                entry i ((x, y), (slot, variant)) =
                    "  - {id: s" ⧺ show (i ∷ Int) ⧺ ", pack: dungeon_1, piece: " ⧺ slot ⧺ ", x: " ⧺ show x
                        ⧺ ", y: " ⧺ show y ⧺ variant ⧺ "}"
                pieces = zip tiles slots
                expected = Map.fromList [ ((t, slot), ()) | (t, (slot, _)) ← pieces, inMap t ]
            (pid, ws) ← scene fx
            sc ← clean fx (doc ("structures:" : zipWith entry [0 ..] pieces))
            _ ← materialize fx pid ws arena40 sc
            c1 ← capturePage fx pid ws arena40
            (seed, Map.fromList [ (((spX p, spY p), spPiece p), ()) | p ← cpStructures c1 ])
                `shouldBe` (seed, Map.mapKeys (\(t, sl) → (t, T.pack sl)) expected)
            (s1, _) ← roundTrip fx c1
            (pid2, ws2) ← scene fx
            _ ← materialize fx pid2 ws2 arena40 s1
            c2 ← capturePage fx pid2 ws2 arena40
            (seed, docOf c2) `shouldBe` (seed, docOf c1)
            (s2, _) ← roundTrip fx c2
            (pid3, ws3) ← scene fx
            _ ← materialize fx pid3 ws3 arena40 s2
            c3 ← capturePage fx pid3 ws3 arena40
            (seed, docOf c3) `shouldBe` (seed, docOf c1)
  where
    unfoldTiles g = let (t, g') = tileOf g in t : unfoldTiles g'
    layout g0 =
        let n = 14
            tilesAndSlots 0 _ = []
            tilesAndSlots k g =
                let (t, g1) = tileOf g
                    (si, g2) = draw g1 0 6
                    (vi, g3) = draw g2 0 1
                    slot = ["floor", "ceiling", "wall_ne", "wall_nw", "post_n", "post_s", "wall_se"] !! si
                    variant = if vi ≡ 1 ∧ slot ≢ "ceiling" then ", variant: damaged" else ""
                in (t, (slot, variant)) : tilesAndSlots (k - 1 ∷ Int) g3
            xs = tilesAndSlots n g0
        in (map fst xs, map snd xs)

-- * Building contents

storageSpec ∷ SpecWith Fx
storageSpec = describe "building contents" $ do

    it "restores nested, explicitly empty and overloaded storage, delivered materials and build progress, twice over" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "buildings:"
            , "  - id: nested"
            , "    definition: cargo_hold_S"
            , "    x: 0"
            , "    y: 0"
            , "    storage:"
            , "      - {id: kit, definition: field_toolbox, contents: [{id: bar, definition: steel_bar}]}"
            , "  - {id: bare, definition: cargo_hold_S, x: 2, y: 0, storage: []}"
            , "  - id: overloaded"
            , "    definition: cargo_hold_S"
            , "    x: 4"
            , "    y: 0"
            , "    storage: [" ⧺ intercalate ", " [ "{id: p" ⧺ show n ⧺ ", definition: steel_plate}" | n ← [1 .. 40 ∷ Int] ] ⧺ "]"
            , "  - id: partial"
            , "    definition: workbench"
            , "    x: 6"
            , "    y: 0"
            , "    build_progress: 10"
            , "    materials_delivered: [{id: wood, definition: wood_log}]" ]
        mz ← materialize fx pid ws arena40 sc
        mzDiagnostics mz `shouldBe` []
        c1 ← capturePage fx pid ws arena40
        let bySpot = Map.fromList [ ((beX b, beY b), b) | b ← cpBuildings c1 ]
        -- an explicitly empty storage stays explicit; progress and materials are stated
        fmap beStorage (Map.lookup (2, 0) bySpot) `shouldBe` Just (Authored [])
        fmap beBuildProgress (Map.lookup (6, 0) bySpot) `shouldBe` Just (Authored 10)
        fmap (length ∘ authoredOr [] ∘ beMaterialsDelivered) (Map.lookup (6, 0) bySpot) `shouldBe` Just 1
        fmap (length ∘ authoredOr [] ∘ beStorage) (Map.lookup (4, 0) bySpot) `shouldBe` Just 40
        (s1, _) ← roundTrip fx c1
        (pid2, ws2) ← scene fx
        _ ← materialize fx pid2 ws2 arena40 s1
        c2 ← capturePage fx pid2 ws2 arena40
        docOf c2 `shouldBe` docOf c1
        (s2, _) ← roundTrip fx c2
        (pid3, ws3) ← scene fx
        _ ← materialize fx pid3 ws3 arena40 s2
        docOf ⊚ capturePage fx pid3 ws3 arena40 `shouldReturn` docOf c1

    it "retires a building whose item references cannot be held, leaving nothing of it behind" $ \fx → do
        (pid1, ws1) ← scene fx
        first ← clean fx $ doc
            [ "buildings:"
            , "  - {id: crate1, definition: cargo_hold_S, x: 0, y: 0, storage: [{id: bars, definition: steel_bar}]}" ]
        mzDiagnostics ⊚ materialize fx pid1 ws1 arena40 first `shouldReturn` []
        -- a second page in the same session: scenario references are unique
        -- per session, so "bars" is already held and the commit of the item
        -- trees is refused after the footprint was committed
        (pid2, ws2) ← newArena (fxEnv fx) "placement_second" 40 40
        second ← clean fx $ doc
            [ "buildings:"
            , "  - id: crate2"
            , "    tags: [doomed]"
            , "    definition: cargo_hold_S"
            , "    x: 2"
            , "    y: 0"
            , "    storage: [{id: bars, definition: steel_bar}]" ]
        mz ← materialize fx pid2 ws2 arena40 second
        [ case c of BuildingNotInstalled r (BuildingItemsRefused _) → Just r; _ → Nothing | c ← mzBuildings mz ]
            `shouldBe` [Just "crate2"]
        bm ← readIORef (buildingManagerRef (fxEnv fx))
        Map.elems (bmScenarioRefs bm) `shouldBe` ["crate1"]
        (HM.size (bmInstances bm), HM.size (bmReservations bm)) `shouldBe` (1, 0)
        categoryTagsOf' bm `shouldBe` []
        ds ← pure (mzDiagnostics mz)
        ds `shouldBe` []
  where
    categoryTagsOf' bm = [ () | b ← HM.keys (bmInstances bm), not (Set.null (categoryTagsOf b (bmTags bm))) ]

-- * Ordinary saves

saveSpec ∷ SpecWith Fx
saveSpec = describe "an ordinary save (no scenario document)" $ do

    it "restores through the real codecs and stage, and a capture of the restored state equals the original" $ \fx → do
        (pid, ws) ← scene fx
        sc ← clean fx $ doc
            [ "structures:"
            , "  - {id: f, pack: dungeon_1, piece: floor, x: 3, y: 3, variant: damaged}"
            , "  - {id: w, pack: dungeon_1, piece: wall_ne, x: 3, y: 3}"
            , "buildings:"
            , "  - {id: bat, definition: high_voltage_battery, x: 8, y: 8, power_charge: 1000}"
            , "  - {id: crate, definition: cargo_hold_S, x: 9, y: 8, storage: [{id: bars, definition: steel_bar}]}"
            , "locations:"
            , "  - {id: crypt, definition: crypt_fx, x: 0, y: 0, significant_items: {1: key}, containers: {1: {item: box1}, 2: {item: box2, realized: true}}}"
            , "units:"
            , "  - {id: n1, definition: nomad_primitive, x: 0.5, y: 0.5, encounter: crypt}"
            , "ground_items:"
            , "  - {id: key, definition: processing_unit, x: 0.5, y: 0.5}"
            , "  - {id: box1, definition: field_toolbox, x: 0.5, y: 1.5}"
            , "  - {id: box2, definition: field_toolbox, x: 1.5, y: 1.5, contents: []}" ]
        mz ← materialize fx pid ws arena40 sc
        mzDiagnostics mz `shouldBe` []
        before ← capturePage fx pid ws arena40
        src ← saveAndStage fx pid ws
        after ← capturePageFrom fx src arena40
        dStructures (docOf after) `shouldBe` dStructures (docOf before)
        length (dBuildings (docOf after)) `shouldBe` length (dBuildings (docOf before))
        forM_ (zip (dBuildings (docOf before)) (dBuildings (docOf after))) $ \(x, y) →
            when (x ≢ y) (expectationFailure (show x ⧺ "\n  /=\n" ⧺ show y))
        dLocations (docOf after) `shouldBe` dLocations (docOf before)
        dUnits (docOf after) `shouldBe` dUnits (docOf before)
        dGround (docOf after) `shouldBe` dGround (docOf before)
        -- and the restored document is itself clean YAML that decodes
        (_, _) ← roundTrip fx after
        pure ()

-- * The retained compatibility fixture

-- | Writes @z17-scenario-placement@ when asked to: a finite 20x10 arena
--   page carrying placed content as the current build saves it — a
--   damaged-variant floor and a wall on one tile, a charged battery and a
--   crate holding a bar (each with its scenario reference), and a real
--   @ruin_small@ location with its significant item bound and its own
--   reference — so @buildings@ v4 and @world-pages@ v16 are retained with
--   their identity mappings populated. The page is built by the real
--   adapters and saved through the real envelope encoder; the donor
--   save (@z16-scenario-world@) supplies the metadata and Lua components
--   the page does not own. The example does nothing unless
--   @SYNARCHY_WRITE_PLACEMENT_FIXTURE@ names the output path
--   (@docs/pr-proofs/issue-2712-make-z17.py.txt@ is the recipe).
fixtureSpec ∷ SpecWith Fx
fixtureSpec = describe "retained save-compat fixture" $
    it "builds the page the z17 fixture is saved from, and writes it when asked" $ \fx → do
        resetRegistries fx
        (pid, ws) ← newArenaAs (fxEnv fx) (WorldPageId "finite_arena") 20 10
        ab ← either (fail ∘ show) pure (mkArenaBounds 20 10)
        sc ← clean fx $ doc
            [ "structures:"
            , "  - {id: floor, pack: dungeon_1, piece: floor, x: 3, y: 0, variant: damaged}"
            , "  - {id: wall, pack: dungeon_1, piece: wall_ne, x: 3, y: 0}"
            , "buildings:"
            , "  - {id: battery, definition: high_voltage_battery, x: 5, y: 2, power_charge: 1500}"
            , "  - {id: crate, definition: cargo_hold_S, x: 6, y: 2, storage: [{id: bar, definition: steel_bar}]}"
            , "locations:"
            , "  - {id: ruin, definition: ruin_small, x: -3, y: 0, significant_items: {1: salvage}}"
            , "ground_items:"
            , "  - {id: salvage, definition: processing_unit, x: -2.5, y: 0.5}" ]
        mz ← materialize fx pid ws ab sc
        mzDiagnostics mz `shouldBe` []
        c ← capturePage fx pid ws ab
        (length (cpStructures c), length (cpBuildings c), length (cpLocations c)) `shouldBe` (2, 2, 1)
        out ← lookupEnv "SYNARCHY_WRITE_PLACEMENT_FIXTURE"
        forM_ out $ \path → do
            donor ← BS.readFile "test-headless/data/save-compat/z16-scenario-world.bin"
            savedEnvelopeFrom fx pid ws donor ≫= BS.writeFile path

-- * The instance owners the adapters rely on

-- | The pure functions @Location.Instance@ gained for scenario placement,
--   against the registry's real definitions.
ownerSpec ∷ SpecWith Fx
ownerSpec = describe "Location.Instance scenario owners" $ do

    let def' fx name = locationRegistry (fxEnv fx) ≫= \r →
            maybe (fail ("no definition " ⧺ T.unpack name)) pure (lookupLocation name r)
        place def lis = either (fail ∘ show) pure (allocateLocationInstanceAtIn Nothing 0 Nothing (0, 0) def lis)

    it "scenarioEncounter is exactly the joined units, an empty roster is cleared, and a no-encounter definition has none" $ \fx → do
        ruinDef ← def' fx "ruin_small"
        quiet ← def' fx "quiet_fx"
        locationDefHasEncounter ruinDef `shouldBe` True
        locationDefHasEncounter quiet `shouldBe` False
        scenarioEncounter quiet [(UnitId 1, (0, 0))] `shouldBe` Nothing
        let two = scenarioEncounter ruinDef [(UnitId 1, (0.5, 0.5)), (UnitId 2, (1.5, 0.5))]
        fmap (\e → (leRolledCount e, map leoUnitId (leOccupants e), leRosterComplete e, leCleared e, leActivated e)) two
            `shouldBe` Just (2, [UnitId 1, UnitId 2], True, False, False)
        fmap (\e → (leRolledCount e, leCleared e)) (scenarioEncounter ruinDef []) `shouldBe` Just (0, True)

    it "newLocationInstanceAt stores the explicit anchor and refuses an unrepresentable one" $ \fx → do
        ruinDef ← def' fx "ruin_small"
        case newLocationInstanceAt 0 Nothing (LocationInstanceId 1) (20, -5) ruinDef of
            Right inst → do
                (liAnchor inst, liChunk inst) `shouldBe` ((20, -5), ChunkCoord 1 (-1))
                liLifecycle inst `shouldBe` LifecycleUnknown
                liContentsSpawned inst `shouldBe` False
            Left e → expectationFailure (show e)
        newLocationInstanceAt 0 Nothing (LocationInstanceId 1) (maxBound, 0) ruinDef
            `shouldSatisfy` either (const True) (const False)

    it "setLocationScenarioRef is injective and refuses an unknown instance" $ \fx → do
        ruinDef ← def' fx "ruin_small"
        (a, lis1) ← place ruinDef emptyLocationInstances
        (b, lis2) ← place ruinDef lis1
        Just lis3 ← pure (setLocationScenarioRef a "first" lis2)
        setLocationScenarioRef b "first" lis3 `shouldBe` Nothing
        setLocationScenarioRef (LocationInstanceId 99) "ghost" lis3 `shouldBe` Nothing
        -- restating an instance's own reference is not a collision
        fmap lisScenarioRefs (setLocationScenarioRef a "first" lis3) `shouldBe` Just (Map.singleton a "first")

    it "setLocationContainerProfile edits only a pending slot" $ \fx → do
        crypt ← def' fx "crypt_fx"
        (iid, lis1) ← place crypt emptyLocationInstances
        let slotOf l n = [ c | i ← instancesToList l, c ← liContainers i, lcsSlot c ≡ n ]
        fmap (map lcsProfile ∘ (\l → slotOf l 1)) (setLocationContainerProfile iid 1 "ruin_industrial_salvage" lis1)
            `shouldBe` Just [Just "ruin_industrial_salvage"]
        setLocationContainerProfile iid 9 "ruin_industrial_salvage" lis1 `shouldBe` Nothing
        setLocationContainerProfile (LocationInstanceId 99) 1 "ruin_industrial_salvage" lis1 `shouldBe` Nothing
        -- a bound, realized slot has had its profile discarded for good
        Just bound ← pure (registerLocationContainerSpawn iid 1 "field_toolbox" 4242 lis1)
        Just realized ← pure (realizeLocationContainerSlot iid 1 4242 bound)
        setLocationContainerProfile iid 1 "ruin_industrial_salvage" realized `shouldBe` Nothing
