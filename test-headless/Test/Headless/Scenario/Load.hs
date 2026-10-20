{-# LANGUAGE Strict #-}
-- | "Scenario.Load" (#2713, SCN-13 of epic #2698): the paused
--   scenario-load service, driven through its production entry point, the
--   production Lua message pump, the real save barrier, the real world
--   thread and the real construction adapters, against the shipped
--   definitions. Only the owner threads a headless harness never starts
--   are stand-ins ("Test.Headless.Scenario.Load.Harness"), and they follow
--   the real owners' protocol and count what they do.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Scenario.Load"'@.
module Test.Headless.Scenario.Load (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef)
import Data.List (sort)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Directory (createDirectoryIfMissing, getModificationTime)
import Building.Types
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.Capability.RenderView (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Load.Status
import Gameplay.Tags.Index (categoryTagsOf)
import Gameplay.Tags.Types (GameplayTag, mkGameplayTag)
import Item.Types (ItemInstance(..))
import Location.Instance
import Power.Types (nodeForBuilding, PowerNode(..))
import Scenario.Items.Refs (itemRefsToAscList)
import Scenario.Load.Types
import Scenario.Schema (ScenarioFormat(..), scenarioFormat)
import Scenario.Types
    ( DiagnosticEffect(..), ScenarioDiagnostic(..), ScenarioFailure(..) )
import Test.Headless.Scenario.Load.Harness
import qualified Test.Headless.Scenario.Load.Exclusion as Exclusion
import qualified Test.Headless.Scenario.Load.Failure as Failure
import qualified Test.Headless.Scenario.Load.Session as Session
import qualified Test.Headless.Scenario.Load.Persist as Persist
import Test.Headless.Scenario.Load.Persist (integratedDoc)
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import Unit.Types
import World.Arena.Bounds (mkArenaBounds)
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Generate.Coordinates (globalToChunk)
import World.Generate.Types (WorldGenParams(..))
import World.State.Types
import World.Tile.Types (lookupChunk)

spec ∷ Spec
spec = aroundAll withH $ do
    readySpec
    preflightSpec
    malformedSpec
    Exclusion.spec
    Failure.spec
    Session.spec
    Persist.spec

tag ∷ Text → GameplayTag
tag t = fromMaybe (error "empty tag") (mkGameplayTag t)

source ∷ String → String → IO ScenarioSource
source name text = SourceFile <$> writeScenario name text

-- * Reading the published session

liveEnv ∷ H → EngineEnv
liveEnv = fxEnv ∘ hFx

unitByRef ∷ H → Text → IO (UnitId, UnitInstance)
unitByRef h ref = do
    um ← readIORef (unitManagerRef (liveEnv h))
    case [ (u, i) | (u, r) ← Map.toList (umScenarioRefs um), r ≡ ref, Just i ← [HM.lookup u (umInstances um)] ] of
        [x] → pure x
        other → fail ("expected one unit '" ⧺ T.unpack ref ⧺ "', found " ⧺ show (length other))

stat ∷ UnitInstance → Text → Float
stat u k = HM.lookupDefault (-1) k (uiStats u)

arenaOrFail ∷ H → IO WorldState
arenaOrFail h = activeArena h ≫= maybe (fail "the arena page is not published") pure

columnFluid ∷ WorldState → (Int, Int) → IO (Maybe FluidCell)
columnFluid ws (x, y) = do
    td ← readIORef (wsTilesRef ws)
    let (c, (lx, ly)) = globalToChunk x y
    pure (lookupChunk c td ⌦ \lc → lcFluidMap lc V.! columnIndex lx ly)

columnTop ∷ WorldState → (Int, Int) → IO (Maybe Int)
columnTop ws (x, y) = do
    td ← readIORef (wsTilesRef ws)
    let (c, (lx, ly)) = globalToChunk x y
    pure (fmap (\lc → lcTerrainSurfaceMap lc VU.! columnIndex lx ly) (lookupChunk c td))

terminal ∷ ScenarioLoadStatus → Expectation
terminal s = slsOutcome s `shouldSatisfy` isJust

-- * Ready

readySpec ∷ SpecWith H
readySpec = describe "ready state" $ do

    it "loads an integrated scenario of every family and reports ready only with the exact content installed, paused" $ \h → do
        let env = liveEnv h
        src ← source "integrated.yaml" integratedDoc
        s ← runLoad h src
        slsOutcome s `shouldBe` Just ScenarioReady
        (slsStage s, slsPublished s, slsRecovery s) `shouldBe` (StageReady, True, RecoveryNone)
        slsWarnings s `shouldBe` []
        -- the generic transaction ended as an unqualified success
        Just ls ← readLoadStatus (slLoadStatusRef (toSaveLoadCapability env))
        (lsPhase ls, lsOutcome ls) `shouldBe` (LoadPublished, Just LoadSucceeded)
        loadInProgress (slLoadStatusRef (toSaveLoadCapability env)) `shouldReturn` False
        -- paused, at the session epoch, and the Lua pause mirror agrees
        readIORef (enginePausedRef env) `shouldReturn` True
        readIORef (gameTimeRef env) `shouldReturn` 0
        lua h "return tostring(engine.isPaused())" `shouldReturn` "true"
        -- the camera starts where the document says
        cam ← readIORef (rvCameraRef (toRenderViewCapability env))
        camPosition cam `shouldBe` (3, 4)
        -- the page: finite bounds, terrain, exact fluid
        ws ← arenaOrFail h
        Just params ← readIORef (wsGenParamsRef ws)
        wgpArenaBounds params `shouldBe` either (const Nothing) Just (mkArenaBounds 40 40)
        columnTop ws (0, 0) `shouldReturn` Just 6
        columnTop ws (8, 8) `shouldReturn` Just (-2)
        wet ← columnFluid ws (8, 8)
        fmap (\c → (fcType c, fcExactSurface c)) wet `shouldBe` Just (Lake, 35)
        -- the units: exact stated values, nested inventory, tags
        (medicId, medic) ← unitByRef h "medic"
        -- an omitted z is the column surface the terrain install made
        uiGridZ medic `shouldBe` 6
        stat medic "hydration" `shouldBe` 4.0
        stat medic "calories" `shouldBe` 600
        uiBlood medic `shouldBe` 3.2
        length (uiWounds medic) `shouldBe` 1
        um ← readIORef (unitManagerRef env)
        categoryTagsOf medicId (umTags um) `shouldBe` Set.fromList [tag "squad"]
        [kit] ← pure (uiInventory medic)
        (iiDefName kit, map iiDefName (iiContents kit)) `shouldBe` ("field_toolbox", ["steel_bar"])
        HM.size (umInstances um) `shouldBe` 2
        -- the buildings: charge on the power node, stored item, tags
        bm ← readIORef (buildingManagerRef env)
        nodes ← readIORef (wsPowerNodesRef ws)
        let byRef r = [ (b, i) | (b, ref) ← Map.toList (bmScenarioRefs bm), ref ≡ r
                               , Just i ← [HM.lookup b (bmInstances bm)] ]
        [(batId, _)] ← pure (byRef "bat")
        fmap pnStoredWh (nodeForBuilding batId nodes) `shouldBe` Just 2500
        categoryTagsOf batId (bmTags bm) `shouldBe` Set.fromList [tag "power"]
        [(_, crate)] ← pure (byRef "crate")
        map iiDefName (biStorage crate) `shouldBe` ["steel_bar"]
        -- the real location, with the unit that joined it
        Just params2 ← readIORef (wsGenParamsRef ws)
        [inst] ← pure (instancesToList (wgpLocationInstances params2))
        fmap (length ∘ leOccupants) (liEncounter inst) `shouldBe` Just 1
        Map.lookup (liId inst) (lisScenarioRefs (wgpLocationInstances params2)) `shouldBe` Just "ruin"
        categoryTagsOf (liId inst) (lisTags (wgpLocationInstances params2)) `shouldBe` Set.fromList [tag "site"]
        -- tag queries across every category, answered by the engine's own
        -- evaluator while the session is paused
        forM_ [ ("pad", "49"), ("grove", "1"), ("power", "1"), ("site", "1"), ("squad", "1"), ("loot", "1") ] $ \(t, n) →
            lua h ("return tostring(gameplayTags.count({ tag = '" <> t <> "' }))") `shouldReturn` n
        readIORef (enginePausedRef env) `shouldReturn` True
        -- the ground item that location binds, with its tag
        wm ← readIORef (worldManagerRef env)
        map snd (itemRefs wm) `shouldContain` ["gem"]

-- | The (instance id, reference) pairs of every referenced physical item.
itemRefs ∷ WorldManager → [(Word64, Text)]
itemRefs = itemRefsToAscList ∘ wmItemScenarioRefs

-- * Preflight

preflightSpec ∷ SpecWith H
preflightSpec = describe "preflight" $ do

    let failsCleanly h label cause mkSource = do
            before ← sessionFingerprint h
            src ← mkSource
            s ← runLoad h src
            case slsOutcome s of
                Just (ScenarioUnsuccessful ctx) → do
                    fcStage ctx `shouldBe` StagePreflight
                    cause (fcCause ctx) `shouldBe` True
                other → expectationFailure (label ⧺ ": " ⧺ show other)
            -- nothing was constructed, the previous session stands, and the
            -- request says so
            slsPublished s `shouldBe` False
            slsRecovery s `shouldBe` RecoveryPreserved
            slsStage s `shouldBe` StageFailed
            sessionFingerprint h `shouldReturn` before
            Just ls ← readLoadStatus (slLoadStatusRef (toSaveLoadCapability (liveEnv h)))
            lsPhase ls `shouldBe` LoadFailed
            -- the exclusion is released: another request is admitted
            loadInProgress (slLoadStatusRef (toSaveLoadCapability (liveEnv h))) `shouldReturn` False

    it "refuses a missing file" $ \h →
        failsCleanly h "missing" (\c → case c of CauseSource (ScenarioUnreadable _) → True; _ → False)
            (SourceFile <$> scratchFile "no-such-scenario.yaml")

    it "refuses an unreadable source (a directory)" $ \h → do
        dir ← scratchFile "a-directory.yaml"
        createDirectoryIfMissing True dir
        failsCleanly h "directory" (\c → case c of CauseSource (ScenarioUnreadable _) → True; _ → False)
            (pure (SourceFile dir))

    it "refuses malformed YAML syntax" $ \h →
        failsCleanly h "syntax" (\c → case c of CauseSource (ScenarioSyntaxError _) → True; _ → False)
            (source "syntax.yaml" "version: 3\nunits: [unclosed\n")

    it "refuses a document that is not a mapping" $ \h →
        failsCleanly h "not a mapping" (\c → case c of CauseSource ScenarioNotAMapping → True; _ → False)
            (source "list.yaml" "- 1\n- 2\n")

    it "refuses a missing and a malformed version" $ \h → do
        failsCleanly h "no version" (\c → case c of CauseSource ScenarioVersionMissing → True; _ → False)
            (source "noversion.yaml" "units: []\n")
        failsCleanly h "bad version" (\c → case c of CauseSource (ScenarioVersionMalformed _) → True; _ → False)
            (source "badversion.yaml" "version: three\n")

    it "refuses an unsupported newer format and one below the first" $ \h → do
        failsCleanly h "newer" (\c → case c of CauseSource (ScenarioVersionUnsupported 99) → True; _ → False)
            (source "newer.yaml" "version: 99\n")
        failsCleanly h "zero" (\c → case c of CauseSource (ScenarioVersionUnsupported 0) → True; _ → False)
            (source "zero.yaml" "version: 0\n")

    it "refuses a document whose migration fails, rewriting nothing" $ \h → do
        path ← writeScenario "migrating.yaml" "version: 1\n"
        bytesBefore ← BS.readFile path
        mtimeBefore ← getModificationTime path
        let failing = scenarioFormat
                { sfMigrations = [ (1, \_ → Left "step 1 cannot be applied"), (2, Right) ] }
        armSeamsWith h (\_ → pure ()) (Just failing)
        failsCleanly h "migration" (\c → case c of CauseSource (ScenarioMigrationFailed 1 _) → True; _ → False)
            (pure (SourceFile path))
        disarm h
        -- the source is untouched, by content and by time
        BS.readFile path `shouldReturn` bytesBefore
        getModificationTime path `shouldReturn` mtimeBefore

    it "never rewrites a source it loads successfully" $ \h → do
        path ← writeScenario "readonly-source.yaml" (doc ["units: []"])
        bytesBefore ← BS.readFile path
        mtimeBefore ← getModificationTime path
        s ← runLoad h (SourceFile path)
        slsOutcome s `shouldBe` Just ScenarioReady
        BS.readFile path `shouldReturn` bytesBefore
        getModificationTime path `shouldReturn` mtimeBefore

-- * Supported malformed content

malformedSpec ∷ SpecWith H
malformedSpec = describe "supported malformed content" $ do

    it "loads the usable subset: exact warnings, rejected owners' descendants absent, cascades, no false whole-file failure" $ \h → do
        let env = liveEnv h
        src ← source "malformed.yaml" $ doc
            [ "map: {width: 20, height: 20}"
            , "locations:"
            , "  - {id: bad-site, definition: no_such_location, x: 5, y: 5}"
            , "  - {id: good-site, definition: ruin_small, x: -5, y: -5, significant_items: {}, incidental_content: false}"
            , "units:"
            , "  - {id: ghost, definition: no_such_unit, x: 1, y: 1, inventory: [{id: lost, definition: steel_bar}]}"
            , "  - {id: raider, definition: nomad_primitive, x: 5.5, y: 5.5, encounter: bad-site}"
            , "  - {id: kept, definition: nomad_primitive, x: -4.5, y: -4.5, encounter: good-site}"
            , "  - {id: loner, definition: acolyte, x: 2, y: 2}"
            , "buildings:"
            , "  - {id: edge, definition: solar_panel, x: 40, y: 40}"
            , "  - {id: ok, definition: solar_panel, x: 3, y: 3}" ]
        s <- runLoad h src
        -- usable, not failed: ready WITH its warnings
        slsOutcome s `shouldBe` Just ScenarioReady
        let found = [ (sdPath d, sdEffect d) | d ← slsWarnings s ]
        found `shouldMatchList`
            [ ("locations[0].definition", EntryRejected)
            , ("units[0].definition", EntryRejected)
            , ("units[0].inventory[0]", CascadeRejected)
            , ("units[1].encounter", CascadeRejected)
            , ("buildings[0]", EntryRejected) ]
        -- retained: the valid unit, the unit that joined the valid site, the valid building
        um ← readIORef (unitManagerRef env)
        sort (Map.elems (umScenarioRefs um)) `shouldBe` ["kept", "loner"]
        bm ← readIORef (buildingManagerRef env)
        Map.elems (bmScenarioRefs bm) `shouldBe` ["ok"]
        -- the rejected owner's inventory never existed: the only item in the
        -- session belongs to nobody that survived
        wm ← readIORef (worldManagerRef env)
        map snd (itemRefs wm) `shouldNotContain` ["lost"]
        -- the automated-caller contract: every warning is in the terminal result
        terminal s
