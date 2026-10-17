{-# LANGUAGE OverloadedStrings #-}
-- | "Gameplay.Tags.Persistence" (#2701, SCN-03 of epic #2698): general
--   gameplay tags on units, buildings and locations, held by their live
--   owners, kept through every survival, removed by every removal, and
--   carried by every save and load.
--
--   Every path is the production one. Entities are admitted through
--   @unit.spawn@\/@building.spawn@ and drained by the real unit and
--   building command drains; locations are allocated into a page's own
--   table and moved through the world thread's own lifecycle handler;
--   destruction, clear-all and page lifecycle transitions are the real
--   handlers ("Test.Headless.World.PageIncarnation" supplies the scene
--   and the three transition paths); and the session examples go
--   through the real component codecs, 'stageSession' and
--   'publishStagedSession'. Tag edits and reads use
--   "Gameplay.Tags.Live", the one internal API, over the live refs.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Gameplay.Tags.Persistence"'@.
module Test.Headless.Gameplay.TagsPersistence (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)

import Building.Command.Types (BuildingCommand(..))
import Building.Types (BuildingId(..), BuildingManager(..))
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.Log
    ( LoggerState, LogEntry(..), LogConfig(..), LogBackend(..)
    , initLogger, defaultLogConfig )
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Graphics.Camera (CameraFacing(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Gameplay.Tags.Index
import Gameplay.Tags.Live
import Gameplay.Tags.Query
import Gameplay.Tags.Registry (registryInvariantViolations, registryCounts, RegistryCounts(..))
import Gameplay.Tags.Types
import Item.Knowledge (emptyPortableKnowledge)
import Location.Bounds (RelBounds(..))
import Location.Instance
import Location.Types
import Language.Semantic.Types (ConceptId(..))
import Structure.Palette (emptyTexPalette)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.World.PageIncarnation
    ( incPage, keepPage, unitTile, oldTile, lateTile, buildingDefName
    , resetScene, drainEntities, spawnUnitOn, spawnBuildingOn
    , destroyPage, Path(..), paths, newBareLuaBackend )
import Unit.Command.Types (UnitCommand(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams, withVolcanoCtx)
import World.Load.Publish (publishStagedSession)
import World.Load.Stage (stageSession, renderStageError)
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Entities
import World.Save.Component.Page
import World.Save.Component.Types
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Integrity
    (IntegrityError(..), sessionIntegrityErrors, sessionIntegrityWarnings)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Save.Types
    ( BuildingSnapshot(..), UnitSnapshot(..), UnitInstanceSnapshot(..)
    , toBuildingSnapshot, toUnitSnapshot )
import World.State.Types (WorldManager(..), WorldState(..))
import World.Thread.Command.Basic (handleWorldDestroyAllCommand)
import World.Thread.Command.Location (handleWorldSetLocationLifecycleCommand)
import World.Render.Zoom.Types (ZoomMapMode(..))

-- * Vocabulary

tg ∷ Text → GameplayTag
tg t = fromMaybe (error "an empty fixture tag") (mkGameplayTag t)

tags ∷ [Text] → Set.Set GameplayTag
tags = Set.fromList . map tg

-- * Targets on the live scene

-- | A unit admitted through @unit.spawn@ on @page@ and drained in.
newUnit ∷ EngineEnv → LuaBackendState → WorldPageId → IO TagTarget
newUnit env ls page = do
    uid ← spawnUnitOn ls page unitTile
    drainEntities env
    pure (TargetUnit (UnitId uid))

-- | A building admitted through @building.spawn@ at @tile@ and drained.
newBuilding ∷ EngineEnv → LuaBackendState → WorldPageId → (Int, Int) → IO TagTarget
newBuilding env ls page tile = do
    bid ← spawnBuildingOn ls buildingDefName tile page Nothing
    drainEntities env
    pure (TargetBuilding (BuildingId bid))

-- | A location allocated into @page@'s own table at @coord@.
newLocation ∷ EngineEnv → WorldPageId → ChunkCoord → IO TagTarget
newLocation env page coord = do
    ws ← pageState env page
    lid ← atomicModifyIORef' (wsGenParamsRef ws) $ \case
        Nothing → error "fixture page has no params"
        Just params →
            case allocateLocationInstance Nothing coord ruinDef
                     (wgpLocationInstances params) of
                Left e → error ("allocateLocationInstance: " <> show e)
                Right (lid, lis) →
                    (Just params { wgpLocationInstances = lis }, lid)
    pure (TargetLocation (LocationTarget page lid))

ruinDef ∷ LocationDef
ruinDef = LocationDef
    { ldId = "tag_ruin", ldLabel = "Tagged Ruin", ldType = "ruin"
    , ldBuilder = "room_small", ldAnchor = [], ldMaxCount = 0
    , ldMinSpacing = 0, ldContents = [], ldBounds = RelBounds (-1) (-1) 1 1
    , ldMapIcon = Nothing
    , ldNaming = LocationNaming
        { lnHeads = [ConceptId "KEEP"], lnModifiers = [ConceptId "ASH"] } }

pageState ∷ EngineEnv → WorldPageId → IO WorldState
pageState env page = do
    wm ← readIORef (worldManagerRef env)
    maybe (fail ("no live page " <> show page)) pure (lookup page (wmWorlds wm))

-- * The one internal API, over the live refs

edit ∷ EngineEnv → TagTarget → TagEdit → IO TagEditResult
edit env t e = case t of
    TargetUnit u     → mutateUnitTags (unitManagerRef env) u e
    TargetBuilding b → mutateBuildingTags (buildingManagerRef env) b e
    TargetLocation l → mutateLocationTags (worldManagerRef env) l e
    _                → fail "not an owned category"

view ∷ EngineEnv → IO LiveTagView
view env = captureLiveTagView (unitManagerRef env) (buildingManagerRef env)
                              (worldManagerRef env)

-- | A target's live tags; 'Nothing' when the target does not exist.
tagsNow ∷ EngineEnv → TagTarget → IO (Maybe (Set.Set GameplayTag))
tagsNow env t = do
    v ← view env
    pure $ case t of
        TargetUnit u     → unitTagsIn (ltvUnits v) u
        TargetBuilding b → buildingTagsIn (ltvBuildings v) b
        TargetLocation (LocationTarget p l) →
            lookup p (ltvLocations v) ⌦ \lis → locationTagsIn lis l
        _ → Nothing

-- | Every live assignment, in result order.
assignedNow ∷ EngineEnv → IO [(TagTarget, Set.Set GameplayTag)]
assignedNow env = liveTagAssignments <$> view env

-- | A query over one category's whole live universe.
queryNow ∷ EngineEnv → TagCategory → TagQuery → IO [TagTarget]
queryNow env cat q = do
    v ← view env
    pure (targetSetsToList (runLiveTagQuery v (selectCategories [cat]) q))

-- | Both indexes of every owner agree, and one category's counts.
coherent ∷ EngineEnv → IO ()
coherent env = do
    v ← view env
    registryInvariantViolations (liveTagRegistry v) `shouldBe` []

countsOf ∷ EngineEnv → TagCategory → IO RegistryCounts
countsOf env cat = do
    v ← view env
    pure (fromMaybe (RegistryCounts 0 0 0)
            (lookup cat (registryCounts (liveTagRegistry v))))

-- * Categories, for the examples every category shares

data Category = Category
    { catName ∷ String
    , catKind ∷ TagCategory
    , catMake ∷ EngineEnv → LuaBackendState → Int → IO TagTarget
      -- ^ The nth fresh target on 'incPage'.
    }

categories ∷ [Category]
categories =
    [ Category "units" UnitCategory $ \env ls _ → newUnit env ls incPage
    , Category "buildings" BuildingCategory $ \env ls n →
        newBuilding env ls incPage ([oldTile, lateTile, (6, 6)] !! n)
    , Category "locations" LocationCategory $ \env _ n →
        newLocation env incPage (ChunkCoord n 0) ]

-- * Spec

spec ∷ Spec
spec = describe "Gameplay.Tags.Persistence" $ aroundAll setup $ do
    liveSpec
    survivalSpec
    removalSpec
    replacementSpec
    sessionSpec
    wireSpec
  where
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        act (env, ls)

-- | Requirement 2: edits and reads through the owners.
liveSpec ∷ SpecWith (EngineEnv, LuaBackendState)
liveSpec = describe "live assignments" $ forM_ categories $ \cat → do
    let name = catName cat

    it (name <> ": the first tag creates the target's entry and removing \
       \the last deletes it, both indexes together") $ \(env, ls) → do
        resetScene env
        t ← catMake cat env ls 0
        tagsNow env t `shouldReturn` Just Set.empty
        edit env t (AddTagsEdit (tags ["guard"])) `shouldReturn` TagEditApplied
        tagsNow env t `shouldReturn` Just (tags ["guard"])
        countsOf env (catKind cat) `shouldReturn` RegistryCounts 1 1 1
        coherent env
        edit env t (RemoveTagsEdit (tags ["guard"])) `shouldReturn` TagEditApplied
        tagsNow env t `shouldReturn` Just Set.empty
        countsOf env (catKind cat) `shouldReturn` RegistryCounts 0 0 0
        assignedNow env `shouldReturn` []
        coherent env

    it (name <> ": replace makes the complete set, and clearing or \
       \replacing with nothing removes the entry") $ \(env, ls) → do
        resetScene env
        t ← catMake cat env ls 0
        _ ← edit env t (AddTagsEdit (tags ["a", "b"]))
        _ ← edit env t (ReplaceTagsEdit (tags ["b", "c"]))
        tagsNow env t `shouldReturn` Just (tags ["b", "c"])
        _ ← edit env t ClearTagsEdit
        assignedNow env `shouldReturn` []
        _ ← edit env t (AddTagsEdit (tags ["d"]))
        _ ← edit env t (ReplaceTagsEdit Set.empty)
        assignedNow env `shouldReturn` []
        coherent env

    it (name <> ": a shared tag stays on the sibling when one target loses \
       \it") $ \(env, ls) → do
        resetScene env
        a ← catMake cat env ls 0
        b ← catMake cat env ls 1
        _ ← edit env a (AddTagsEdit (tags ["shared", "only_a"]))
        _ ← edit env b (AddTagsEdit (tags ["shared"]))
        sort ⊚ queryNow env (catKind cat) (tagged (tg "shared"))
            `shouldReturn` sort [a, b]
        _ ← edit env a (RemoveTagsEdit (tags ["shared"]))
        queryNow env (catKind cat) (tagged (tg "shared")) `shouldReturn` [b]
        tagsNow env a `shouldReturn` Just (tags ["only_a"])
        coherent env

    it (name <> ": an untagged target is in the universe, so a negative \
       \query finds it") $ \(env, ls) → do
        resetScene env
        a ← catMake cat env ls 0
        b ← catMake cat env ls 1
        _ ← edit env a (AddTagsEdit (tags ["defender"]))
        found ← queryNow env (catKind cat) (QComplement (tagged (tg "defender")))
        found `shouldSatisfy` elem b
        found `shouldNotSatisfy` elem a
        queryNow env (catKind cat) (tagged (tg "nobody")) `shouldReturn` []

    it (name <> ": an edit naming no existing target creates nothing") $
        \(env, _) → do
            resetScene env
            let ghost = case catKind cat of
                    UnitCategory     → TargetUnit (UnitId 9999)
                    BuildingCategory → TargetBuilding (BuildingId 9999)
                    _ → TargetLocation (LocationTarget incPage (LocationInstanceId 99))
            edit env ghost (AddTagsEdit (tags ["ghost"]))
                `shouldReturn` TagEditNoTarget
            assignedNow env `shouldReturn` []

-- | Requirement 3: a surviving object keeps its tags.
survivalSpec ∷ SpecWith (EngineEnv, LuaBackendState)
survivalSpec = describe "surviving objects keep their tags" $ do

    it "a unit keeps its tags through a teleport and through death, which \
       \leaves its instance in place" $ \(env, ls) → do
        resetScene env
        t@(TargetUnit uid) ← newUnit env ls incPage
        _ ← edit env t (AddTagsEdit (tags ["scout"]))
        before ← instanceOf env uid
        Q.writeQueue (unitQueue env) (UnitTeleport uid 5 5 (Just 0))
        drainEntities env
        moved ← instanceOf env uid
        (uiGridX ⊚ moved) `shouldNotBe` (uiGridX ⊚ before)
        tagsNow env t `shouldReturn` Just (tags ["scout"])
        Q.writeQueue (unitQueue env) (UnitKill uid)
        drainEntities env
        (fmap uiPose ⊚ instanceOf env uid) `shouldReturn` Just "dead"
        tagsNow env t `shouldReturn` Just (tags ["scout"])

    it "tagging changes nothing about the unit itself — faction, pose and \
       \stats are untouched" $ \(env, ls) → do
        resetScene env
        t@(TargetUnit uid) ← newUnit env ls incPage
        before ← instanceOf env uid
        _ ← edit env t (AddTagsEdit (tags ["hostile_looking"]))
        instanceOf env uid `shouldReturn` before

    it "a building keeps its tags through an in-place change" $ \(env, ls) → do
        resetScene env
        t@(TargetBuilding (BuildingId bid)) ← newBuilding env ls incPage oldTile
        _ ← edit env t (AddTagsEdit (tags ["depot"]))
        r ← executeDebugLua (lbsLuaState ls)
                ("building.setSpawnRemaining(" <> tshow bid <> ", 3); \
                 \return building.getSpawnRemaining(" <> tshow bid <> ")")
        drainEntities env
        T.strip r `shouldSatisfy` (`elem` ["3", "3.0"])
        tagsNow env t `shouldReturn` Just (tags ["depot"])

    it "a location keeps its tags through every lifecycle step, cleared \
       \and depleted included" $ \(env, _) → do
        resetScene env
        t@(TargetLocation (LocationTarget page lid)) ←
            newLocation env incPage (ChunkCoord 0 0)
        _ ← edit env t (AddTagsEdit (tags ["landmark"]))
        forM_ [LifecycleHinted, LifecycleDiscovered, LifecycleActive
              , LifecycleCleared, LifecycleDepleted] $ \step → do
            handleWorldSetLocationLifecycleCommand (toWorldSimCapability env)
                page lid step
            ws ← pageState env page
            mParams ← readIORef (wsGenParamsRef ws)
            (liLifecycle ⊚ (mParams ⌦ lookupLocationInstance lid
                                       ∘ wgpLocationInstances))
                `shouldBe` Just step
            tagsNow env t `shouldReturn` Just (tags ["landmark"])

-- | Requirement 4: every removal path clears.
removalSpec ∷ SpecWith (EngineEnv, LuaBackendState)
removalSpec = describe "removal clears assignments" $ do

    it "destroying a unit removes its tags from both indexes and leaves a \
       \sibling's shared tag" $ \(env, ls) → do
        resetScene env
        a@(TargetUnit uidA) ← newUnit env ls incPage
        b ← newUnit env ls incPage
        _ ← edit env a (AddTagsEdit (tags ["shared", "mine"]))
        _ ← edit env b (AddTagsEdit (tags ["shared"]))
        Q.writeQueue (unitQueue env) (UnitDestroy uidA)
        drainEntities env
        tagsNow env a `shouldReturn` Nothing
        assignedNow env `shouldReturn` [(b, tags ["shared"])]
        queryNow env UnitCategory (tagged (tg "mine")) `shouldReturn` []
        coherent env

    it "demolishing a building removes its tags and leaves a sibling's" $
        \(env, ls) → do
            resetScene env
            a@(TargetBuilding bidA) ← newBuilding env ls incPage oldTile
            b ← newBuilding env ls incPage lateTile
            _ ← edit env a (AddTagsEdit (tags ["shared"]))
            _ ← edit env b (AddTagsEdit (tags ["shared"]))
            Q.writeQueue (buildingQueue env) (BuildingDestroy bidA)
            drainEntities env
            assignedNow env `shouldReturn` [(b, tags ["shared"])]
            coherent env

    it "clear-all removes every unit, building and location tag" $
        \(env, ls) → do
            resetScene env
            u ← newUnit env ls incPage
            b ← newBuilding env ls incPage oldTile
            l ← newLocation env keepPage (ChunkCoord 0 0)
            forM_ [u, b, l] $ \t → edit env t (AddTagsEdit (tags ["x"]))
            length ⊚ assignedNow env `shouldReturn` 3
            Q.writeQueue (unitQueue env) UnitClearAll
            Q.writeQueue (buildingQueue env) BuildingClearAll
            drainEntities env
            logger ← readIORef (loggerRef env)
            handleWorldDestroyAllCommand env logger
            assignedNow env `shouldReturn` []

    it "destroying one page removes its entities' and locations' tags \
       \and leaves the other page's" $ \(env, ls) → do
        resetScene env
        uInc ← newUnit env ls incPage
        uKeep ← newUnit env ls keepPage
        bInc ← newBuilding env ls incPage oldTile
        lInc ← newLocation env incPage (ChunkCoord 0 0)
        lKeep ← newLocation env keepPage (ChunkCoord 0 0)
        forM_ [uInc, uKeep, bInc, lInc, lKeep] $ \t →
            edit env t (AddTagsEdit (tags ["here"]))
        destroyPage env incPage
        -- Immediate, before any drain: the transition retired the rows.
        sort . map fst ⊚ assignedNow env `shouldReturn` sort [uKeep, lKeep]
        drainEntities env
        sort . map fst ⊚ assignedNow env `shouldReturn` sort [uKeep, lKeep]
        coherent env

-- | Requirement 5: page replacement and the delayed, cutoff-bounded
--   clears.
replacementSpec ∷ SpecWith (EngineEnv, LuaBackendState)
replacementSpec = describe "page replacement" $ do

    forM_ paths $ \p →
        it ("after " <> pathName p <> ", the old incarnation's tags are \
            \gone and a replacement admitted before the queued clear \
            \drains keeps its own") $ \(env, ls) → do
            resetScene env
            uOld ← newUnit env ls incPage
            bOld ← newBuilding env ls incPage oldTile
            lOld ← newLocation env incPage (ChunkCoord 0 0)
            uKeep ← newUnit env ls keepPage
            forM_ [uOld, bOld, lOld, uKeep] $ \t →
                edit env t (AddTagsEdit (tags ["old"]))
            oldWs ← pageState env incPage
            pathRun p env
            -- The transition retired the old rows and their tags at once.
            map fst ⊚ assignedNow env `shouldReturn` [uKeep]
            -- HOLD the delayed clears the transition queued, so the
            -- replacement below is committed and tagged while they are
            -- still outstanding — the window their cutoff exists for.
            heldUnits ← Q.flushQueue (unitQueue env)
            heldBuildings ← Q.flushQueue (buildingQueue env)
            [ () | UnitClearPage _ _ ← heldUnits ] `shouldBe` [()]
            [ () | BuildingClearPage _ _ ← heldBuildings ] `shouldBe` [()]
            -- Admitted after the transition, so at or above the cutoffs.
            uNew ← TargetUnit . UnitId ⊚ spawnUnitOn ls incPage lateTile
            bNew ← TargetBuilding . BuildingId ⊚
                       spawnBuildingOn ls buildingDefName lateTile incPage Nothing
            -- Commits the two spawns only: the held clears are not queued.
            drainEntities env
            -- The replacement's location table restarts its ids, so this
            -- is location 1 AGAIN — and inherits nothing.
            lNew ← newLocation env incPage (ChunkCoord 0 0)
            lNew `shouldBe` lOld
            tagsNow env lNew `shouldReturn` Just Set.empty
            forM_ [uNew, bNew, lNew] $ \t →
                edit env t (AddTagsEdit (tags ["new"])) `shouldReturn` TagEditApplied
            -- Now the delayed clears run, through the production
            -- handlers, over the tagged replacement.
            mapM_ (Q.writeQueue (unitQueue env)) heldUnits
            mapM_ (Q.writeQueue (buildingQueue env)) heldBuildings
            drainEntities env
            tagsNow env uNew `shouldReturn` Just (tags ["new"])
            tagsNow env bNew `shouldReturn` Just (tags ["new"])
            -- ...in both directions: the reverse index still names them.
            sort ⊚ (concat ⊚ mapM (\c → queryNow env c (tagged (tg "new")))
                                   [UnitCategory, BuildingCategory, LocationCategory])
                `shouldReturn` sort [uNew, bNew, lNew]
            -- Old work against the departed incarnation's state lands
            -- there and nowhere else.
            atomicModifyIORef' (wsGenParamsRef oldWs) $ \mp →
                ( fmap (\params → params { wgpLocationInstances =
                          let lis = wgpLocationInstances params
                          in lis { lisTags = setCategoryTags
                                       (LocationInstanceId 1) (tags ["stale"])
                                       (lisTags lis) } }) mp
                , () )
            assigned ← assignedNow env
            sort assigned `shouldBe` sort
                [ (uKeep, tags ["old"]), (uNew, tags ["new"])
                , (bNew, tags ["new"]), (lNew, tags ["new"]) ]
            coherent env

-- * Whole-session save and load

-- | The live scene as a captured session, as the save barrier captures
--   it: each page's entities and their tags through the production
--   page filter, with the page's own location table. Pages are put in
--   the arena shape so staging rebuilds flat chunks instead of
--   generating terrain.
capturedSession ∷ EngineEnv → IO SessionSnapshot
capturedSession env = do
    um ← readIORef (unitManagerRef env)
    bm ← readIORef (buildingManagerRef env)
    pages ← forM [incPage, keepPage] $ \pid → do
        ws ← pageState env pid
        mParams ← readIORef (wsGenParamsRef ws)
        let lis = maybe emptyLocationInstances wgpLocationInstances mParams
        pure (blankPageSnapshot pid
                (withVolcanoCtx arenaParams { wgpLocationInstances = lis }))
            { pgsUnits = toUnitSnapshot pid um
            , pgsBuildings = toBuildingSnapshot pid bm
            , pgsGeneratedId = Just (fixtureGeneratedWorldIdForPage pid)
            , pgsDateYear = 1, pgsDateMonth = 1, pgsDateDay = 1 }
    either (\e → fail ("capture refused: " <> show e)) pure $
        captureSessionSnapshot SessionGlobals
            { sgGameTime = 0, sgTexPalette = emptyTexPalette
            -- Above the fixture supplier's own items (ids 1..4).
            , sgNextItemId = 100, sgNextBuildingId = bmNextId bm
            , sgNextUnitId = umNextId um, sgActivePage = incPage
            , sgVisiblePages = [incPage]
            , sgLiveCamera = LiveCameraSnapshot (Just incPage) 0 0 1 FaceSouth
            , sgPortableKnowledge = emptyPortableKnowledge }
            pages

arenaParams ∷ WorldGenParams
arenaParams = defaultWorldGenParams { wgpSeed = 0, wgpWorldSize = 8 }

-- | Through the real envelope: encode the session exactly as a save
--   does, decode it exactly as a load does.
throughCodec ∷ SessionSnapshot → IO SessionSnapshot
throughCodec snap = do
    let meta  = snapshotSaveMetadata (SaveRequestMeta "tags" "t" False) snap
        bytes = encodeSessionSnapshot meta snap []
    case decodeSessionEnvelope HS.empty HS.empty bytes of
        Left err → fail ("decode: " <> T.unpack err)
        Right (_, back, _, _) → pure back

-- | A logger that keeps every entry, for the diagnostics a load emits.
capturingLogger ∷ IO (LoggerState, IO [LogEntry])
capturingLogger = do
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback
            (\e → atomicModifyIORef' ref (\es → (e : es, ()))) }
    pure (logger, reverse ⊚ readIORef ref)

stageOnly ∷ EngineEnv → LoggerState → SessionSnapshot
          → IO (Either Text ())
stageOnly env logger snap = do
    matReg ← readIORef (materialRegistryRef env)
    either (Left . renderStageError) (const (Right ()))
        ⊚ stageSession env logger
            (snapshotToSaveData (SaveRequestMeta "tags" "t" False) snap) matReg

-- | Stage and publish a session, as the load transaction does.
loadSession ∷ EngineEnv → LoggerState → SessionSnapshot → IO ()
loadSession env logger snap = do
    matReg ← readIORef (materialRegistryRef env)
    staged ← stageSession env logger
        (snapshotToSaveData (SaveRequestMeta "tags" "t" False) snap) matReg
        ⌦ either (\e → fail ("stage: " <> T.unpack (renderStageError e))) pure
    publishStagedSession env logger 424242 staged

-- | The assignments a captured session carries, as live targets.
sessionAssignments ∷ SessionSnapshot → [(TagTarget, Set.Set GameplayTag)]
sessionAssignments snap = sort $ concat
    [ [ (TargetUnit u, ts) | (u, ts) ← Map.toList (usnTags (pgsUnits page)) ]
      ⧺ [ (TargetBuilding b, ts) | (b, ts) ← Map.toList (bsnTags (pgsBuildings page)) ]
      ⧺ [ (TargetLocation (LocationTarget pid l), ts)
        | (l, ts) ← categoryAssignments
            (lisTags (wgpLocationInstances (pgsGenParams page))) ]
    | (pid, page) ← HM.toList (snapPages snap) ]

-- | A scene with assignments in every category on BOTH pages, the two
--   pages' location 1 tagged differently, and a tag shared across
--   pages and categories.
seededScene ∷ EngineEnv → LuaBackendState → IO [TagTarget]
seededScene env ls = do
    resetScene env
    uInc  ← newUnit env ls incPage
    uKeep ← newUnit env ls keepPage
    bInc  ← newBuilding env ls incPage oldTile
    bKeep ← newBuilding env ls keepPage oldTile
    lInc  ← newLocation env incPage (ChunkCoord 0 0)
    lKeep ← newLocation env keepPage (ChunkCoord 0 0)
    _ ← newUnit env ls incPage                       -- untagged
    _ ← edit env uInc  (AddTagsEdit (tags ["defender", "meeting_point"]))
    _ ← edit env uKeep (AddTagsEdit (tags ["defender"]))
    _ ← edit env bInc  (AddTagsEdit (tags ["depot"]))
    _ ← edit env bKeep (AddTagsEdit (tags ["depot", "far"]))
    _ ← edit env lInc  (AddTagsEdit (tags ["landmark"]))
    _ ← edit env lKeep (AddTagsEdit (tags ["landmark", "far"]))
    pure [uInc, uKeep, bInc, bKeep, lInc, lKeep]

-- | Requirements 6 and 7.
sessionSpec ∷ SpecWith (EngineEnv, LuaBackendState)
sessionSpec = describe "whole-session save and load" $ do

    it "captures each page's own assignments under the page filter, and \
       \round-trips a nonempty session through the real codec exactly" $
        \(env, ls) → do
            targets ← seededScene env ls
            snap ← capturedSession env
            live ← assignedNow env
            sessionAssignments snap `shouldBe` sort live
            length live `shouldBe` length targets
            -- Partitioned, not copied: each page carries only its own.
            forM_ (HM.elems (snapPages snap)) $ \page → do
                Map.keysSet (usnTags (pgsUnits page)) `shouldSatisfy`
                    all (`HM.member` usnInstances (pgsUnits page))
                Map.keysSet (bsnTags (pgsBuildings page)) `shouldSatisfy`
                    all (`HM.member` bsnInstances (pgsBuildings page))
            back ← throughCodec snap
            back `shouldBe` snap
            sessionAssignments back `shouldBe` sort live

    it "a load replaces every outgoing assignment and installs the saved \
       \ones with their reverse indexes rebuilt" $ \(env, ls) → do
        _ ← seededScene env ls
        snap ← capturedSession env ⌦ throughCodec
        let saved = sessionAssignments snap
        -- The outgoing session moves on: new tags, a new tagged unit.
        extra ← newUnit env ls incPage
        _ ← edit env extra (AddTagsEdit (tags ["outgoing"]))
        v0 ← view env
        forM_ (map fst (liveTagAssignments v0)) $ \t →
            edit env t (AddTagsEdit (tags ["outgoing"]))
        (logger, _) ← capturingLogger
        loadSession env logger snap
        sort ⊚ assignedNow env `shouldReturn` saved
        coherent env
        -- The reverse indexes answer from the restored state alone.
        defenders ← queryNow env UnitCategory (tagged (tg "defender"))
        sort defenders `shouldBe` sort
            [ t | (t@(TargetUnit _), ts) ← saved, Set.member (tg "defender") ts ]
        far ← sort ⊚ (concat ⊚ mapM (\c → queryNow env c (tagged (tg "far")))
                                    [BuildingCategory, LocationCategory])
        far `shouldBe` sort [ t | (t, ts) ← saved, Set.member (tg "far") ts ]
        queryNow env UnitCategory (tagged (tg "outgoing")) `shouldReturn` []
        -- Page scoping: the two pages' location 1 kept their own tags.
        tagsNow env (TargetLocation (LocationTarget incPage (LocationInstanceId 1)))
            `shouldReturn` Just (tags ["landmark"])
        tagsNow env (TargetLocation (LocationTarget keepPage (LocationInstanceId 1)))
            `shouldReturn` Just (tags ["landmark", "far"])

    it "a load whose staging fails before publication leaves the live \
       \objects and their tags exactly as they were" $ \(env, ls) → do
        _ ← seededScene env ls
        snap ← capturedSession env
        before ← assignedNow env
        unitsBefore ← umInstances ⊚ readIORef (unitManagerRef env)
        let broken = snap { snapPages = HM.adjust
                (\page → page { pgsUnits = (pgsUnits page)
                    { usnInstances = HM.map (\u → u { uisDefName = "no_such_unit" })
                                            (usnInstances (pgsUnits page)) } })
                incPage (snapPages snap) }
        (logger, _) ← capturingLogger
        stageOnly env logger broken ⌦ (`shouldSatisfy` isLeft')
        assignedNow env `shouldReturn` before
        (umInstances ⊚ readIORef (unitManagerRef env)) `shouldReturn` unitsBefore

    it "a saved assignment naming no existing target is reported at both \
       \boundaries and dropped, never restored" $ \(env, ls) → do
        _ ← seededScene env ls
        snap0 ← capturedSession env
        let snap = snap0 { snapPages = HM.adjust
                (\page → page
                    { pgsUnits = (pgsUnits page)
                        { usnTags = Map.insert (UnitId 9999) (tags ["ghost"])
                                               (usnTags (pgsUnits page)) }
                    , pgsGenParams = let params = pgsGenParams page
                                         lis = wgpLocationInstances params
                                     in params { wgpLocationInstances = lis
                        { lisTags = setCategoryTags (LocationInstanceId 77)
                                        (tags ["ghost"]) (lisTags lis) } } })
                incPage (snapPages snap0) }
        sessionIntegrityErrors snap `shouldBe` []
        map ieRefValue (filter ((≡ "dangling-reference") . ieCode)
                               (sessionIntegrityWarnings snap))
            `shouldMatchList` ["9999", "77"]
        back ← throughCodec snap
        (logger, entries) ← capturingLogger
        loadSession env logger back
        sort ⊚ assignedNow env `shouldReturn` sessionAssignments snap0
        msgs ← map leMessage ⊚ entries
        filter ("dropped gameplay tags" `T.isInfixOf`) msgs
            `shouldSatisfy` ((≡ 2) . length)

    it "a saved assignment naming another page's unit is a wrong-page \
       \integrity error, and the save cannot be decoded" $ \(env, ls) → do
        TargetUnit uInc : _ ← seededScene env ls
        snap0 ← capturedSession env
        let snap = snap0 { snapPages = HM.adjust
                (\page → page { pgsUnits = (pgsUnits page)
                    { usnTags = Map.insert uInc (tags ["stolen"])
                                           (usnTags (pgsUnits page)) } })
                keepPage (snapPages snap0) }
        map ieCode (sessionIntegrityErrors snap) `shouldBe` ["wrong-page"]
        let meta = snapshotSaveMetadata (SaveRequestMeta "tags" "t" False) snap
        isLeft' (decodeSessionEnvelope HS.empty HS.empty
                    (encodeSessionSnapshot meta snap []))
            `shouldBe` True

-- | Requirement 8: the wire, its migrations and its validation.
wireSpec ∷ SpecWith (EngineEnv, LuaBackendState)
wireSpec = describe "the component wire" $ do

    it "a units v4, buildings v2 and world-pages v13 payload decodes with \
       \no assignments" $ \(env, ls) → do
        _ ← seededScene env ls
        snap ← capturedSession env
        let pages = HM.elems (snapPages snap)
            units4 = S.encode (UnitsDTOv4
                [ PageUnitsDTOv4 (pgsPageId p)
                    (HM.map toUnitInstanceDTO (usnInstances (pgsUnits p)))
                | p ← pages ])
            buildings2 = S.encode (BuildingsDTOv2
                [ PageBuildingsDTOv2 (pgsPageId p)
                    (HM.map toBuildingInstanceDTO (bsnInstances (pgsBuildings p)))
                | p ← pages ])
        case ccDecode unitsCodec 4 units4 of
            Left e → expectationFailure (show e)
            Right (UnitsDTO slices) → do
                map puTags slices `shouldBe` map (const []) slices
                map (HM.size . puInstances) slices `shouldSatisfy` all (> 0)
        case ccDecode buildingsCodec 2 buildings2 of
            Left e → expectationFailure (show e)
            Right (BuildingsDTO slices) → map pbTags slices `shouldBe` map (const []) slices
        let pages13 = S.encode (WorldPagesDTOv13
                [ PageCoreDTOv13 (pgsPageId p) (toWorldGenParamsDTO (pgsGenParams p))
                    0 0 0 0 0 1 1 1 (pgsMapMode p) Nothing (pgsGeneratedId p)
                | p ← pages ])
        case ccDecode worldPagesCodec 13 pages13 of
            Left e → expectationFailure (show e)
            Right wp → do
                ccValidate worldPagesCodec wp `shouldBe` []
                [ () | p ← HM.elems (wpBase wp)
                     , not (nullCategoryIndex
                              (lisTags (wgpLocationInstances (pgsGenParams p)))) ]
                    `shouldBe` []
                -- The table itself survived: only its tags are new.
                sum [ instancesCount (wgpLocationInstances (pgsGenParams p))
                    | p ← HM.elems (wpBase wp) ] `shouldBe` 2

    it "tracked pre-tag fixtures decode with no general assignments — \
       \faction tags included — and re-encode at the new versions" $ \_ →
        forM_ [ "z6-unit-faction-profile", "z8-units-v4-physiology-overrides" ] $
            \name → do
                bytes ← BS.readFile ("test-headless/data/save-compat/" <> name <> ".bin")
                let luaNames = HS.fromList ["unit_ai", "building_spawn"]
                case decodeSessionEnvelope luaNames luaNames bytes of
                    Left err → expectationFailure (T.unpack err)
                    Right (_, snap, _, _) → do
                        (name, sessionAssignments snap) `shouldBe` (name, [])
                        -- z6's units DO carry faction tags; none of
                        -- them may become a general tag.
                        (name, sum (HM.size . usnInstances . pgsUnits
                                      <$> HM.elems (snapPages snap)))
                            `shouldSatisfy` ((> 0) . snd)

    it "refuses a malformed tag list in each component, naming it" $ \_ → do
        let page = WorldPageId "wire_page"
            unitErrs w = ccValidate unitsCodec (UnitsDTO [PageUnitsDTO page HM.empty w])
            bldErrs w = ccValidate buildingsCodec
                (BuildingsDTO [PageBuildingsDTO page HM.empty w])
        map cePhase (unitErrs [(UnitId 1, [""])]) `shouldBe` [ValidatePhase]
        unitErrs [(UnitId 1, [])] `shouldSatisfy` ((≡ 1) . length)
        unitErrs [(UnitId 1, ["a", "a"])] `shouldSatisfy` ((≡ 1) . length)
        unitErrs [(UnitId 1, ["a"]), (UnitId 1, ["b"])] `shouldSatisfy` ((≡ 1) . length)
        unitErrs [(UnitId 1, ["a", "b"]), (UnitId 2, ["a"])] `shouldBe` []
        map ceComponent (bldErrs [(BuildingId 3, [""])])
            `shouldBe` [buildingsComponentId]
        let core = PageCoreDTO page (toWorldGenParamsDTO arenaParams)
                       0 0 0 0 0 1 1 1 ZMDefault Nothing
                       (Just (fixtureGeneratedWorldIdForPage page))
            locErrs w = case ccDecode worldPagesCodec 14
                                 (S.encode (WorldPagesDTO [core w])) of
                Left e → [renderComponentError e]
                Right wp → map renderComponentError (ccValidate worldPagesCodec wp)
        locErrs [(LocationInstanceId 1, [])] `shouldSatisfy` ((≡ 1) . length)
        locErrs [(LocationInstanceId 1, ["ok"])] `shouldBe` []

-- * Small readers

instanceOf ∷ EngineEnv → UnitId → IO (Maybe UnitInstance)
instanceOf env uid = HM.lookup uid . umInstances ⊚ readIORef (unitManagerRef env)

isLeft' ∷ Either a b → Bool
isLeft' = either (const True) (const False)
