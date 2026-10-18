{-# LANGUAGE Strict #-}
-- | Installing a constructed scenario unit (#2710, SCN-10 of epic
--   #2698): the commit that makes a "Scenario.Units.Build" result live,
--   and the completion result the paused scenario loader (SCN-13) reads.
--
--   __One commit, behind the page-incarnation fence.__ The unit id is
--   allocated, the instance, its tags and its scenario reference go into
--   the unit manager, and its simulation state goes into the unit thread
--   state, all inside the page-lifecycle lock after re-checking that the
--   page is still the incarnation the unit was built for — the spawn
--   handler's fence ('Unit.Thread.Command.Spawn'), applied to the same
--   two writes. A replaced or destroyed page therefore admits nothing: no
--   id, no manager-only or simulation-only unit, no reference.
--
--   __Items attach all or nothing, after the owner.__ The item trees'
--   scenario references and tags are attached through
--   'Gameplay.Tags.Live.commitItemTreeAssignments', which requires every
--   item to be live, so it runs once the unit holding them is installed.
--   If it refuses, the unit is retired again in one step (instance, sim
--   state, tags, reference): a unit whose items could not take their
--   identities is not a completed unit, and nothing of it remains.
--
--   __Paused readiness.__ The Lua per-unit reconciliation decides deaths,
--   collapses, crawls, revivals and the calorie strength discount. After
--   the items attach, the commit runs the readiness hook ('uirReady'; in
--   production 'Scenario.Units.Ready', which calls
--   @unit_resources.settleScenarioUnit@: the SAME @reconcileUnit@ entry
--   the tick runs, at zero elapsed time, with the unit's effects
--   recorded). A death it would cause rejects the unit; the pose it would
--   establish is applied through the handlers the queued @UnitCollapse@ /
--   @UnitCrawl@ / @UnitRevive@ commands run ("Unit.Thread.Command.Pose"),
--   synchronously, so nothing is left queued. A hook that fails retires
--   the unit like a refused attachment.
--
--   __Completion.__ 'UnitInstalled' is returned only once all of that has
--   happened: the instance, its simulation state and every identity are
--   live, and the native and Lua-owned initialization is finished.
--   Accepting a request or allocating an id is never reported as
--   completion.
--
--   The commit takes the live references it writes as arguments
--   ('UnitInstallRefs'), as "Gameplay.Tags.Live" does, so the caller
--   (the session loader, or a test) decides which managers it targets.
module Scenario.Units.Install
    ( UnitInstallRefs(..)
    , UnitCompletion(..)
    , UnitInstallFailure(..)
    , Readiness(..)
    , installBuiltUnit
    , installUnitBuilds
    ) where

import UPrelude
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Building.Types (BuildingManager)
import Gameplay.Tags.Index (setCategoryTags)
import Gameplay.Tags.Live (commitItemTreeAssignments)
import Gameplay.Tags.Tag (ItemTarget)
import Scenario.Items.Build (itemRefTargets, itemTagTargets)
import Scenario.Types (DiagnosticEffect(..), DiagnosticReason(..), ScenarioDiagnostic(..))
import Scenario.Units.Build (BuiltUnit(..), UnitBuild(..))
import Unit.Anim (poseTag)
import Unit.Sim.Types (Pose(..), UnitSimState(..), UnitThreadState(..))
import Gameplay.Tags.ItemLifecycle (withDestroyedItems)
import Unit.Thread.Command.Lifecycle (destroyedUnit)
import Unit.Thread.Command.Pose
    (handleUnitCollapseCommand, handleUnitCrawlCommand, handleUnitReviveCommand)
import Unit.Types
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Residency (ChunkGeneration)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..))

-- | The live state a commit writes, and the lock that orders it against
--   page transitions ('Engine.Core.Capability.WorldSim.withPageLifecycle').
data UnitInstallRefs = UnitInstallRefs
    { uirUnits     ∷ IORef UnitManager
    , uirSims      ∷ IORef UnitThreadState
    , uirBuildings ∷ IORef BuildingManager
    , uirWorld     ∷ IORef WorldManager
    , uirLifecycle ∷ ∀ α. IO α → IO α
    , uirReady     ∷ UnitId → IO (Either Text Readiness)
      -- ^ The paused-readiness hook: settles the unit's Lua-owned derived
      --   state and answers what to establish. 'Left' is the hook's error.
    }

-- | What the paused-readiness hook answered.
data Readiness
    = ReadyPose !(Maybe Text)
      -- ^ the last pose the zero-time reconciliation would establish
      --   (@"collapsed"@, @"crawling"@, @"standing"@ for a revival), or
      --   'Nothing' to keep the built one
    | ReadyLethal !Text
      -- ^ the zero-time reconciliation would kill this unit now; the cause
    deriving (Show, Eq)

data UnitInstallFailure
    = PageNotAdmitting
      -- ^ the page is gone, or is no longer the incarnation the unit was
      --   built for
    | ReferenceHeld !Text
      -- ^ another live unit already holds this scenario reference
    | ItemsRefused ![ItemTarget]
      -- ^ the unit's item trees could not take their references and
      --   tags; the unit was retired again
    | ReadinessFailed !Text
      -- ^ the paused-readiness hook failed or answered an unknown pose;
      --   the unit was retired again
    | AlreadyLethal !Text
      -- ^ the gameplay lethality rule rejects the unit's health; retired
      --   again, and reported by 'installUnitBuilds' as a rejection
    deriving (Show, Eq)

-- | The terminal result for one unit entry.
data UnitCompletion
    = UnitInstalled !UnitId !Text
      -- ^ live, complete, with its scenario reference
    | UnitRejected ![ScenarioDiagnostic]
      -- ^ construction rejected the entry; nothing was installed
    | UnitNotInstalled !Text !UnitInstallFailure
      -- ^ construction succeeded but the commit did not; nothing remains
    deriving (Show, Eq)

-- | Commit one constructed unit onto @page@, admitted for incarnation
--   @epoch@.
installBuiltUnit ∷ UnitInstallRefs → WorldPageId → ChunkGeneration → BuiltUnit
                 → IO (Either UnitInstallFailure UnitId)
installBuiltUnit refs page epoch b = do
    admitted ← uirLifecycle refs $ do
        wm ← readIORef (uirWorld refs)
        ours ← case lookup page (wmWorlds wm) of
            Nothing → pure False
            Just ws → (≡ epoch) ⊚ pageIncarnation ws
        if not ours then pure (Left PageNotAdmitting) else do
            r ← atomicModifyIORef' (uirUnits refs) $ \um →
                if buRef b `elem` Map.elems (umScenarioRefs um)
                    then (um, Left (ReferenceHeld (buRef b)))
                    else
                        let (uid, um1) = nextUnitId um
                            inst = (buInstance b) { uiPage = page }
                        in ( um1 { umInstances = HM.insert uid inst (umInstances um1)
                                 , umTags = if Set.null (buTags b) then umTags um1
                                            else setCategoryTags uid (buTags b) (umTags um1)
                                 , umScenarioRefs = Map.insert uid (buRef b) (umScenarioRefs um1) }
                           , Right uid )
            forM_ r $ \uid → atomicModifyIORef' (uirSims refs) $ \uts →
                (uts { utsSimStates = HM.insert uid (buSimState b) (utsSimStates uts) }, ())
            pure r
    case admitted of
        Left why → pure (Left why)
        Right uid → do
            attached ← commitItemTreeAssignments (uirUnits refs) (uirBuildings refs) (uirWorld refs)
                           (itemTagTargets (buItems b)) (itemRefTargets (buItems b))
            case attached of
                Left targets → do
                    retire uid
                    pure (Left (ItemsRefused targets))
                Right () → do
                    settled ← uirReady refs uid
                    let failed why = do
                            retire uid
                            pure (Left why)
                    case settled of
                        Left why → failed (ReadinessFailed why)
                        Right (ReadyLethal cause) → failed (AlreadyLethal cause)
                        Right (ReadyPose Nothing) → pure (Right uid)
                        Right (ReadyPose (Just "upright")) → pure (Right uid)
                        Right (ReadyPose (Just "standing")) → establish uid Standing handleUnitReviveCommand
                        Right (ReadyPose (Just "collapsed")) → establish uid Collapsed handleUnitCollapseCommand
                        Right (ReadyPose (Just "crawling")) → establish uid Crawling handleUnitCrawlCommand
                        Right (ReadyPose (Just other)) →
                            failed (ReadinessFailed ("unknown readiness pose '" <> other <> "'"))
  where
    -- the queued pose command's own handler, run now, and the published
    -- mirror the unit thread would write from it
    establish uid pose handler = do
        handler (uirSims refs) uid
        uts ← readIORef (uirSims refs)
        let simPose = maybe pose usPose (HM.lookup uid (utsSimStates uts))
        atomicModifyIORef' (uirUnits refs) $ \um →
            ( um { umInstances = HM.adjust (\i → i { uiPose = poseTag simPose }) uid (umInstances um) }
            , () )
        pure (Right uid)
    -- the unit-destroy command's own step ('destroyedUnit'): instance,
    -- selection, tags and reference in one modify, and every item it held
    -- forgotten by the epoch-guarded item-destruction cleanup
    -- ('withDestroyedItems'), which releases their tags and references
    retire uid = uirLifecycle refs $ do
        withDestroyedItems (uirWorld refs) $
            atomicModifyIORef' (uirUnits refs) $ \um →
                let (um', destroyed) = destroyedUnit uid um in (um', ((), destroyed))
        atomicModifyIORef' (uirSims refs) $ \uts →
            (uts { utsSimStates = HM.delete uid (utsSimStates uts) }, ())

-- | Commit every build of one owner list in order, one completion per
--   entry.
installUnitBuilds ∷ UnitInstallRefs → WorldPageId → ChunkGeneration → [UnitBuild]
                  → IO [UnitCompletion]
installUnitBuilds refs page epoch = mapM one
  where
    one ub = case ubUnit ub of
        Nothing → pure (UnitRejected (ubDiagnostics ub))
        Just b → installBuiltUnit refs page epoch b ≫= \case
            Right uid → pure (UnitInstalled uid (buRef b))
            -- the same structured rejection as the native death checks
            Left (AlreadyLethal cause) → pure $ UnitRejected
                (ubDiagnostics ub
                 ⧺ [ScenarioDiagnostic (buPath b)
                        (InvalidValue ("a living unit: the injury pass would kill it ("
                                       <> cause <> ")"))
                        EntryRejected])
            Left why → pure (UnitNotInstalled (buRef b) why)
