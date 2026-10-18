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
--   __Completion.__ 'UnitInstalled' is returned only once all of that has
--   happened: the instance, its simulation state and every identity are
--   live, and no Lua hook is outstanding — construction already ran the
--   one pool-seeding authority ("Scenario.Units.Build"). Accepting a
--   request or allocating an id is never reported as completion.
--
--   The commit takes the live references it writes as arguments
--   ('UnitInstallRefs'), as "Gameplay.Tags.Live" does, so the caller
--   (the session loader, or a test) decides which managers it targets.
module Scenario.Units.Install
    ( UnitInstallRefs(..)
    , UnitCompletion(..)
    , UnitInstallFailure(..)
    , installBuiltUnit
    , installUnitBuilds
    ) where

import UPrelude
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Building.Types (BuildingManager)
import Gameplay.Tags.Index (deleteCategoryTarget, setCategoryTags)
import Gameplay.Tags.Live (commitItemTreeAssignments)
import Gameplay.Tags.Tag (ItemTarget)
import Scenario.Items.Build (itemRefTargets, itemTagTargets)
import Scenario.Types (ScenarioDiagnostic)
import Scenario.Units.Build (BuiltUnit(..), UnitBuild(..))
import Unit.Sim.Types (UnitThreadState(..))
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
    }

data UnitInstallFailure
    = PageNotAdmitting
      -- ^ the page is gone, or is no longer the incarnation the unit was
      --   built for
    | ReferenceHeld !Text
      -- ^ another live unit already holds this scenario reference
    | ItemsRefused ![ItemTarget]
      -- ^ the unit's item trees could not take their references and
      --   tags; the unit was retired again
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
                Right () → pure (Right uid)
                Left targets → do
                    retire uid
                    pure (Left (ItemsRefused targets))
  where
    retire uid = uirLifecycle refs $ do
        atomicModifyIORef' (uirUnits refs) $ \um →
            ( um { umInstances = HM.delete uid (umInstances um)
                 , umSelected = HS.delete uid (umSelected um)
                 , umTags = deleteCategoryTarget uid (umTags um)
                 , umScenarioRefs = Map.delete uid (umScenarioRefs um) }
            , () )
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
        Just b → either (UnitNotInstalled (buRef b)) (\uid → UnitInstalled uid (buRef b))
                     ⊚ installBuiltUnit refs page epoch b
