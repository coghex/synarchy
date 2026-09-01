{-# LANGUAGE Strict #-}
module World.Thread.Command.Location
    ( handleWorldMarkLocationContentsSpawnedCommand
    , handleWorldRegisterLocationEncounterOccupantsCommand
    , handleWorldRegisterLocationSignificantSpawnCommand
    , handleWorldSetLocationEncounterOccupantStateCommand
    , handleWorldSetLocationEncounterEpisodeStateCommand
    , handleWorldSetLocationLifecycleCommand
    , handleWorldMarkLocationStampedCommand
    ) where

import UPrelude
import qualified Data.HashSet as HS
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.Log (logWarn, LogCategory(..), LoggerState)
import Structure.Types (StructureCommitWindow(..), takeDeclinedInWindow)
import Location.Instance
    ( LocationEncounterOccupant(..), LocationInstanceId, LocationLifecycle
    , adjustLocationEncounterOccupant
    , markLocationContentsSpawned, registerLocationEncounterOccupants
    , registerLocationSignificantSpawn
    , setLocationEncounterEpisodeState, setLocationLifecycle )
import Unit.Types (UnitId)
import World.Types
import World.Generate.Coordinates (globalToChunk)

-- | Apply a pure edit to one page's live gen params. A no-op when the
--   page or its gen params aren't live (mirrors the other
--   cursor/designation command handlers).
withPageParams
    ∷ WorldSimCapability → WorldPageId
    → (WorldGenParams → WorldGenParams) → IO ()
withPageParams wsc pageId f = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState →
            atomicModifyIORef' (wsGenParamsRef worldState) $ \mParams →
                (fmap f mParams, ())

-- | One-time content-spawn flag (#90), per instance since #911 — see
--   'World.Command.Types.WorldMarkLocationContentsSpawned'. An unknown
--   instance id is a no-op.
handleWorldMarkLocationContentsSpawnedCommand
    ∷ WorldSimCapability → WorldPageId → LocationInstanceId → IO ()
handleWorldMarkLocationContentsSpawnedCommand wsc pageId iid =
    withPageParams wsc pageId $ \params → params
        { wgpLocationInstances =
            markLocationContentsSpawned iid (wgpLocationInstances params) }

handleWorldRegisterLocationEncounterOccupantsCommand
    ∷ WorldSimCapability → WorldPageId → LocationInstanceId
    → [(UnitId, (Float, Float))] → IO ()
handleWorldRegisterLocationEncounterOccupantsCommand wsc pageId iid occupants =
    withPageParams wsc pageId $ \params → params
        { wgpLocationInstances = registerLocationEncounterOccupants iid occupants
            (wgpLocationInstances params) }

-- | Bind one spawned guaranteed significant item to its obligation
--   slot (#917) — see
--   'World.Command.Types.WorldRegisterLocationSignificantSpawn'. An
--   unknown instance id, an unknown slot, and a slot already bound are
--   all no-ops: 'registerLocationSignificantSpawn' is write-once, so a
--   retried content spawn cannot repoint an obligation.
handleWorldRegisterLocationSignificantSpawnCommand
    ∷ WorldSimCapability → WorldPageId → LocationInstanceId → Int → Word64
    → IO ()
handleWorldRegisterLocationSignificantSpawnCommand wsc pageId iid slot itemId =
    withPageParams wsc pageId $ \params →
        case registerLocationSignificantSpawn iid slot itemId
                 (wgpLocationInstances params) of
            Just instances' → params { wgpLocationInstances = instances' }
            Nothing         → params

handleWorldSetLocationEncounterOccupantStateCommand
    ∷ WorldSimCapability → WorldPageId → LocationInstanceId → UnitId
    → Bool → Bool → IO ()
handleWorldSetLocationEncounterOccupantStateCommand wsc pageId iid uid
        engaged returning =
    withPageParams wsc pageId $ \params → params
        { wgpLocationInstances = adjustLocationEncounterOccupant iid uid
            (\o → o { leoEngaged = engaged
                    , leoReturning = returning
                    }) (wgpLocationInstances params)
        }

handleWorldSetLocationEncounterEpisodeStateCommand
    ∷ WorldSimCapability → WorldPageId → LocationInstanceId
    → Bool → Bool → Bool → IO ()
handleWorldSetLocationEncounterEpisodeStateCommand wsc pageId iid active
        aggressionAnnounced disengageAnnounced =
    withPageParams wsc pageId $ \params → params
        { wgpLocationInstances = setLocationEncounterEpisodeState iid active
            aggressionAnnounced disengageAnnounced
            (wgpLocationInstances params)
        }

-- | Lifecycle promotion (#911) — see
--   'World.Command.Types.WorldSetLocationLifecycle'. An unknown instance
--   id, or a request that does not move the instance strictly forward,
--   leaves the table untouched.
handleWorldSetLocationLifecycleCommand
    ∷ WorldSimCapability → WorldPageId → LocationInstanceId
    → LocationLifecycle → IO ()
handleWorldSetLocationLifecycleCommand wsc pageId iid lifecycle =
    withPageParams wsc pageId $ \params →
        case setLocationLifecycle iid lifecycle (wgpLocationInstances params) of
            Just instances' → params { wgpLocationInstances = instances' }
            Nothing         → params

-- | One-time geometry-stamp flag (#424) — see
--   'World.Command.Types.WorldMarkLocationStamped'. Deliberately still
--   CHUNK-keyed (#911 left it alone): "has this chunk had its location's
--   geometry written into it IN FULL" (#1719) is genuinely about the
--   chunk, and that is what makes stamping idempotent under player edits.
--   Only a stamp whose every attempted placement succeeded queues this;
--   a partial one is left unmarked to be retried on the next load.
--
--   __That Lua-side answer is necessary, not sufficient (#2051).__ It is
--   taken from @structure.place@'s synchronous return, which is true as
--   soon as the placement is staged and its 'WorldSetStructure' queued —
--   the target chunk can still evict before this thread's own residency
--   check, which declines the commit, retracts the stage and appends no
--   edit. The command therefore carries the invocation's
--   'StructureCommitWindow', and the marker is withheld when any attempt
--   in it was declined, leaving the every-load dispatch in
--   "World.Thread.ChunkLoading" to retry the whole builder next load.
--
--   The queue is FIFO and this thread dispatches it sequentially, so
--   every 'WorldSetStructure' the window names has already been decided
--   by the time this command runs: the answer is complete, never a
--   snapshot of work still in flight.
--
--   Reading the window also RETIRES the declines it consumed, so the
--   retry is judged on its own attempts. A command carrying no window
--   (the bare console verb) marks unconditionally, as before.
handleWorldMarkLocationStampedCommand
    ∷ WorldSimCapability → LoggerState → WorldPageId → Int → Int
    → Maybe StructureCommitWindow → IO ()
handleWorldMarkLocationStampedCommand wsc logger pageId gx gy mWindow = do
    declined ← case mWindow of
        Nothing     → pure False
        Just window → do
            mgr ← readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Nothing → pure False
                Just ws → atomicModifyIORef' (wsStructureStageRef ws) $
                    takeDeclinedInWindow window
    if declined
        then logWarn logger CatWorld $
            "Location stamp at " <> tshow gx <> "," <> tshow gy
              <> " on page " <> unWorldPageId pageId
              <> " had a placement declined after it was queued — chunk left"
              <> " unmarked, will retry on next load"
        else withPageParams wsc pageId $ \params → params
            { wgpLocationStamped =
                HS.insert (fst (globalToChunk gx gy)) (wgpLocationStamped params) }
