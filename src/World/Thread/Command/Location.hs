{-# LANGUAGE Strict #-}
module World.Thread.Command.Location
    ( handleWorldMarkLocationContentsSpawnedCommand
    , handleWorldSetLocationLifecycleCommand
    , handleWorldMarkLocationStampedCommand
    ) where

import UPrelude
import qualified Data.HashSet as HS
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Location.Instance
    ( LocationInstanceId, LocationLifecycle
    , markLocationContentsSpawned, setLocationLifecycle )
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
handleWorldMarkLocationStampedCommand
    ∷ WorldSimCapability → WorldPageId → Int → Int → IO ()
handleWorldMarkLocationStampedCommand wsc pageId gx gy =
    withPageParams wsc pageId $ \params → params
        { wgpLocationStamped =
            HS.insert (fst (globalToChunk gx gy)) (wgpLocationStamped params) }
