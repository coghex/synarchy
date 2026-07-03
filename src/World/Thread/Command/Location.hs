{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Command.Location
    ( handleWorldMarkLocationContentsSpawnedCommand
    , handleWorldMarkLocationStampedCommand
    ) where

import UPrelude
import qualified Data.HashSet as HS
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import World.Types
import World.Generate.Coordinates (globalToChunk)

-- | One-time content-spawn flag (#90) — see
--   'World.Command.Types.WorldMarkLocationContentsSpawned'. A no-op
--   when the page or its gen params aren't live (mirrors the other
--   cursor/designation command handlers).
handleWorldMarkLocationContentsSpawnedCommand
    ∷ EngineEnv → WorldPageId → Int → Int → IO ()
handleWorldMarkLocationContentsSpawnedCommand env pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState → do
            let (coord, _) = globalToChunk gx gy
            atomicModifyIORef' (wsGenParamsRef worldState) $ \mParams →
                case mParams of
                    Nothing → (mParams, ())
                    Just params →
                        ( Just params
                            { wgpLocationContentsSpawned =
                                HS.insert coord
                                    (wgpLocationContentsSpawned params)
                            }
                        , ()
                        )

-- | One-time geometry-stamp flag (#424) — see
--   'World.Command.Types.WorldMarkLocationStamped'. A no-op when the page
--   or its gen params aren't live (mirrors the content-spawned handler
--   above).
handleWorldMarkLocationStampedCommand
    ∷ EngineEnv → WorldPageId → Int → Int → IO ()
handleWorldMarkLocationStampedCommand env pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState → do
            let (coord, _) = globalToChunk gx gy
            atomicModifyIORef' (wsGenParamsRef worldState) $ \mParams →
                case mParams of
                    Nothing → (mParams, ())
                    Just params →
                        ( Just params
                            { wgpLocationStamped =
                                HS.insert coord (wgpLocationStamped params)
                            }
                        , ()
                        )
