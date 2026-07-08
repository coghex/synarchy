{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Fluid-tile edit handler. Split out of "World.Thread.Command.Edit"
--   (issue #563).
module World.Thread.Command.Edit.Fluid
    ( handleWorldSetFluidTileCommand
    ) where

import UPrelude
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Generate.Coordinates (globalToChunk)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Edit.Apply (applyEdit)
import World.Thread.Command.Edit.Sync (syncEditToSim)
import World.Thread.Helpers (unWorldPageId)

-- | Place one tile of fluid on top of the column at (gx, gy). Records
--   the edit in the world's log; in-memory mutation uses the same
--   `applyEdit` helper.
handleWorldSetFluidTileCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → FluidType → IO ()
handleWorldSetFluidTileCommand env logger pageId gx gy fluidType = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for set fluid: " <> unWorldPageId pageId
        Just ws → do
            let (coord, _) = globalToChunk gx gy
                edit = WeSetFluidTile gx gy fluidType
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing →
                    logWarn logger CatWorld $
                        "Chunk not loaded for set fluid at "
                          <> T.pack (show gx) <> "," <> T.pack (show gy)
                Just lc → do
                    let lc' = applyEdit edit lc
                    atomicModifyIORef' (wsTilesRef ws) $ \w →
                        (insertChunk lc' w, ())
                    atomicModifyIORef' (wsEditsRef ws) $ \es →
                        (appendEdit coord edit es, ())
                    -- Re-seed the sim with the placed fluid so it flows /
                    -- settles instead of being overwritten by stale sim
                    -- output (#60).
                    syncEditToSim env pageId lc'
                    bumpQuadCacheGen ws
                    writeIORef (wsZoomQuadCacheRef ws) Nothing
                    writeIORef (wsBgQuadCacheRef ws)   Nothing
                    logDebug logger CatWorld $
                        "Placed fluid " <> T.pack (show fluidType)
                          <> " at " <> T.pack (show gx) <> ","
                          <> T.pack (show gy)
