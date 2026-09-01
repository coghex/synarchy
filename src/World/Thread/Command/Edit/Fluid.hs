{-# LANGUAGE Strict #-}

-- | Fluid-tile edit handler. Split out of "World.Thread.Command.Edit"
--   (issue #563).
module World.Thread.Command.Edit.Fluid
    ( handleWorldSetFluidTileCommand
    ) where

import UPrelude
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Generate.Coordinates (globalToChunk)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Edit.Apply (applyEdit)
import World.Thread.Command.Edit.Sync (syncEditToSim)
import World.Plant.Validate (revalidatePlantDesignations)
import World.Construct.Revalidate
    (ConstructScope(..), revalidateConstructDesignations)
import World.Flora.Designation (replaceChunkForgettingFlora)

-- | Place one tile of fluid on top of the column at (gx, gy). Records
--   the edit in the world's log; in-memory mutation uses the same
--   `applyEdit` helper.
handleWorldSetFluidTileCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → FluidType → IO ()
handleWorldSetFluidTileCommand env logger pageId gx gy fluidType = do
    let wsc = toWorldSimCapability env
    mgr ← readIORef (wsWorldManagerRef wsc)
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
                          <> tshow gx <> "," <> tshow gy
                Just lc → do
                    let lc' = applyEdit edit lc
                    -- #1854 requirement 16: an edit that takes the tile's
                    -- rooted flora with it must take that plant's
                    -- designation and regrowth timer too, or an orphan
                    -- entry outlives the plant it addressed.
                    replaceChunkForgettingFlora ws lc lc'
                    atomicModifyIORef' (wsEditsRef ws) $ \es →
                        (appendEdit coord edit es, ())
                    -- Re-seed the sim with the placed fluid so it flows /
                    -- settles instead of being overwritten by stale sim
                    -- output (#60).
                    syncEditToSim wsc pageId ws lc'
                    bumpQuadCacheGen ws
                    writeIORef (wsZoomQuadCacheRef ws) Nothing
                    writeIORef (wsBgQuadCacheRef ws)   Nothing
                    -- #1858: fluid raises the resolved surface, so a
                    -- flooded tilled tile stops being plantable.
                    _ ← revalidatePlantDesignations logger ws
                    -- #1844: and it moves the very surface a structure
                    -- designation captured as its 'cdZ', so the same
                    -- edit can strand a build site under water. Scoped
                    -- to the edited tile, like every other live hook.
                    _ ← revalidateConstructDesignations env logger ws
                            (ConstructKeys [(gx, gy)])
                    logDebug logger CatWorld $
                        "Placed fluid " <> tshow fluidType
                          <> " at " <> tshow gx <> ","
                          <> tshow gy
