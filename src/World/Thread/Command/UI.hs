module World.Thread.Command.UI
    ( handleWorldShowCommand
    , handleWorldHideCommand
    , handleWorldSetMapModeCommand
    , handleWorldSetToolModeCommand
    ) where


import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, logWarn
                       , LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Constants (seaLevel)
import World.Generate (generateChunk)
import World.Generate.Constants (chunkLoadRadius)
import World.Generate.Timeline (applyTimelineFast)
import World.Geology (buildTimeline)
import World.Geology.Log (formatTimeline, formatPlatesSummary)
import World.Fluids (computeOceanMap, isOceanChunk)
import World.Plate (generatePlates, elevationAtGlobal)
import World.Preview (buildPreviewImage, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCache)
import World.Weather (initEarlyClimate, formatWeather, defaultClimateParams)
import World.Thread.Helpers (sendGenLog, unWorldPageId)
import World.Thread.ChunkLoading (maxChunksPerTick)

handleWorldShowCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldShowCommand env logger pageId = do
    logDebug logger CatWorld $ "Showing world: " <> unWorldPageId pageId

    -- Only worlds that actually exist may enter wmVisible. Inserting a
    -- nonexistent pageId would poison getActiveWorldId() (which reads the
    -- head of wmVisible) and silently retarget every current-world API.
    -- atomicModifyIORef' returns whether the world was found so the
    -- existence check and the visible-list mutation share one consistent
    -- snapshot of the manager.
    found ← atomicModifyIORef' (worldManagerRef env) $ \mgr →
        case lookup pageId (wmWorlds mgr) of
            Nothing → (mgr, False)
            Just _
                | pageId `elem` wmVisible mgr → (mgr, True)
                | otherwise →
                    (mgr { wmVisible = pageId : wmVisible mgr }, True)

    if not found
    then logWarn logger CatWorld $
        "Ignoring world.show for nonexistent world: " <> unWorldPageId pageId
    else do
        mgr ← readIORef (worldManagerRef env)
        logDebug logger CatWorld $
            "Visible worlds after show: " <> T.pack (show $ length $ wmVisible mgr)

        -- Activate this world in the sim thread. The sim no longer holds the
        -- tile ref — it emits WorldApplyFluids back to the world thread (the
        -- sole writer of wsTilesRef) — so this is just a per-world "is
        -- active" signal.
        Q.writeQueue (simQueue env) (SimActivateWorld pageId)

handleWorldHideCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldHideCommand env logger pageId = do
    logDebug logger CatWorld $ "Hiding world: " <> unWorldPageId pageId

    -- Only deactivate sim for a world that was actually visible. Hiding an
    -- invalid / already-hidden page is a no-op for sim state, and hiding one
    -- world never tears down the others' sim (per-world deactivate, #55).
    wasVisible ← atomicModifyIORef' (worldManagerRef env) $ \mgr →
        ( mgr { wmVisible = filter (/= pageId) (wmVisible mgr) }
        , pageId `elem` wmVisible mgr )

    -- Clear this page's cursor selection on hide: the ground-item
    -- selection (#175) and the zoom-map chunk / zoomed-in tile selection
    -- (#183). All three live in the per-world cursor (wsCursorRef), but a
    -- Lua-side deselect resolves through activeWorld, which head-falls-back
    -- to another registered world once this page leaves wmVisible — so a
    -- Lua deselect could clear the wrong world and leave this one's
    -- selection live. For the tile/chunk selection that strands the HUD on
    -- re-show: resolveActiveWorld still resolves this page (head-fallback
    -- over wmWorlds), so 'pollCursorInfo' sees no active-world change and
    -- the cursor snapshot sees no selection change, so it never re-sends
    -- the info text — leaving a live selection with an empty HUD panel
    -- (#183). Clearing here, keyed on the exact pageId being hidden, is
    -- race-free and always targets the right world; the next cursor poll
    -- then blanks the panel and updates the snapshot to match.
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { selectedGroundItem = Nothing
                    , zoomSelectedPos    = Nothing
                    , worldSelectedTile  = Nothing }, ())
        Nothing → pure ()

    when wasVisible $
        Q.writeQueue (simQueue env) (SimDeactivateWorld pageId)

handleWorldSetMapModeCommand ∷ EngineEnv → LoggerState → WorldPageId
    → ZoomMapMode → IO ()
handleWorldSetMapModeCommand env logger pageId mode = do
    logDebug logger CatWorld $
        "Setting map mode for world: " <> unWorldPageId pageId
        <> " to " <> T.pack (show mode)
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            writeIORef (wsMapModeRef worldState) mode
            logInfo logger CatWorld $
                "Map mode updated for world: " <> unWorldPageId pageId
                <> ", new mode: " <> T.pack (show mode)
        Nothing →
            logDebug logger CatWorld $
                "World not found for map mode update: " <> unWorldPageId pageId

handleWorldSetToolModeCommand ∷ EngineEnv → LoggerState → WorldPageId → ToolMode → IO ()
handleWorldSetToolModeCommand env logger pagedId mode = do
        mgr ← readIORef (worldManagerRef env)
        case lookup pagedId (wmWorlds mgr) of
            Just worldState → do
                writeIORef (wsToolModeRef worldState) mode
                logInfo logger CatWorld $
                    "Tool mode updated for world: " <> unWorldPageId pagedId
                    <> ", new mode: " <> T.pack (show mode)
            Nothing →
                logDebug logger CatWorld $
                    "World not found for tool mode update: " <> unWorldPageId pagedId
