module World.Thread.Command.UI
    ( handleWorldShowCommand
    , handleWorldHideCommand
    , handleWorldSetMapModeCommand
    , handleWorldSetToolModeCommand
    ) where


import UPrelude
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.Log (logInfo, logDebug, logWarn, LogCategory(..), LoggerState)
import Engine.Graphics.Solar (maxSolarPages)
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
import Sim.Topology (SimTopology(..))
import World.Types
import World.Render.Zoom.Types (ZoomMapMode(..))

-- | What one 'handleWorldShowCommand' actually did, decided inside the
--   manager's own atomic update so the reason and the state it was
--   decided from cannot disagree.
data ShowOutcome
    = ShowApplied         -- ^ visible now (or already was)
    | ShowNoSuchPage      -- ^ no such page in @wmWorlds@
    | ShowAtVisibleLimit  -- ^ 'maxSolarPages' pages are visible already
    deriving (Eq, Show)

handleWorldShowCommand ∷ WorldSimCapability → LoggerState → WorldPageId → IO ()
handleWorldShowCommand wsc logger pageId = do
    logDebug logger CatWorld $ "Showing world: " <> unWorldPageId pageId

    -- Only worlds that actually exist may enter wmVisible. Inserting a
    -- nonexistent pageId would poison getActiveWorldId() (which reads the
    -- head of wmVisible) and silently retarget every current-world API.
    -- atomicModifyIORef' returns whether the world was found so the
    -- existence check and the visible-list mutation share one consistent
    -- snapshot of the manager.
    outcome ← atomicModifyIORef' (wsWorldManagerRef wsc) $ \mgr' →
      let mgr = completeSelectionChange mgr' in
        case lookup pageId (wmWorlds mgr) of
            Nothing → (mgr, ShowNoSuchPage)
            Just _
                | pageId `elem` wmVisible mgr → (mgr, ShowApplied)
                -- Every visible page must be lit by its own clock and
                -- circumference (#1869), and the render path can
                -- describe exactly 'maxSolarPages' of them per frame.
                -- Refusing here is what keeps that a contract rather
                -- than a best effort: the alternative is a page drawn
                -- with another page's sun, which is the defect #1869
                -- exists to remove. No shipped flow reaches this.
                | length (wmVisible mgr) ≥ maxSolarPages →
                    (mgr, ShowAtVisibleLimit)
                | otherwise →
                    -- #1602: the selection generation moves in the SAME
                    -- atomic update as the list it describes, and only on
                    -- the branch that actually changes it — a re-show of an
                    -- already-visible page must not invalidate a live
                    -- placement binding.
                    (bumpSelectionGen
                        (mgr { wmVisible = pageId : wmVisible mgr }), ShowApplied)

    case outcome of
      ShowNoSuchPage → logWarn logger CatWorld $
        "Ignoring world.show for nonexistent world: " <> unWorldPageId pageId
      ShowAtVisibleLimit → logWarn logger CatWorld $
        "Ignoring world.show for " <> unWorldPageId pageId
        <> ": already " <> tshow maxSolarPages
        <> " visible worlds, the most one frame can light individually"
      ShowApplied → do
        mgr ← readIORef (wsWorldManagerRef wsc)
        logDebug logger CatWorld $
            "Visible worlds after show: " <> tshow (length $ wmVisible mgr)

        -- Force a quad-cache rebuild when a world becomes visible: a world
        -- can have been cached while invisible (or before its textures
        -- finished loading) and the render thread only rebuilds when the
        -- generation no longer matches. Cheap insurance against showing a
        -- world with a stale cache (#35).
        let mWorldState = lookup pageId (wmWorlds mgr)
        forM_ mWorldState bumpQuadCacheGen

        -- Activate this world in the sim thread. The sim no longer holds the
        -- tile ref — it emits WorldApplyFluids back to the world thread (the
        -- sole writer of wsTilesRef) — so this is just a per-world "is
        -- active" signal, plus the page's seam topology: activation is what
        -- lets the world tick, so its neighbour frame is established in the
        -- same message (#2044).
        topo ← maybe (pure SimFlatTopology) pageSimTopology mWorldState
        Q.writeQueue (wsSimQueue wsc) (SimActivateWorld pageId topo)

handleWorldHideCommand ∷ WorldSimCapability → LoggerState → WorldPageId → IO ()
handleWorldHideCommand wsc logger pageId = do
    logDebug logger CatWorld $ "Hiding world: " <> unWorldPageId pageId

    -- Only deactivate sim for a world that was actually visible. Hiding an
    -- invalid / already-hidden page is a no-op for sim state, and hiding one
    -- world never tears down the others' sim (per-world deactivate, #55).
    wasVisible ← atomicModifyIORef' (wsWorldManagerRef wsc) $ \mgr' →
      let mgr = completeSelectionChange mgr' in
        -- #1602: as in show above — the GENERATION moves only when the
        -- visible HEAD changes, which is the only page a placement
        -- binding can name. Hiding an already-hidden page, or a visible
        -- one that is not the head, is a true no-op for live bindings.
        -- The PENDING count is discharged either way: it tracks
        -- requests, not effects.
        ( (if selectionHead (wmVisible mgr) ≡ Just pageId
             then bumpSelectionGen else id)
            (mgr { wmVisible = filter (≢ pageId) (wmVisible mgr) })
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
    --
    -- Also clear the one-shot ARM flags (zoomSelectNow/worldSelectNow).
    -- Chunk/tile selection is two-step: setZoomCursorSelect /
    -- setWorldCursorSelect only set the *Now flag, and the render thread
    -- commits it into zoomSelectedPos/worldSelectedTile at draw time
    -- (makeCursorQuad / Render.Quads). A hide that lands after the arm but
    -- before the commit would leave the flag set, so the first render after
    -- re-show would re-commit the selection — re-stranding the HUD exactly
    -- as above. Clearing the flags too matches what the existing
    -- WorldSetZoomCursorDeselect / WorldSetWorldCursorDeselect handlers do
    -- (both reset position AND *Now), and is unambiguously correct here: a
    -- hidden world has no pending selection to commit.
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { selectedGroundItem = Nothing
                    , zoomSelectedPos    = Nothing
                    , zoomSelectNow      = False
                    , worldSelectedTile  = Nothing
                    , worldSelectNow     = False }, ())
        Nothing → pure ()

    when wasVisible $
        Q.writeQueue (wsSimQueue wsc) (SimDeactivateWorld pageId)

handleWorldSetMapModeCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → ZoomMapMode → IO ()
handleWorldSetMapModeCommand wsc logger pageId mode = do
    logDebug logger CatWorld $
        "Setting map mode for world: " <> unWorldPageId pageId
        <> " to " <> tshow mode
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            writeIORef (wsMapModeRef worldState) mode
            logInfo logger CatWorld $
                "Map mode updated for world: " <> unWorldPageId pageId
                <> ", new mode: " <> tshow mode
        Nothing →
            logDebug logger CatWorld $
                "World not found for map mode update: " <> unWorldPageId pageId

handleWorldSetToolModeCommand ∷ WorldSimCapability → LoggerState → WorldPageId → ToolMode → IO ()
handleWorldSetToolModeCommand wsc logger pagedId mode = do
        mgr ← readIORef (wsWorldManagerRef wsc)
        case lookup pagedId (wmWorlds mgr) of
            Just worldState → do
                writeIORef (wsToolModeRef worldState) mode
                logInfo logger CatWorld $
                    "Tool mode updated for world: " <> unWorldPageId pagedId
                    <> ", new mode: " <> tshow mode
            Nothing →
                logDebug logger CatWorld $
                    "World not found for tool mode update: " <> unWorldPageId pagedId
