{-# LANGUAGE UnicodeSyntax #-}

-- | Raw cursor hover/select/texture state (zoom cursor + world cursor)
--   and direct tile-by-coordinate selection. Split out of
--   "World.Thread.Command.Cursor" (issue #564).
module World.Thread.Command.Cursor.Select
    ( handleWorldSetZoomCursorHoverCommand
    , handleWorldSetZoomCursorSelectCommand
    , handleWorldSetZoomCursorDeselectCommand
    , handleWorldSetZoomCursorSelectTextureCommand
    , handleWorldSetZoomCursorHoverTextureCommand
    , handleWorldSetWorldCursorHoverCommand
    , handleWorldSetWorldCursorSelectCommand
    , handleWorldSetWorldCursorDeselectCommand
    , handleWorldSetWorldCursorSelectTextureCommand
    , handleWorldSetWorldCursorHoverTextureCommand
    , handleWorldSetWorldCursorSelectBgTextureCommand
    , handleWorldSetWorldCursorHoverBgTextureCommand
    , handleWorldSelectTileByCoordCommand
    , handleWorldSelectChunkByCoordCommand
    ) where

import UPrelude
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logWarn, LogCategory(..), LoggerState)
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Generate (globalToChunk)
import World.Thread.Helpers (unWorldPageId)

handleWorldSetZoomCursorHoverCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetZoomCursorHoverCommand env logger pageId x y = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { zoomCursorPos = Just (x, y) }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor hover update: " <> unWorldPageId pageId
handleWorldSetZoomCursorSelectCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldSetZoomCursorSelectCommand env _logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            -- Only ARM the selection here. The chunk is resolved from the
            -- cursor hover at render time (makeCursorQuad), which is also
            -- where the opposing tile selection is cleared — doing the
            -- clear here instead would blank the cursor for the frames
            -- before the commit lands (issue #135). NOTE: the zoom-map
            -- left click no longer drives this arm/render-commit path
            -- (issue #813) — it binds to the clicked chunk synchronously
            -- via 'handleWorldSelectChunkByCoordCommand' below instead, so
            -- a later hover update or camera move can't retarget an
            -- already-accepted click. This command remains as the
            -- lower-level "arm from current hover" primitive
            -- @world.setZoomCursorSelect@ still exposes.
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { zoomSelectNow = True }, ())
        Nothing → pure ()
handleWorldSetZoomCursorDeselectCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldSetZoomCursorDeselectCommand env _logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { zoomSelectedPos = Nothing, zoomSelectNow = False }, ())
        Nothing → pure ()
handleWorldSetZoomCursorSelectTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetZoomCursorSelectTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { zoomCursorTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for zoom cursor texture update: "
                    <> unWorldPageId pageId
handleWorldSetZoomCursorHoverTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetZoomCursorHoverTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { zoomHoverTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for zoom cursor hover texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorHoverCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetWorldCursorHoverCommand env logger pageId x y = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldCursorPos = Just (x, y) }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor hover update: " <> unWorldPageId pageId
handleWorldSetWorldCursorSelectCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldSetWorldCursorSelectCommand env _logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            -- Only ARM the selection here. The tile is resolved from the
            -- cursor hover at render time (renderWorldCursorQuads), which
            -- is also where the opposing chunk selection is cleared —
            -- doing the clear here instead would blank the cursor for the
            -- frames before the commit lands (issue #135).
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { worldSelectNow = True }, ())
        Nothing → pure ()
handleWorldSetWorldCursorDeselectCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldSetWorldCursorDeselectCommand env _logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { worldSelectedTile = Nothing, worldSelectNow = False }, ())
        Nothing → pure ()
handleWorldSetWorldCursorSelectTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorSelectTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldCursorTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorHoverTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorHoverTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldHoverTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor hover texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorSelectBgTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorSelectBgTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldCursorBgTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorHoverBgTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorHoverBgTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldHoverBgTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor hover texture update: "
                    <> unWorldPageId pageId

-- | Directly select the column at (gx, gy) on the given world. The
--   @Maybe Int@ picks the z: @Just z@ selects that exact tile (the
--   live-picked z from a left-click, so clicking below the surface
--   selects the clicked tile rather than the column top — issue #367);
--   @Nothing@ falls back to the loaded chunk's surface z (the
--   context-menu "Info" path, which has no live pick). Used so a tile
--   can be selected without going through the hover-then-select cursor
--   flow (which races with the per-tick mouse-hover updates from
--   hud.update). No-op if the chunk isn't loaded.
handleWorldSelectTileByCoordCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Maybe Int → IO ()
handleWorldSelectTileByCoordCommand env _logger pageId gx gy mz = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            let (chunkCoord, (lx, ly)) = globalToChunk gx gy
            case lookupChunk chunkCoord tileData of
                Nothing → pure ()
                Just lc → do
                    -- Use the live-picked z when supplied; otherwise
                    -- default to the column surface.
                    let z = fromMaybe (lcSurfaceMap lc VU.! columnIndex lx ly) mz
                    -- This path resolves the tile immediately (no hover
                    -- round-trip), so the set and the opposing-chunk clear
                    -- happen in the SAME write — no blank window. A new
                    -- tile selection drops any chunk selection (issue #135).
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                        (cs { worldSelectedTile = Just (gx, gy, z)
                            , zoomSelectedPos   = Nothing }, ())

-- | Directly select the chunk whose chunk-aligned grid origin is
--   (gx, gy) on the given world — the coordinates 'world.pickChunk'
--   (backed by 'World.Render.Zoom.Cursor.pixelToChunkOrigin') already
--   resolved against the click pixel, live camera, and this page's own
--   world size. This is the chunk-selection analog of
--   'handleWorldSelectTileByCoordCommand': the set and the opposing
--   tile-selection clear happen in the SAME atomic write, so there is
--   no armed-but-uncommitted window a later hover update, camera
--   pan/zoom, or render pass could resolve differently from what the
--   player actually clicked (issue #813). No-op if the page doesn't
--   exist, so a click for one page can never commit into another
--   page's cursor state.
--
--   Also clears zoomSelectNow: this direct selection is authoritative
--   and must win outright over any still-pending deferred arm from
--   world.setZoomCursorSelect (issue #813 review) — leaving it True
--   would let a LATER render pass's makeCursorQuad resolve that stale
--   arm against whatever zoomCursorPos is by then and clobber the
--   fresh selection just committed here.
handleWorldSelectChunkByCoordCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSelectChunkByCoordCommand env _logger pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { zoomSelectedPos   = Just (gx, gy)
                    , zoomSelectNow     = False
                    , worldSelectedTile = Nothing }, ())
