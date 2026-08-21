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
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.Log (logWarn, LogCategory(..), LoggerState)
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Generate (globalToChunk)

handleWorldSetZoomCursorHoverCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetZoomCursorHoverCommand wsc logger pageId x y = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { zoomCursorPos = Just (x, y) }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor hover update: " <> unWorldPageId pageId
handleWorldSetZoomCursorSelectCommand ∷ WorldSimCapability → LoggerState → WorldPageId → IO ()
handleWorldSetZoomCursorSelectCommand wsc _logger pageId = do
    mgr ← readIORef (wsWorldManagerRef wsc)
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
handleWorldSetZoomCursorDeselectCommand ∷ WorldSimCapability → LoggerState → WorldPageId → IO ()
handleWorldSetZoomCursorDeselectCommand wsc _logger pageId = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { zoomSelectedPos = Nothing, zoomSelectNow = False }, ())
        Nothing → pure ()
handleWorldSetZoomCursorSelectTextureCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetZoomCursorSelectTextureCommand wsc logger pageId tid = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { zoomCursorTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for zoom cursor texture update: "
                    <> unWorldPageId pageId
handleWorldSetZoomCursorHoverTextureCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetZoomCursorHoverTextureCommand wsc logger pageId tid = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { zoomHoverTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for zoom cursor hover texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorHoverCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetWorldCursorHoverCommand wsc logger pageId x y = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldCursorPos = Just (x, y) }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor hover update: " <> unWorldPageId pageId
handleWorldSetWorldCursorSelectCommand ∷ WorldSimCapability → LoggerState → WorldPageId → IO ()
handleWorldSetWorldCursorSelectCommand wsc _logger pageId = do
    mgr ← readIORef (wsWorldManagerRef wsc)
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
handleWorldSetWorldCursorDeselectCommand ∷ WorldSimCapability → LoggerState → WorldPageId → IO ()
handleWorldSetWorldCursorDeselectCommand wsc _logger pageId = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { worldSelectedTile = Nothing, worldSelectNow = False }, ())
        Nothing → pure ()
handleWorldSetWorldCursorSelectTextureCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorSelectTextureCommand wsc logger pageId tid = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldCursorTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorHoverTextureCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorHoverTextureCommand wsc logger pageId tid = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldHoverTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor hover texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorSelectBgTextureCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorSelectBgTextureCommand wsc logger pageId tid = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldCursorBgTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorHoverBgTextureCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorHoverBgTextureCommand wsc logger pageId tid = do
    mgr ← readIORef (wsWorldManagerRef wsc)
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
--   live-picked z from a left-click or a right-click → Info
--   context-menu selection, so clicking below the surface selects the
--   clicked tile rather than the column top — issue #367); @Nothing@
--   falls back to the loaded chunk's surface z, a latent API
--   affordance no current UI path exercises (both live callers always
--   resolve a pick and pass its z). Used so a tile can be selected
--   without going through the hover-then-select cursor flow (which
--   races with the per-tick mouse-hover updates from hud.update).
--   No-op if the chunk isn't loaded.
handleWorldSelectTileByCoordCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → Int → Int → Maybe Int → IO ()
handleWorldSelectTileByCoordCommand wsc _logger pageId gx gy mz = do
    mgr ← readIORef (wsWorldManagerRef wsc)
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
--   Also clears BOTH zoomSelectNow and worldSelectNow: this direct
--   selection is authoritative and must win outright over any
--   still-pending deferred arm from EITHER world.setZoomCursorSelect
--   or world.setWorldCursorSelect (issue #813 review). Leaving
--   zoomSelectNow True would let a LATER render pass's makeCursorQuad
--   resolve that stale arm against whatever zoomCursorPos is by then
--   and clobber the fresh selection just committed here; leaving
--   worldSelectNow True is just as dangerous from the OTHER side —
--   renderWorldCursorQuads's own per-frame commit
--   (World.Render.CursorQuads) unconditionally clears zoomSelectedPos
--   whenever it resolves a pending worldSelectNow arm (the #135
--   opposing-clear, mirrored the other way), so a lingering tile arm
--   could wipe out this fresh chunk selection on the very next tile
--   render even though nothing about it was ever re-armed.
handleWorldSelectChunkByCoordCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSelectChunkByCoordCommand wsc _logger pageId gx gy = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { zoomSelectedPos   = Just (gx, gy)
                    , zoomSelectNow     = False
                    , worldSelectNow    = False
                    , worldSelectedTile = Nothing }, ())
