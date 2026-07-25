{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.World.Cursor
    ( worldSetZoomCursorHoverFn
    , worldSetZoomCursorSelectFn
    , worldClearZoomCursorSelectFn
    , worldSetZoomCursorSelectTextureFn
    , worldSetZoomCursorHoverTextureFn
    , worldSetWorldCursorSelectTextureFn
    , worldSetWorldCursorHoverTextureFn
    , worldSetWorldCursorHoverFn
    , worldSetWorldCursorSelectFn
    , worldClearWorldCursorSelectFn
    , worldSelectTileFn
    , worldGetSelectedTileFn
    , worldSelectChunkFn
    , worldSetWorldCursorSelectBgTextureFn
    , worldSetWorldCursorHoverBgTextureFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Asset.Handle (TextureHandle(..))
import World.Types

-- | world.setZoomCursorHover(pageId, x, y)
worldSetZoomCursorHoverFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorHoverFn wsc = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3

    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetZoomCursorHover pageId (round x) (round y)
        _ → pure ()
    return 0

-- | world.setZoomCursorSelect(pageId, x, y)
worldSetZoomCursorSelectFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorSelectFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $ WorldSetZoomCursorSelect pageId
        _ → pure ()
    return 0

-- | world.clearZoomCursorDeselect(pageId)
worldClearZoomCursorSelectFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldClearZoomCursorSelectFn wsc = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetZoomCursorDeselect pageId
        _ → pure ()
    return 0

worldSetZoomCursorSelectTextureFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorSelectTextureFn wsc = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2

    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetZoomCursorSelectTexture pageId texHandle
        _ → pure ()
    return 0

worldSetZoomCursorHoverTextureFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorHoverTextureFn wsc = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2

    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetZoomCursorHoverTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorHoverFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverFn wsc = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetWorldCursorHover pageId (round x) (round y)
        _ → pure ()
    return 0

worldSetWorldCursorSelectFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $ WorldSetWorldCursorSelect pageId
        _ → pure ()
    return 0

worldClearWorldCursorSelectFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldClearWorldCursorSelectFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $ WorldSetWorldCursorDeselect pageId
        _ → pure ()
    return 0

-- | world.selectTile(pageId, gx, gy[, z]) — atomically mark the column
--   at (gx, gy) as the world's selected tile. With @z@ (the live-picked
--   z from @world.pickTile@) the exact clicked tile is selected — below
--   the surface that is NOT the column top (issue #367). Both live UI
--   callers (the Info-tool left-click in scripts/hud.lua and the
--   right-click → Info context-menu callback in
--   scripts/init_context_menu.lua) resolve a live pick and always pass
--   its z; omitting @z@ falls back to the chunk's surface z, a latent
--   API affordance no current UI path exercises. Unlike
--   setWorldCursorSelect (which races with per-tick mouse hover
--   updates), this is direct: a one-shot selection that doesn't touch
--   the cursor position.
worldSelectTileFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSelectTileFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    zArg      ← Lua.tonumber 4
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                mz     = round ⊚ zArg
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSelectTileByCoord pageId (round gx) (round gy) mz
        _ → pure ()
    return 0

-- | world.getSelectedTile(pageId) -> {gx,gy,z} | nil -- the tile
--   'worldSelectTileFn' (or ordinary click-selection) last committed
--   into 'worldSelectedTile'. Reads 'wsCursorRef' directly (synchronous;
--   mirrors 'worldGetMineDesignationCountFn''s exact page-lookup
--   pattern) -- headless diagnostics for the persistence contract's
--   reset-policy check (#767, requirement 6: tile selection is
--   transient/Excluded state, never persisted, so it must read back
--   empty after any load).
worldGetSelectedTileFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetSelectedTileFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    cs ← Lua.liftIO $ readIORef (wsCursorRef ws)
                    case worldSelectedTile cs of
                        Just (gx, gy, z) → do
                            Lua.newtable
                            Lua.pushinteger (fromIntegral gx)
                            Lua.setfield (-2) "gx"
                            Lua.pushinteger (fromIntegral gy)
                            Lua.setfield (-2) "gy"
                            Lua.pushinteger (fromIntegral z)
                            Lua.setfield (-2) "z"
                            return 1
                        Nothing → do
                            Lua.pushnil
                            return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

worldSetWorldCursorSelectTextureFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectTextureFn wsc = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetWorldCursorSelectTexture pageId texHandle
        _ → pure ()
    return 0

-- | world.selectChunk(pageId, gx, gy) — atomically mark the chunk whose
--   chunk-aligned grid origin is (gx, gy) as the zoom map's selected
--   chunk, dropping any zoomed-in tile selection in the same write
--   (issue #135). Pair with @world.pickChunk@, the zoomed-out analog of
--   @world.pickTile@ + @world.selectTile@: a zoom-map click resolves the
--   chunk under the click NOW and commits it in one shot, instead of
--   arming @setZoomCursorSelect@'s deferred render-time resolve — so a
--   later hover update, camera pan/zoom, or render timing can't retarget
--   an already-accepted click (issue #813).
worldSelectChunkFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSelectChunkFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSelectChunkByCoord pageId (round gx) (round gy)
        _ → pure ()
    return 0

worldSetWorldCursorHoverTextureFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverTextureFn wsc = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetWorldCursorHoverTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorHoverBgTextureFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverBgTextureFn wsc = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetWorldCursorHoverBgTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorSelectBgTextureFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectBgTextureFn wsc = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetWorldCursorSelectBgTexture pageId texHandle
        _ → pure ()
    return 0
