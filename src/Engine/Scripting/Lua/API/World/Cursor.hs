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
    , worldSetWorldCursorSelectBgTextureFn
    , worldSetWorldCursorHoverBgTextureFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import World.Types

-- | world.setZoomCursorHover(pageId, x, y)
worldSetZoomCursorHoverFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorHoverFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3

    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorHover pageId (round x) (round y)
        _ → pure ()
    return 0

-- | world.setZoomCursorSelect(pageId, x, y)
worldSetZoomCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldSetZoomCursorSelect pageId
        _ → pure ()
    return 0

-- | world.clearZoomCursorDeselect(pageId)
worldClearZoomCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldClearZoomCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorDeselect pageId
        _ → pure ()
    return 0

worldSetZoomCursorSelectTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorSelectTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2

    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorSelectTexture pageId texHandle
        _ → pure ()
    return 0

worldSetZoomCursorHoverTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorHoverTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2

    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorHoverTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorHoverFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorHover pageId (round x) (round y)
        _ → pure ()
    return 0

worldSetWorldCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldSetWorldCursorSelect pageId
        _ → pure ()
    return 0

worldClearWorldCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldClearWorldCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldSetWorldCursorDeselect pageId
        _ → pure ()
    return 0

-- | world.selectTile(pageId, gx, gy[, z]) — atomically mark the column
--   at (gx, gy) as the world's selected tile. With @z@ (the live-picked
--   z from @world.pickTile@) the exact clicked tile is selected — below
--   the surface that is NOT the column top (issue #367). Omit @z@ to use
--   the chunk's surface z (the right-click → Info context-menu path,
--   which has no live pick). Unlike setWorldCursorSelect (which races
--   with per-tick mouse hover updates), this is direct: a one-shot
--   selection that doesn't touch the cursor position.
worldSelectTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSelectTileFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    zArg      ← Lua.tonumber 4
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                mz     = round ⊚ zArg
            Q.writeQueue (worldQueue env) $
                WorldSelectTileByCoord pageId (round gx) (round gy) mz
        _ → pure ()
    return 0

worldSetWorldCursorSelectTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorSelectTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorHoverTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorHoverTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorHoverBgTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverBgTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorHoverBgTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorSelectBgTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectBgTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorSelectBgTexture pageId texHandle
        _ → pure ()
    return 0
