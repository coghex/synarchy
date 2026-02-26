{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua bindings for creating UI elements: generic elements, boxes,
--   text labels, sprites, and box-texture registration.
module Engine.Scripting.Lua.API.UI.Element
  ( uiNewElementFn
  , uiNewBoxFn
  , uiNewTextFn
  , uiNewSpriteFn
  , uiLoadBoxTexturesFn
  ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..), FontHandle(..))
import UI.Types
import UI.Manager

-----------------------------------------------------------
-- Element Creation
-----------------------------------------------------------

-- | UI.newElement(name, width, height, pageHandle) -> elementHandle
uiNewElementFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiNewElementFn env = do
    nameArg   ← Lua.tostring  1
    widthArg  ← Lua.tonumber  2
    heightArg ← Lua.tonumber  3
    pageArg   ← Lua.tointeger 4

    case (nameArg, widthArg, heightArg, pageArg) of
        (Just nameBS, Just w, Just h, Just p) → do
            let name       = TE.decodeUtf8 nameBS
                pageHandle = PageHandle (fromIntegral p)

            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (elemH, newMgr) = createElement name (realToFrac w) (realToFrac h) pageHandle mgr
                in (newMgr, elemH)

            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ → Lua.pushnil

    return 1

-- | UI.newBox(name, width, height, boxTexHandle, tileSize, r, g, b, a, overflow, pageHandle) -> elementHandle
uiNewBoxFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiNewBoxFn env = do
    nameArg     ← Lua.tostring  1
    widthArg    ← Lua.tonumber  2
    heightArg   ← Lua.tonumber  3
    boxTexArg   ← Lua.tointeger 4
    tileSizeArg ← Lua.tonumber  5
    rArg        ← Lua.tonumber  6
    gArg        ← Lua.tonumber  7
    bArg        ← Lua.tonumber  8
    aArg        ← Lua.tonumber  9
    overflowArg ← Lua.tonumber  10
    pageArg     ← Lua.tointeger 11

    case (nameArg, widthArg, heightArg, boxTexArg, tileSizeArg, rArg, gArg, bArg, aArg, overflowArg, pageArg) of
        (Just nameBS, Just w, Just h, Just bt, Just ts, Just r, Just g, Just b, Just a, Just ovf, Just p) → do
            let name         = TE.decodeUtf8 nameBS
                boxTexHandle = BoxTextureHandle (fromIntegral bt)
                tileSize     = realToFrac ts
                color        = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                pageHandle   = PageHandle (fromIntegral p)
                overflow     = realToFrac ovf

            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (elemH, newMgr) = createBox name (realToFrac w) (realToFrac h)
                                        boxTexHandle tileSize color overflow pageHandle mgr
                in (newMgr, elemH)

            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ → Lua.pushnil

    return 1

-- | UI.loadBoxTextures(texCenter, texN, texS, texE, texW, texNE, texNW, texSE, texSW) -> boxTextureHandle
uiLoadBoxTexturesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiLoadBoxTexturesFn env = do
    centerArg ← Lua.tointeger 1
    nArg      ← Lua.tointeger 2
    sArg      ← Lua.tointeger 3
    eArg      ← Lua.tointeger 4
    wArg      ← Lua.tointeger 5
    neArg     ← Lua.tointeger 6
    nwArg     ← Lua.tointeger 7
    seArg     ← Lua.tointeger 8
    swArg     ← Lua.tointeger 9

    case (centerArg, nArg, sArg, eArg, wArg, neArg, nwArg, seArg, swArg) of
        (Just c, Just n, Just s, Just e, Just w, Just ne, Just nw, Just se, Just sw) → do
            let texSet = BoxTextureSet
                    { btsCenter = TextureHandle (fromIntegral c)
                    , btsN      = TextureHandle (fromIntegral n)
                    , btsS      = TextureHandle (fromIntegral s)
                    , btsE      = TextureHandle (fromIntegral e)
                    , btsW      = TextureHandle (fromIntegral w)
                    , btsNE     = TextureHandle (fromIntegral ne)
                    , btsNW     = TextureHandle (fromIntegral nw)
                    , btsSE     = TextureHandle (fromIntegral se)
                    , btsSW     = TextureHandle (fromIntegral sw)
                    }

            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (h, newMgr) = registerBoxTextures texSet mgr
                in (newMgr, h)

            Lua.pushinteger (fromIntegral $ unBoxTextureHandle handle)
        _ → Lua.pushnil

    return 1

-- | UI.newText(name, text, fontHandle, size, r, g, b, a, pageHandle) -> elementHandle
uiNewTextFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiNewTextFn env = do
    nameArg ← Lua.tostring  1
    textArg ← Lua.tostring  2
    fontArg ← Lua.tointeger 3
    sizeArg ← Lua.tonumber  4
    rArg    ← Lua.tonumber  5
    gArg    ← Lua.tonumber  6
    bArg    ← Lua.tonumber  7
    aArg    ← Lua.tonumber  8
    pageArg ← Lua.tointeger 9

    case (nameArg, textArg, fontArg, sizeArg, rArg, gArg, bArg, aArg, pageArg) of
        (Just nameBS, Just txtBS, Just f, Just s, Just r, Just g, Just b, Just a, Just p) → do
            let name       = TE.decodeUtf8 nameBS
                text       = TE.decodeUtf8 txtBS
                fontHandle = FontHandle (fromIntegral f)
                color      = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                pageHandle = PageHandle (fromIntegral p)
                size       = realToFrac s

            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (elemH, newMgr) = createText name text fontHandle size color pageHandle mgr
                in (newMgr, elemH)

            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ → Lua.pushnil

    return 1

-- | UI.newSprite(name, width, height, textureHandle, r, g, b, a, pageHandle) -> elementHandle
uiNewSpriteFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiNewSpriteFn env = do
    nameArg   ← Lua.tostring  1
    widthArg  ← Lua.tonumber  2
    heightArg ← Lua.tonumber  3
    texArg    ← Lua.tointeger 4
    rArg      ← Lua.tonumber  5
    gArg      ← Lua.tonumber  6
    bArg      ← Lua.tonumber  7
    aArg      ← Lua.tonumber  8
    pageArg   ← Lua.tointeger 9

    case (nameArg, widthArg, heightArg, texArg, rArg, gArg, bArg, aArg, pageArg) of
        (Just nameBS, Just w, Just h, Just t, Just r, Just g, Just b, Just a, Just p) → do
            let name      = TE.decodeUtf8 nameBS
                texHandle = TextureHandle (fromIntegral t)
                color     = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                pageHandle = PageHandle (fromIntegral p)

            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (elemH, newMgr) = createSprite name (realToFrac w) (realToFrac h) texHandle color pageHandle mgr
                in (newMgr, elemH)

            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ → Lua.pushnil

    return 1
