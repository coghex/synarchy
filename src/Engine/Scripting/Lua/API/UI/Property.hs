{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua bindings for reading and writing element properties:
--   position, size, visibility, clickability, colour, z-index,
--   text content, and texture assignment.
module Engine.Scripting.Lua.API.UI.Property
  ( uiSetPositionFn
  , uiSetSizeFn
  , uiSetVisibleFn
  , uiIsPageVisibleFn
  , uiSetClickableFn
  , uiSetOnClickFn
  , uiSetOnRightClickFn
  , uiSetZIndexFn
  , uiSetColorFn
  , uiSetTextFn
  , uiSetSpriteTextureFn
  , uiSetBoxTexturesFn
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import UI.Types
import UI.Manager

-----------------------------------------------------------
-- Properties
-----------------------------------------------------------

-- | UI.setPosition(elementHandle, x, y)
uiSetPositionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetPositionFn env = do
    elemArg ← Lua.tointeger 1
    xArg    ← Lua.tonumber  2
    yArg    ← Lua.tonumber  3

    case (elemArg, xArg, yArg) of
        (Just e, Just x, Just y) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementPosition (ElementHandle $ fromIntegral e) (realToFrac x) (realToFrac y) mgr, ())
        _ → pure ()

    return 0

-- | UI.setSize(elementHandle, width, height)
uiSetSizeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetSizeFn env = do
    elemArg ← Lua.tointeger 1
    wArg    ← Lua.tonumber  2
    hArg    ← Lua.tonumber  3

    case (elemArg, wArg, hArg) of
        (Just e, Just w, Just h) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementSize (ElementHandle $ fromIntegral e) (realToFrac w) (realToFrac h) mgr, ())
        _ → pure ()

    return 0

-- | UI.setVisible(elementHandle, visible)
uiSetVisibleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetVisibleFn env = do
    elemArg ← Lua.tointeger 1
    visArg  ← Lua.toboolean 2

    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementVisible (ElementHandle $ fromIntegral e) visArg mgr, ())
        Nothing → pure ()

    return 0

-- | UI.isPageVisible(pageHandle) -> boolean
uiIsPageVisibleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiIsPageVisibleFn env = do
    handleArg ← Lua.tointeger 1
    case handleArg of
        Just n → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case getPage (PageHandle $ fromIntegral n) mgr of
                Just page → Lua.pushboolean (upVisible page)
                Nothing   → Lua.pushboolean False
        Nothing → Lua.pushboolean False
    return 1

-- | UI.setClickable(elementHandle, clickable)
uiSetClickableFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetClickableFn env = do
    elemArg  ← Lua.tointeger 1
    clickArg ← Lua.toboolean 2

    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementClickable (ElementHandle $ fromIntegral e) clickArg mgr, ())
        Nothing → pure ()

    return 0

-- | UI.setOnClick(elementHandle, callbackName)
uiSetOnClickFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetOnClickFn env = do
    elemArg     ← Lua.tointeger 1
    callbackArg ← Lua.tostring  2

    case (elemArg, callbackArg) of
        (Just e, Just cbBS) → do
            let callback = TE.decodeUtf8 cbBS
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (setElementOnClick (ElementHandle $ fromIntegral e) callback mgr, ())
        _ → pure ()

    return 0

-- | UI.setOnRightClick(elementHandle, callbackName)
uiSetOnRightClickFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetOnRightClickFn env = do
    elemArg     ← Lua.tointeger 1
    callbackArg ← Lua.tostring  2

    case (elemArg, callbackArg) of
        (Just e, Just cbBS) → do
            let callback = TE.decodeUtf8 cbBS
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (setElementOnRightClick (ElementHandle $ fromIntegral e) callback mgr, ())
        _ → pure ()

    return 0

-- | UI.setZIndex(elementHandle, z)
uiSetZIndexFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetZIndexFn env = do
    elemArg ← Lua.tointeger 1
    zArg    ← Lua.tointeger 2

    case (elemArg, zArg) of
        (Just e, Just z) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementZIndex (ElementHandle $ fromIntegral e) (fromIntegral z) mgr, ())
        _ → pure ()

    return 0

-- | UI.setColor(elementHandle, r, g, b, a)
--
--   Dispatches to the correct colour setter based on the element's
--   render-data variant (box, sprite, or text).
uiSetColorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetColorFn env = do
    elemArg ← Lua.tointeger 1
    rArg    ← Lua.tonumber  2
    gArg    ← Lua.tonumber  3
    bArg    ← Lua.tonumber  4
    aArg    ← Lua.tonumber  5

    case (elemArg, rArg, gArg, bArg, aArg) of
        (Just e, Just r, Just g, Just b, Just a) → do
            let elemHandle = ElementHandle (fromIntegral e)
                color      = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                case Map.lookup elemHandle (upmElements mgr) of
                    Nothing   → (mgr, ())
                    Just elem → case ueRenderData elem of
                        RenderBox    _ → (setBoxColor    elemHandle color mgr, ())
                        RenderSprite _ → (setSpriteColor elemHandle color mgr, ())
                        RenderText   _ → (setTextColor   elemHandle color mgr, ())
                        RenderNone     → (mgr, ())
        _ → pure ()

    return 0

-- | UI.setText(elementHandle, text)
uiSetTextFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetTextFn env = do
    elemArg ← Lua.tointeger 1
    textArg ← Lua.tostring  2

    case (elemArg, textArg) of
        (Just e, Just txtBS) → do
            let elemHandle = ElementHandle (fromIntegral e)
                text       = TE.decodeUtf8 txtBS
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (setText elemHandle text mgr, ())
        _ → pure ()

    return 0

-- | UI.setSpriteTexture(elementHandle, textureHandle)
uiSetSpriteTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetSpriteTextureFn env = do
    elemArg ← Lua.tointeger 1
    texArg  ← Lua.tointeger 2

    case (elemArg, texArg) of
        (Just e, Just t) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setSpriteTexture (ElementHandle $ fromIntegral e) (TextureHandle $ fromIntegral t) mgr, ())
        _ → pure ()

    return 0

-- | UI.setBoxTextures(elementHandle, boxTextureHandle)
uiSetBoxTexturesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetBoxTexturesFn env = do
    elemArg ← Lua.tointeger 1
    texArg  ← Lua.tointeger 2

    case (elemArg, texArg) of
        (Just e, Just t) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setBoxTextures (ElementHandle $ fromIntegral e) (BoxTextureHandle $ fromIntegral t) mgr, ())
        _ → pure ()

    return 0
