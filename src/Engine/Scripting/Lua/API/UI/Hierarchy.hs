{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua bindings for the element tree: adding/removing elements
--   to pages and parents, hit-testing, and hover-target lookup.
module Engine.Scripting.Lua.API.UI.Hierarchy
  ( uiAddToPageFn
  , uiAddChildFn
  , uiRemoveElementFn
  , uiDeleteElementFn
  , uiRemoveFromPageFn
  , uiFindElementAtFn
  , uiGetElementOnClickFn
  , uiFindHoverTargetFn
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Core.State (EngineEnv(..))
import UI.Types
import UI.Manager

-----------------------------------------------------------
-- Hierarchy
-----------------------------------------------------------

-- | UI.addToPage(pageHandle, elementHandle, x, y)
uiAddToPageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiAddToPageFn env = do
    pageArg ← Lua.tointeger 1
    elemArg ← Lua.tointeger 2
    xArg    ← Lua.tonumber  3
    yArg    ← Lua.tonumber  4

    case (pageArg, elemArg, xArg, yArg) of
        (Just p, Just e, Just x, Just y) → do
            let pageHandle = PageHandle    (fromIntegral p)
                elemHandle = ElementHandle (fromIntegral e)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (addElementToPage pageHandle elemHandle (realToFrac x) (realToFrac y) mgr, ())
        _ → pure ()

    return 0

-- | UI.addChild(parentHandle, childHandle, x, y)
uiAddChildFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiAddChildFn env = do
    parentArg ← Lua.tointeger 1
    childArg  ← Lua.tointeger 2
    xArg      ← Lua.tonumber  3
    yArg      ← Lua.tonumber  4

    case (parentArg, childArg, xArg, yArg) of
        (Just p, Just c, Just x, Just y) → do
            let parentHandle = ElementHandle (fromIntegral p)
                childHandle  = ElementHandle (fromIntegral c)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (addChildElement parentHandle childHandle (realToFrac x) (realToFrac y) mgr, ())
        _ → pure ()

    return 0

-- | UI.removeElement(elementHandle)
uiRemoveElementFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiRemoveElementFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (removeElement (ElementHandle $ fromIntegral e) mgr, ())
        Nothing → pure ()
    return 0

-- | UI.deleteElement(elementHandle)
uiDeleteElementFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiDeleteElementFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (deleteElement (ElementHandle $ fromIntegral e) mgr, ())
        Nothing → pure ()
    return 0

-- | UI.removeFromPage(pageHandle, elementHandle)
uiRemoveFromPageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiRemoveFromPageFn env = do
    pageArg ← Lua.tointeger 1
    elemArg ← Lua.tointeger 2

    case (pageArg, elemArg) of
        (Just p, Just e) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (removeFromPage (PageHandle $ fromIntegral p) (ElementHandle $ fromIntegral e) mgr, ())
        _ → pure ()

    return 0

-- | UI.findElementAt(x, y) -> elementHandle or nil
--
--   Hit-tests all visible pages and returns the top-most element
--   whose bounding box contains the given point.
uiFindElementAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiFindElementAtFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just x, Just y) → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case findElementAt (realToFrac x, realToFrac y) mgr of
                Just (ElementHandle h) → Lua.pushinteger (fromIntegral h)
                Nothing                → Lua.pushnil
        _ → Lua.pushnil
    return 1

-- | UI.getElementOnClick(elementHandle) -> callbackName or nil
uiGetElementOnClickFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiGetElementOnClickFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case Map.lookup (ElementHandle $ fromIntegral e) (upmElements mgr) of
                Just elem → case ueOnClick elem of
                    Just cb → Lua.pushstring (TE.encodeUtf8 cb)
                    Nothing → Lua.pushnil
                Nothing → Lua.pushnil
        Nothing → Lua.pushnil
    return 1

-- | UI.findHoverTarget(x, y) -> elementHandle, callbackName  or  nil, nil
--
--   Finds the element at the given point, then walks up the tree to
--   locate the nearest clickable ancestor and its callback name.
uiFindHoverTargetFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiFindHoverTargetFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just x, Just y) → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case findElementAt (realToFrac x, realToFrac y) mgr of
                Just hitElem → case findClickableAncestor hitElem mgr of
                    Just (ElementHandle h, cb) → do
                        Lua.pushinteger (fromIntegral h)
                        Lua.pushstring  (TE.encodeUtf8 cb)
                    Nothing → do
                        Lua.pushnil
                        Lua.pushnil
                Nothing → do
                    Lua.pushnil
                    Lua.pushnil
        _ → do
            Lua.pushnil
            Lua.pushnil
    return 2
