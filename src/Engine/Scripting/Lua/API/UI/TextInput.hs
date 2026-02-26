{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua bindings for the per-element text buffer: enabling text input,
--   reading/writing content, and cursor movement / editing.
module Engine.Scripting.Lua.API.UI.TextInput
  ( uiEnableTextInputFn
  , uiGetTextFn
  , uiSetTextInputFn
  , uiGetCursorFn
  , uiSetCursorFn
  , uiInsertCharFn
  , uiDeleteBackwardFn
  , uiDeleteForwardFn
  , uiCursorLeftFn
  , uiCursorRightFn
  , uiCursorHomeFn
  , uiCursorEndFn
  ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Core.State (EngineEnv(..))
import UI.Types
import UI.Manager
import qualified UI.TextBuffer as TB

-----------------------------------------------------------
-- Text Buffer Operations
-----------------------------------------------------------

-- | UI.enableTextInput(elementHandle) - Enable text input on an element
uiEnableTextInputFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiEnableTextInputFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (enableTextInput (ElementHandle $ fromIntegral e) mgr, ())
        Nothing → pure ()
    return 0

-- | UI.getText(elementHandle) -> string or nil
uiGetTextFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiGetTextFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case getTextBuffer (ElementHandle $ fromIntegral e) mgr of
                Just buf → Lua.pushstring (TE.encodeUtf8 $ tbContent buf)
                Nothing  → Lua.pushnil
        Nothing → Lua.pushnil
    return 1

-- | UI.setTextInput(elementHandle, text) - Set the text content
uiSetTextInputFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetTextInputFn env = do
    elemArg ← Lua.tointeger 1
    textArg ← Lua.tostring  2
    case (elemArg, textArg) of
        (Just e, Just txtBS) → do
            let txt        = TE.decodeUtf8 txtBS
                elemHandle = ElementHandle (fromIntegral e)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let newBuffer = TextBuffer { tbContent = txt, tbCursor = T.length txt }
                in (setTextBuffer elemHandle newBuffer mgr, ())
        _ → pure ()
    return 0

-- | UI.getCursor(elementHandle) -> int or nil
uiGetCursorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiGetCursorFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case getTextBuffer (ElementHandle $ fromIntegral e) mgr of
                Just buf → Lua.pushinteger (fromIntegral $ tbCursor buf)
                Nothing  → Lua.pushnil
        Nothing → Lua.pushnil
    return 1

-- | UI.setCursor(elementHandle, position)
uiSetCursorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetCursorFn env = do
    elemArg ← Lua.tointeger 1
    posArg  ← Lua.tointeger 2
    case (elemArg, posArg) of
        (Just e, Just pos) → do
            let elemHandle = ElementHandle (fromIntegral e)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (modifyTextBuffer elemHandle (\buf →
                    buf { tbCursor = max 0 (min (T.length $ tbContent buf) (fromIntegral pos)) }
                ) mgr, ())
        _ → pure ()
    return 0

-- | UI.insertChar(elementHandle, char) - Insert character at cursor
uiInsertCharFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiInsertCharFn env = do
    elemArg ← Lua.tointeger 1
    charArg ← Lua.tostring  2
    case (elemArg, charArg) of
        (Just e, Just charBS) → do
            let elemHandle = ElementHandle (fromIntegral e)
                charText   = TE.decodeUtf8 charBS
            case T.uncons charText of
                Just (c, _) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                    (modifyTextBuffer elemHandle (TB.insertChar c) mgr, ())
                Nothing → pure ()
        _ → pure ()
    return 0

-- | UI.deleteBackward(elementHandle) - Delete character before cursor (Backspace)
uiDeleteBackwardFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiDeleteBackwardFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.deleteBackward mgr, ())
        Nothing → pure ()
    return 0

-- | UI.deleteForward(elementHandle) - Delete character at cursor (Delete)
uiDeleteForwardFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiDeleteForwardFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.deleteForward mgr, ())
        Nothing → pure ()
    return 0

-- | UI.cursorLeft(elementHandle)
uiCursorLeftFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiCursorLeftFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.cursorLeft mgr, ())
        Nothing → pure ()
    return 0

-- | UI.cursorRight(elementHandle)
uiCursorRightFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiCursorRightFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.cursorRight mgr, ())
        Nothing → pure ()
    return 0

-- | UI.cursorHome(elementHandle)
uiCursorHomeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiCursorHomeFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.cursorHome mgr, ())
        Nothing → pure ()
    return 0

-- | UI.cursorEnd(elementHandle)
uiCursorEndFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiCursorEndFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.cursorEnd mgr, ())
        Nothing → pure ()
    return 0
