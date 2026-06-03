module Engine.Scripting.Lua.Script
  ( loadScriptAsModule
  , callModuleFunction
  , callLuaFunction
  ) where

import UPrelude
import Engine.Scripting.Types (ScriptValue(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Core.Log (logWarn, LogCategory(..))
import Data.IORef (readIORef)
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad (forM_)

-- | Load a Lua script and return a reference to its module table
loadScriptAsModule ∷ Lua.State → FilePath → IO (Maybe Lua.Reference)
loadScriptAsModule lst path = Lua.runWith lst $ do
    status ← Lua.dofileTrace (Just path)
    case status of
        Lua.OK → do
            isTable ← Lua.istable (-1)
            if isTable
                then do
                    ref ← Lua.ref Lua.registryindex
                    return (Just ref)
                else do
                    Lua.pop 1
                    return Nothing
        _ → return Nothing

-- | Call a function on a module table
-- | Call a module function under 'pcall' so a Lua error in a callback
--   does NOT throw a Haskell 'Lua.Exception' (which would propagate to
--   the Lua-thread crash handler and shut the whole engine down). Errors
--   are logged (they carry file:line) and the offending callback is
--   skipped; the engine keeps running.
callModuleFunction ∷ LuaBackendState → Lua.Reference → T.Text → [ScriptValue] → IO ()
callModuleFunction ls modRef funcName args = do
    mErr ← Lua.runWith (lbsLuaState ls) $ do
        _ ← Lua.getref Lua.registryindex modRef ∷ Lua.LuaE Lua.Exception Lua.Type
        _ ← Lua.getfield (-1) (Lua.Name $ TE.encodeUtf8 funcName)
        isFunc ← Lua.isfunction (-1)
        if isFunc
            then do
                forM_ args pushScriptValue
                status ← Lua.pcall (fromIntegral $ length args) 0 Nothing
                case status of
                    Lua.OK → do
                        Lua.pop 1                      -- pop module table
                        return Nothing
                    _ → do
                        err ← Lua.tostring (-1)
                        Lua.pop 2                      -- error object + module table
                        return (Just (maybe "unknown error" TE.decodeUtf8 err))
            else do
                Lua.pop 2
                return Nothing
    case mErr of
        Nothing  → pure ()
        Just msg → do
            logger ← readIORef (lbsLoggerRef ls)
            logWarn logger CatLua $ "Lua error in " <> funcName <> "(): " <> msg

-- | Call a global Lua function
callLuaFunction ∷ T.Text → [ScriptValue] → Lua.LuaE Lua.Exception Lua.Status
callLuaFunction funcName args = do
  let name = Lua.Name (TE.encodeUtf8 funcName)
  _ ← Lua.getglobal name
  forM_ args pushScriptValue
  let numArgs = fromIntegral (length args)
  -- pcall so a Lua error returns a non-OK Status instead of throwing.
  Lua.pcall numArgs Lua.multret Nothing

-- | Push one 'ScriptValue' onto the Lua stack. 'ScriptTable' pushes
--   a real Lua table built from the key/value list — without this,
--   broadcasts couldn't pass list/record data to Lua callbacks.
--   'ScriptFunction' is still nil since 'Dynamic' has no Lua
--   representation.
--
--   Whole-number 'ScriptNumber' values within Int range are pushed
--   as Lua integers rather than floats so they work cleanly as
--   array indices (ipairs is sensitive to integer subtype on some
--   Lua builds). Fractional values fall back to 'pushnumber'.
pushScriptValue ∷ ScriptValue → Lua.LuaE Lua.Exception ()
pushScriptValue v = case v of
    ScriptNumber n → pushNumeric n
    ScriptString s → Lua.pushstring (TE.encodeUtf8 s)
    ScriptBool   b → Lua.pushboolean b
    ScriptNil      → Lua.pushnil
    ScriptTable kvs → do
        Lua.newtable
        forM_ kvs $ \(k, val) → do
            pushScriptValue k
            pushScriptValue val
            Lua.rawset (-3)
    ScriptFunction _ → Lua.pushnil

-- | Push a 'Double' as a Lua integer when it represents a
--   round-trippable whole number, otherwise as a Lua float.
pushNumeric ∷ Double → Lua.LuaE Lua.Exception ()
pushNumeric n
    | isNaN n || isInfinite n = Lua.pushnumber (Lua.Number n)
    | n == fromIntegral asInt = Lua.pushinteger (fromIntegral asInt)
    | otherwise               = Lua.pushnumber (Lua.Number n)
  where
    asInt = round n ∷ Int
