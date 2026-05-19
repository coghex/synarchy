module Engine.Scripting.Lua.Script
  ( loadScriptAsModule
  , callModuleFunction
  , callLuaFunction
  ) where

import UPrelude
import Engine.Scripting.Types (ScriptValue(..))
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
callModuleFunction ∷ Lua.State → Lua.Reference → T.Text → [ScriptValue] → IO Lua.Status
callModuleFunction lst modRef funcName args = Lua.runWith lst $ do
    _ ← Lua.getref Lua.registryindex modRef ∷ Lua.LuaE Lua.Exception Lua.Type
    _ ← Lua.getfield (-1) (Lua.Name $ TE.encodeUtf8 funcName)
    isFunc ← Lua.isfunction (-1)
    if isFunc
        then do
            forM_ args pushScriptValue
            Lua.call (fromIntegral $ length args) 0
            Lua.pop 1
            return Lua.OK
        else do
            Lua.pop 2
            return Lua.OK

-- | Call a global Lua function
callLuaFunction ∷ T.Text → [ScriptValue] → Lua.LuaE Lua.Exception Lua.Status
callLuaFunction funcName args = do
  let name = Lua.Name (TE.encodeUtf8 funcName)
  _ ← Lua.getglobal name
  forM_ args pushScriptValue
  let numArgs = fromIntegral (length args)
  Lua.call numArgs Lua.multret
  return Lua.OK

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
