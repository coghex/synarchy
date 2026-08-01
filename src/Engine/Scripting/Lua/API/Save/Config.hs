{-# LANGUAGE Strict #-}
-- | Lua-stack marshalling for the autosave 'SaveConfig' verbs
--   (@engine.getSaveConfig@ / @getDefaultSaveConfig@ / @setSaveConfig@,
--   issue #913). Split out of "Engine.Scripting.Lua.API.Save" by issue
--   #985: nothing here takes an @EngineEnv@, so this module stays
--   outside the save\/load path's permanent full-access exception
--   (@docs\/engineenv_capability_inventory.md@ §6.1).
module Engine.Scripting.Lua.API.Save.Config
    ( pushSaveConfig
    , optBooleanField
    , optIntegerField
    ) where

import UPrelude
import qualified HsLua as Lua
import Engine.Save.Config (SaveConfig(..))

-- | Push the {enabled=, intervalMinutes=, rotationDepth=} table every
--   save-config getter returns.
pushSaveConfig ∷ SaveConfig → Lua.LuaE Lua.Exception ()
pushSaveConfig cfg = do
    Lua.newtable
    Lua.pushboolean (scEnabled cfg)
    Lua.setfield (-2) "enabled"
    Lua.pushinteger (fromIntegral (scIntervalMinutes cfg))
    Lua.setfield (-2) "intervalMinutes"
    Lua.pushinteger (fromIntegral (scRotationDepth cfg))
    Lua.setfield (-2) "rotationDepth"

-- | Read an optional boolean field, so @setSaveConfig@'s table can be a
--   PATCH: a key that is absent (or not a boolean) reports 'Nothing' and
--   leaves the current effective value alone.
optBooleanField ∷ Lua.StackIndex → Lua.Name
                → Lua.LuaE Lua.Exception (Maybe Bool)
optBooleanField idx key = do
    ty ← Lua.getfield idx key
    v ← if ty ≡ Lua.TypeBoolean then Just ⊚ Lua.toboolean (-1)
                                else pure Nothing
    Lua.pop 1
    pure v

-- | The integer counterpart of 'optBooleanField', with the same
--   absent-key-keeps-the-current-value contract.
optIntegerField ∷ Lua.StackIndex → Lua.Name
                → Lua.LuaE Lua.Exception (Maybe Int)
optIntegerField idx key = do
    _ ← Lua.getfield idx key
    v ← Lua.tointeger (-1)
    Lua.pop 1
    pure (fromIntegral ⊚ v)
