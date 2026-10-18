-- | The production paused-readiness hook for scenario units (#2710,
--   SCN-10 of epic #2698): 'Scenario.Units.Install.uirReady' run against
--   the Lua state.
--
--   It calls @unit_resources.settleScenarioUnit(uid)@, which runs the
--   per-unit reconciliation the tick itself runs (@reconcileUnit@) once at
--   zero elapsed time with the unit's effects recorded
--   (@scripts/unit_effects.lua@): injuries, failure meters, every pool's
--   death and collapse thresholds, starvation's floors, the strength
--   discount and revival. It answers the death or the last pose that pass
--   would cause. Nothing is advanced, queued or announced.
--
--   It runs Lua directly, so it must be called on the Lua thread (the
--   thread that owns the state), as the session loader's paused
--   readiness step is. A Lua error is caught by @pcall@ and returned as
--   'Left', never thrown.
module Scenario.Units.Ready
    ( luaUnitReadiness
    , settleFunction
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Scenario.Units.Install (Readiness(..))
import Unit.Types (UnitId(..))

-- | The Lua function the hook calls: @require(module)[name]@.
settleFunction ∷ (Text, Text)
settleFunction = ("scripts.unit_resources", "settleScenarioUnit")

luaUnitReadiness ∷ Lua.State → UnitId → IO (Either Text Readiness)
luaUnitReadiness st (UnitId n) = Lua.runWith st settle
  where
  settle ∷ Lua.LuaE Lua.Exception (Either Text Readiness)
  settle = do
    top ← Lua.gettop
    let (modName, fnName) = settleFunction
        finish r = do
            Lua.settop top
            pure r
        failure = do
            err ← Lua.tostring (-1)
            finish (Left (maybe "unknown Lua error" TE.decodeUtf8Lenient err))
    _ ← Lua.getglobal "require"
    Lua.pushstring (TE.encodeUtf8 modName)
    loaded ← Lua.pcall 1 1 Nothing
    if loaded ≢ Lua.OK then failure else do
        _ ← Lua.getfield (-1) (Lua.Name (TE.encodeUtf8 fnName))
        isFn ← Lua.isfunction (-1)
        if not isFn
            then finish (Left (modName <> "." <> fnName <> " is not a function"))
            else do
                Lua.pushinteger (fromIntegral n)
                called ← Lua.pcall 1 2 Nothing
                if called ≢ Lua.OK then failure else do
                    answer ← textAt (-2)
                    detail ← textAt (-1)
                    finish $ Right $ case answer of
                        Just "lethal" → ReadyLethal (fromMaybe "a lethal injury" detail)
                        other → ReadyPose other

-- | A text at a stack index, or 'Nothing' for nil.
textAt ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe Text)
textAt i = do
    isNil ← Lua.isnil i
    if isNil then pure Nothing else fmap TE.decodeUtf8Lenient ⊚ Lua.tostring i
