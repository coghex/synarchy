-- | The production paused-readiness hook for scenario units (#2710,
--   SCN-10 of epic #2698): 'Scenario.Units.Install.uirReady' run against
--   the Lua state.
--
--   It calls @unit_resources.settleScenarioUnit(uid)@, which settles the
--   two Lua-owned derived states through the producers the per-tick pass
--   itself uses — the calorie strength discount (@reconcileStrength@ →
--   @starvation.refreshStrength@) and the locomotor pose
--   (@unit_resource_injury.injuryPose@) — and answers the pose to
--   establish. Nothing is advanced and nothing is queued.
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
import Unit.Types (UnitId(..))

-- | The Lua function the hook calls: @require(module)[name]@.
settleFunction ∷ (Text, Text)
settleFunction = ("scripts.unit_resources", "settleScenarioUnit")

luaUnitReadiness ∷ Lua.State → UnitId → IO (Either Text (Maybe Text))
luaUnitReadiness st (UnitId n) = Lua.runWith st settle
  where
  settle ∷ Lua.LuaE Lua.Exception (Either Text (Maybe Text))
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
                called ← Lua.pcall 1 1 Nothing
                if called ≢ Lua.OK then failure else do
                    isNil ← Lua.isnil (-1)
                    if isNil then finish (Right Nothing) else do
                        s ← Lua.tostring (-1)
                        finish (Right (TE.decodeUtf8Lenient ⊚ s))
