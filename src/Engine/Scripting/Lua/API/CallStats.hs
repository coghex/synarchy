-- | Non-consuming, independent snapshots of the Lua runtime's call window.
module Engine.Scripting.Lua.API.CallStats
  ( getLuaCallStatsFn
  , resetLuaCallStatsFn
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Engine.Scripting.Lua.CallStats
import qualified HsLua as Lua

-- | @{available, sequence, verbs={{id,count,totalDurationNs,maxDurationNs},...}}@.
-- Rows are sorted by fully qualified id. Durations are inclusive elapsed
-- monotonic nanoseconds, including nested calls and waits (not exclusive CPU).
getLuaCallStatsFn ∷ LuaCallStats → Lua.LuaE Lua.Exception Lua.NumResults
getLuaCallStatsFn stats = do
    snapshot ← Lua.liftIO (readLuaCallStats stats)
    Lua.newtable
    Lua.pushboolean (not (Map.null (callVerbs snapshot)))
    Lua.setfield (-2) "available"
    pushMetric "sequence" (callSequence snapshot)
    Lua.newtable
    forM_ (zip [1 ..] (Map.toAscList (callVerbs snapshot))) $ \(i, (name, row)) → do
        Lua.newtable
        Lua.pushstring name
        Lua.setfield (-2) "id"
        pushMetric "count" (callCount row)
        pushMetric "totalDurationNs" (callTotalDurationNs row)
        pushMetric "maxDurationNs" (callMaxDurationNs row)
        Lua.rawseti (-2) i
    Lua.setfield (-2) "verbs"
    pure 1

resetLuaCallStatsFn ∷ LuaCallStats → Lua.LuaE Lua.Exception Lua.NumResults
resetLuaCallStatsFn stats = Lua.liftIO (resetLuaCallStats stats) ≫ pure 0

pushMetric ∷ Lua.Name → Integer → Lua.LuaE Lua.Exception ()
pushMetric name value = do
    -- Keep the Lua representation non-negative even after its signed-integer
    -- range is exhausted; Haskell's accumulated values never wrap.
    Lua.pushinteger (fromInteger (min value (toInteger (maxBound ∷ Lua.Integer))))
    Lua.setfield (-2) name
