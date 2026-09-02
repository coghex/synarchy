module Engine.Scripting.Lua.Util
  ( isValidRef
  , broadcastToModules
  , broadcastToModulesReportingErrors
  , nowSeconds
  , isDenseArray
  ) where

import UPrelude
import Engine.Scripting.Lua.Types (ScriptValue, LuaBackendState(..), LuaScript(..))
import Engine.Scripting.Lua.Script (callModuleFunctionReportingError)
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM (readTVarIO)
import Engine.Core.Clock (monotonicSeconds)

isValidRef ∷ Lua.Reference → Bool
isValidRef (Lua.Reference n) = n ≢ fromIntegral Lua.refnil
isValidRef Lua.RefNil        = False

-- | Count every entry in a Lua table (array part *and* associative
--   part) by full traversal. Reads only key/value *presence*, never
--   converting a key (lua_tostring on a numeric key mutates it in place
--   and breaks the following lua_next).
tableEntryCount ∷ Lua.StackIndex → Lua.LuaE Lua.Exception Int
tableEntryCount idx = do
    Lua.pushvalue idx   -- work on a copy so a relative index stays stable
    Lua.pushnil         -- first key
    let loop c = do
          more ← Lua.next (-2)
          if not more
            then Lua.pop 1 >> return c   -- pop the table copy; done
            else Lua.pop 1 >> loop (c + 1)  -- pop value, keep key for next
    loop (0 ∷ Int)

-- | Is the table at this index a DENSE one-based array — no holes, no
--   associative keys?
--
--   'Lua.rawlen' alone cannot answer this: it returns a *border*, so a
--   sparse @{ [1] = a, [3] = b }@ may report length 1 (silently hiding
--   @b@ from a @1..n@ loop) or 3, depending on the table's internals.
--   Comparing the border against the true entry count rejects both
--   shapes deterministically, and also catches a stray associative key
--   (@{ "W", x = 1 }@).
--
--   The caller is expected to treat False as a malformed ARGUMENT
--   rather than silently processing a prefix.
isDenseArray ∷ Lua.StackIndex → Lua.LuaE Lua.Exception Bool
isDenseArray idx = do
    n     ← Lua.rawlen idx
    total ← tableEntryCount idx
    pure (total ≡ fromIntegral n)

-- | The scheduler's clock, in seconds: the MONOTONIC source from
--   "Engine.Core.Clock" (#2204), shared with the render, world and unit
--   ticks. Every 'scriptNextTick' is a value of this clock and every
--   due-check compares against it, so the origin is irrelevant; what
--   matters is that it never steps. The wall clock it replaced could,
--   and a host sleep or clock correction then left every timed script
--   badly overdue and replaying its missed intervals as a burst — see
--   'Engine.Scripting.Lua.TickPolicy.advanceTick' for the deadline rule
--   that closes the rest of that. (The predecessor of THAT, 'utctDayTime',
--   wrapped to 0 at UTC midnight and stalled every script tick for a
--   day; a monotonic source cannot wrap either.) Wall-clock consumers —
--   @engine.realTime()@, log and save timestamps — keep their own source.
nowSeconds ∷ IO Double
nowSeconds = monotonicSeconds

-- | Broadcast a callback to all loaded Lua modules.
--   Thread safety: only called from the Lua thread (via processLuaMsg
--   in luaTick). The Lua.State is never touched from other threads;
--   inter-thread communication uses STM queues (luaQueue, debugQueue).
broadcastToModules ∷ LuaBackendState → T.Text → [ScriptValue] → IO ()
broadcastToModules ls funcName args =
    void $ broadcastToModulesReportingErrors ls funcName args

-- | 'broadcastToModules', additionally RETURNING every callback that
--   raised paired with its module's 'scriptPath' (issue #1204), in
--   broadcast order. Isolation is unchanged: each callback is still
--   pcall-guarded and still logged individually, and one that raises
--   never stops the ones after it — failures are AGGREGATED across the
--   whole broadcast rather than short-circuiting at the first, which is
--   what lets a caller report the complete set. An empty result means
--   every registered module's callback completed.
broadcastToModulesReportingErrors
    ∷ LuaBackendState → T.Text → [ScriptValue] → IO [(FilePath, T.Text)]
broadcastToModulesReportingErrors ls funcName args = do
    scriptsMap ← readTVarIO (lbsScripts ls)
    fmap catMaybes $ forM (Map.elems scriptsMap) $ \script →
        if isValidRef (scriptModuleRef script)
            then do
                mErr ← callModuleFunctionReportingError
                           ls (scriptModuleRef script) funcName args
                pure $ (,) (scriptPath script) ⊚ mErr
            else pure Nothing
