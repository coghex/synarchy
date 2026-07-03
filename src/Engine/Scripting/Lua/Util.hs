module Engine.Scripting.Lua.Util
  ( isValidRef
  , broadcastToModules
  , nowSeconds
  ) where

import UPrelude
import Engine.Scripting.Types (ScriptValue)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaScript(..))
import Engine.Scripting.Lua.Script (callModuleFunction)
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM (readTVarIO)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

isValidRef ∷ Lua.Reference → Bool
isValidRef (Lua.Reference n) = n ≠ fromIntegral Lua.refnil
isValidRef Lua.RefNil        = False

-- | Wall-clock seconds since the POSIX epoch. The tick scheduler must
--   use this, not 'utctDayTime' — day time wraps to 0 at UTC midnight,
--   which left every scriptNextTick a full day in the future and
--   stalled all script ticks.
nowSeconds ∷ IO Double
nowSeconds = realToFrac ∘ utcTimeToPOSIXSeconds ⊚ getCurrentTime

-- | Broadcast a callback to all loaded Lua modules.
--   Thread safety: only called from the Lua thread (via processLuaMsg
--   in runLuaLoop). The Lua.State is never touched from other threads;
--   inter-thread communication uses STM queues (luaQueue, debugQueue).
broadcastToModules ∷ LuaBackendState → T.Text → [ScriptValue] → IO ()
broadcastToModules ls funcName args = do
    scriptsMap ← readTVarIO (lbsScripts ls)
    forM_ (Map.elems scriptsMap) $ \script →
        when (isValidRef (scriptModuleRef script)) $ do
            callModuleFunction ls (scriptModuleRef script) funcName args
