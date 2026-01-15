module Engine.Lua.Base
  ( logLuaError
  , logLuaInfo
  , logLuaDebug
  , addScript
  , removeScript
  , getScript
  , updateScript
  , calculateLCM
  , formatLuaError
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Engine.Lua.Types
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, modifyTVar')

-- | Log a Lua error to the event queue
logLuaError ∷ Q.Queue T.Text → LuaError → IO ()
logLuaError queue err = Q.writeQueue queue $ T.pack $ "Lua Error: " ⧺ formatLuaError err

-- | Log informational Lua message
logLuaInfo ∷ Q.Queue T.Text → T.Text → IO ()
logLuaInfo queue msg = Q.writeQueue queue $ "Lua Info: " ⊕ msg

-- | Log debug Lua message
logLuaDebug ∷ Q.Queue T.Text → T.Text → IO ()
logLuaDebug queue msg = Q.writeQueue queue $ "Lua Debug: " ⊕ msg

-- | Format a LuaError for display
formatLuaError ∷ LuaError → String
formatLuaError (LuaLoadError msg path) =
  "Load error in " ⧺ path ⧺ ": " ⧺ T.unpack msg
formatLuaError (LuaExecutionError msg path) =
  "Execution error in " ⧺ path ⧺ ": " ⧺ T.unpack msg
formatLuaError (LuaScriptNotFound name) =
  "Script not found: " ⧺ T.unpack name
formatLuaError (LuaStateError msg) =
  "Lua state error: " ⧺ T.unpack msg

-- | Add a script to the scripts map
addScript ∷ TVar LuaScripts → LuaScript → IO ()
addScript scriptsVar script =
  atomically $ modifyTVar' scriptsVar $ Map.insert (lsName script) script

-- | Remove a script from the scripts map
removeScript ∷ TVar LuaScripts → T.Text → IO ()
removeScript scriptsVar name =
  atomically $ modifyTVar' scriptsVar $ Map.delete name

-- | Get a script by name
getScript ∷ TVar LuaScripts → T.Text → IO (Maybe LuaScript)
getScript scriptsVar name = do
  scripts ← atomically $ readTVar scriptsVar
  return $ Map.lookup name scripts

-- | Update a script in the scripts map
updateScript ∷ TVar LuaScripts → LuaScript → IO ()
updateScript scriptsVar script =
  atomically $ modifyTVar' scriptsVar $ Map.insert (lsName script) script

-- | Calculate the LCM of all tick intervals for scheduling optimization
calculateLCM ∷ LuaScripts → Word64
calculateLCM scripts =
  let intervals = [lsTickInterval s | s ← Map.elems scripts, lsTickInterval s > 0]
  in if null intervals
     then 1
     else foldr lcm 1 intervals
