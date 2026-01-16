module Engine.Lua.Base where

import UPrelude
import Control.Concurrent.STM (atomically, modifyTVar', writeTQueue, readTVarIO)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TQueue (TQueue)
import Engine.Lua.Types

-- | adds a new lua script to the engine state
addScript ∷ LuaScripts → FilePath → Double → IO ()
addScript luascripts path interval = do
  now ← getCurrentTime
  let intervalNominal = realToFrac interval ∷ NominalDiffTime
      nextTick = addUTCTime intervalNominal now
      luaScript = LuaScript path interval nextTick
  atomically $ modifyTVar' luascripts (M.insert path luaScript)

-- | log an event to the lua event queue
logEvent ∷ TQueue LuaEvent → LuaLogLevel → String → IO ()
logEvent queue level msg =
  atomically $ writeTQueue queue (LuaLog level msg)

-- | update the next tick time for a given script
updateScriptTick ∷ LuaScripts → FilePath → UTCTime → IO ()
updateScriptTick luascripts path newTick =
  atomically $ modifyTVar' luascripts $ M.adjust (\s -> s { nextTick = newTick }) path

-- | retreive the lua script associated with the given path
getScript ∷ LuaScripts → FilePath → IO (Maybe LuaScript)
getScript luascripts path = do
  scriptsMap ← readTVarIO luascripts
  return $ M.lookup path scriptsMap     

removeScript ∷ LuaScripts → FilePath → IO ()
removeScript luascripts path =
  atomically $ modifyTVar' luascripts (M.delete path)

getReadyScripts ∷ LuaScripts → IO [(FilePath, LuaScript)]
getReadyScripts luascripts = do
  now ← getCurrentTime
  scriptsMap ← readTVarIO luascripts
  let readyScripts = M.toList $ M.filter (\s -> nextTick s <= now) scriptsMap
  return readyScripts
