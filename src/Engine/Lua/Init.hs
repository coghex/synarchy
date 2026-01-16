module Engine.Lua.Init where

import UPrelude
import Control.Monad (forM_)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Time.Clock (getCurrentTime, addUTCTime)
import qualified HsLua as Lua
import Engine.Lua.Types
import Engine.Lua.Base (addScript, logEvent)

-- initialize the lua environment and load all initLua scripts
luaInit ∷ LuaEnv → FilePath → IO ()
luaInit env modDir = do
  logEvent (luaEventQueue env) LuaLogInfo "initializing lua environment"
  let ls = luaState env
  -- open lua standard libraries
  _ ← Lua.runWith ls Lua.openlibs

  -- load all initLua scripts
  loadInitScripts env modDir
  logEvent (luaEventQueue env) LuaLogInfo "lua environment initialized"

loadInitScripts ∷ LuaEnv → FilePath → IO ()
loadInitScripts env modDir = do
  let scripts = luaScripts env
  files ← listDirectory modDir
  let luaFiles = filter (\f → takeExtension f == ".lua") files
  forM_ luaFiles $ \file → do
    let filePath = modDir </> file
    --res ← Lua.runWith @Lua.Exception (luaState env) $ Lua.dofile $ Just filePath
    res ← Lua.runWith @Lua.Exception (luaState env)
                                      $ Lua.dofile $ Just filePath
    case res of
      Lua.OK → do
                  tickInterval ← getLuaTickInterval (luaState env)
                  addScript (luaScripts env) filePath tickInterval
                  logEvent (luaEventQueue env) LuaLogInfo
                    $ "loaded lua script: " ⧺ filePath
                    ⧺ " with tick interval " ⧺ (show tickInterval)
      err → logEvent (luaEventQueue env) LuaLogError
                   $ "failed to load lua script " ⧺ filePath ⧺ ": " ⧺ (show err)

getLuaTickInterval ∷ Lua.State → IO (Double)
getLuaTickInterval ls = do
  res ← Lua.runWith @Lua.Exception ls $ do
    Lua.getglobal "tickInterval"
    typ ← Lua.ltype (-1)
    case typ of
      Lua.TypeNumber → do
        v ← Lua.peek (-1) ∷ Lua.Lua Double
        Lua.pop 1
        return $ Right v
      _ → do
        Lua.pop 1
        return $ Left "tickInterval is not a number"
  return $ case res of
    Left _ → 1.0  -- default tick interval
    Right v → v
