module Engine.Lua.Init where

import UPrelude
import Control.Monad (forM_)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension, takeBaseName)
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Exception (displayException)
import qualified Control.Exception as E
import Data.Time.Clock (getCurrentTime, addUTCTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified HsLua as Lua
import Engine.Asset.Base
import Engine.Asset.Manager
import Engine.Core.Error.Exception
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Var
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

loadLuaFunctions ∷ LuaEnv → EngineM' EngineEnv ()
loadLuaFunctions env = do
  (envVar, stateVar) <- getEngineVars  -- Extract the environment and state variables from EngineM
  let ls = luaState env
  -- Run Lua initialization in IO and lift it back to EngineM
  liftIO $ Lua.runWith ls (luaSetup envVar stateVar)
  where
    luaSetup :: Var EngineEnv -> Var EngineState -> Lua.LuaE Lua.Exception ()
    luaSetup envVar stateVar = do
      -- Register the `loadTexture` function as a Lua function
      Lua.registerHaskellFunction "loadTexture" $ \(_ :: ()) -> do
        -- Attempt to fetch the string from the Lua stack
        result <- Lua.try $ Lua.tostring' (-1)

        -- Handle the result (either an error or a valid string)
        case result of
          Left err -> do
            -- Log the error if Lua.tostring' raises an exception
            let errMsg = case Lua.exceptionMessage err of
                          ""  -> "An unknown error occurred in loadTexture"
                          msg -> "loadTexture error: " <> msg
            liftIO $ liftEngineM envVar stateVar $
              throwLuaError (LuaCallbackError (T.pack errMsg)) "Error in loadTexture"
          Right path -> do
            -- Convert ByteString to FilePath using UTF-8 decoding
            let fp = T.unpack $ T.decodeUtf8 path
            liftIO $ do
              liftEngineM envVar stateVar $
                logDebug $ "Lua calling loadTexture with path: " <> fp
              assetId <- liftEngineM envVar stateVar $ loadTexture fp
              liftEngineM envVar stateVar $
                logDebug $ "Lua loadTexture returned AssetId: " <> show assetId
        return ()
        
      -- Expose the `loadTexture` function to Lua
      Lua.setglobal "loadTexture"

liftEngineM :: Var EngineEnv -> Var EngineState -> EngineM' EngineEnv a -> IO a
liftEngineM envVar stateVar action = do
  result <- runEngineM action envVar stateVar return
  case result of
    Left err -> error $ "EngineM failed: " <> displayException err
    Right value -> pure value

loadTexture ∷ FilePath → EngineM' EngineEnv AssetId
loadTexture filepath = do
  let name = T.pack $ takeBaseName filepath
      arrayName = T.pack "default"
  logDebug $ "loadTexture: attempting to load texture: " ⧺ filepath
  assetId ← loadTextureAtlas name filepath arrayName
  logDebug $ "loadTexture: loaded texture with AssetId: " ⧺ show assetId
  return assetId

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
