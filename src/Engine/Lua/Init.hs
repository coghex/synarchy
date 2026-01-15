{-# LANGUAGE Strict #-}
module Engine.Lua.Init
  ( luaInit
  , loadModScripts
  , executeInitScripts
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Scripting.Lua as Lua
import qualified Engine.Core.Queue as Q
import Engine.Lua.Types
import Engine.Lua.Base
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
import Control.Exception (catch, SomeException)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension, takeBaseName)

-- | Initialize the Lua environment and load all mod scripts
luaInit ∷ Q.Queue T.Text → FilePath → IO (Either LuaError LuaEnv)
luaInit logQueue modPath = catch
  (do
    -- Log initialization start
    logLuaInfo logQueue "Initializing Lua environment..."
    
    -- Check if mod directory exists
    modDirExists ← doesDirectoryExist modPath
    unless modDirExists $ do
      logLuaInfo logQueue $ "Mod directory does not exist, creating: " ⊕ T.pack modPath
      -- We'll assume directory is created externally or skip if not needed
    
    -- Create new Lua state
    luaState ← Lua.newstate
    Lua.openlibs luaState
    
    -- Initialize STM variables
    scriptsVar ← newTVarIO Map.empty
    tickVar ← newTVarIO 0
    
    let luaEnv = LuaEnv
          { leLuaState = luaState
          , leModPath = modPath
          , leScripts = scriptsVar
          , leCurrentTick = tickVar
          }
    
    -- Load and register all scripts from mod directory
    when modDirExists $ do
      scripts ← loadModScripts logQueue luaEnv
      atomically $ writeTVar scriptsVar scripts
    
    -- Execute all initLua scripts
    executeInitScripts logQueue luaEnv
    
    logLuaInfo logQueue "Lua environment initialized successfully"
    return $ Right luaEnv
  )
  (\(e ∷ SomeException) → do
    let errMsg = T.pack $ "Failed to initialize Lua: " ⧺ show e
    logLuaError logQueue $ LuaStateError errMsg
    return $ Left $ LuaStateError errMsg
  )

-- | Load all Lua scripts from the mod directory
loadModScripts ∷ Q.Queue T.Text → LuaEnv → IO LuaScripts
loadModScripts logQueue luaEnv = catch
  (do
    let modPath = leModPath luaEnv
    logLuaInfo logQueue $ "Loading scripts from: " ⊕ T.pack modPath
    
    -- List all files in the mod directory
    files ← listDirectory modPath
    let luaFiles = filter (\f → takeExtension f ≡ ".lua") files
    
    logLuaInfo logQueue $ T.pack $ "Found " ⧺ show (length luaFiles) ⧺ " Lua files"
    
    -- Create LuaScript entries for each file
    let scripts = Map.fromList
          [ (T.pack name, LuaScript
              { lsName = T.pack name
              , lsFilePath = modPath </> file
              , lsTickInterval = 1  -- Default tick interval; can be configured via script metadata
              , lsNextTick = 0
              , lsEnabled = True
              , lsInitialized = False
              })
          | file ← luaFiles
          , let name = takeBaseName file
          ]
    
    return scripts
  )
  (\(e ∷ SomeException) → do
    logLuaError logQueue $ LuaStateError $ T.pack $
      "Error loading mod scripts: " ⧺ show e
    return Map.empty
  )

-- | Execute all initLua functions from loaded scripts
executeInitScripts ∷ Q.Queue T.Text → LuaEnv → IO ()
executeInitScripts logQueue luaEnv = do
  scripts ← atomically $ readTVar (leScripts luaEnv)
  let luaState = leLuaState luaEnv
  
  forM_ (Map.elems scripts) $ \script → catch
    (do
      -- Load the script file
      logLuaDebug logQueue $ "Loading script: " ⊕ lsName script
      result ← Lua.loadfile luaState (lsFilePath script)
      
      when (result ≡ 0) $ do
        -- Execute the script to load its functions
        pcallResult ← Lua.pcall luaState 0 0 0
        
        when (pcallResult ≡ 0) $ do
          -- Try to call initLua if it exists
          Lua.getglobal luaState "initLua"
          isFunc ← Lua.isfunction luaState (-1)
          
          when isFunc $ do
            logLuaInfo logQueue $ "Executing initLua for: " ⊕ lsName script
            initResult ← Lua.pcall luaState 0 0 0
            
            if initResult ≡ 0
              then do
                -- Mark script as initialized
                let updatedScript = script { lsInitialized = True }
                updateScript (leScripts luaEnv) updatedScript
                logLuaInfo logQueue $ "Successfully initialized: " ⊕ lsName script
              else do
                errMsg ← Lua.tostring luaState (-1)
                logLuaError logQueue $ LuaExecutionError
                  (T.pack $ fromMaybe "Unknown error" errMsg)
                  (lsFilePath script)
                Lua.pop luaState 1
          
          -- Pop the function or nil value
          unless isFunc $ Lua.pop luaState 1
        else do
          errMsg ← Lua.tostring luaState (-1)
          logLuaError logQueue $ LuaExecutionError
            (T.pack $ fromMaybe "Unknown error" errMsg)
            (lsFilePath script)
          Lua.pop luaState 1
      else do
        errMsg ← Lua.tostring luaState (-1)
        logLuaError logQueue $ LuaLoadError
          (T.pack $ fromMaybe "Unknown error" errMsg)
          (lsFilePath script)
        Lua.pop luaState 1
    )
    (\(e ∷ SomeException) → do
      logLuaError logQueue $ LuaExecutionError
        (T.pack $ "Exception during script initialization: " ⧺ show e)
        (lsFilePath script)
    )
