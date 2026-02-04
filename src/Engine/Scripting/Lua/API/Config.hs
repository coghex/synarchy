{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Config
  ( -- Config functions
    getVideoConfigFn
  , setVideoConfigFn
  , saveVideoConfigFn
  ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Config

-- | Get current video config settings
-- Returns: width, height, fullscreen, vsync, msaa
getVideoConfigFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
getVideoConfigFn env = do
    config <- Lua.liftIO $ readIORef (videoConfigRef env)
    Lua.pushinteger (fromIntegral $ vcWidth config)
    Lua.pushinteger (fromIntegral $ vcHeight config)
    Lua.pushboolean (vcFullscreen config)
    Lua.pushboolean (vcVSync config)
    Lua.pushinteger (fromIntegral $ vcMSAA config)
    return 5

-- | Set video config (doesn't save to file)
-- engine.setVideoConfig(width, height, fullscreen, vsync, msaa)
setVideoConfigFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
setVideoConfigFn env = do
    widthArg <- Lua.tointeger 1
    heightArg <- Lua.tointeger 2
    fullscreenArg <- Lua.toboolean 3
    vsyncArg <- Lua.toboolean 4
    msaaArg <- Lua.tointeger 5
    
    case (widthArg, heightArg, fullscreenArg, vsyncArg, msaaArg) of
        (Just w, Just h, fs, vs, Just m) -> do
            Lua.liftIO $ do
                oldConfig <- readIORef (videoConfigRef env)
                let newConfig = oldConfig
                      { vcWidth = fromIntegral w
                      , vcHeight = fromIntegral h
                      , vcFullscreen = fs
                      , vcVSync = vs
                      , vcMSAA = fromIntegral m
                      }
                writeIORef (videoConfigRef env) newConfig
            Lua.pushboolean True
        _ -> Lua.pushboolean False
    
    return 1

-- | Save current video config to file
-- engine.saveVideoConfig()
saveVideoConfigFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
saveVideoConfigFn env = do
    Lua.liftIO $ do
        config <- readIORef (videoConfigRef env)
        logger <- readIORef (loggerRef env)
        saveVideoConfig logger "config/video.yaml" config
    return 0
