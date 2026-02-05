{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Config
  ( -- Config functions
    getVideoConfigFn
  , setVideoConfigFn
  , saveVideoConfigFn
  , setUIScaleFn
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
    let scale = realToFrac (vcUIScale config) ∷ Double
    Lua.pushinteger (fromIntegral $ vcWidth config)
    Lua.pushinteger (fromIntegral $ vcHeight config)
    Lua.pushboolean (vcFullscreen config)
    Lua.pushnumber (Lua.Number scale)
    Lua.pushboolean (vcVSync config)
    Lua.pushinteger (maybe 0 fromIntegral $ vcFrameLimit config)
    Lua.pushinteger (fromIntegral $ vcMSAA config)
    return 7

-- | Set video config (doesn't save to file)
-- engine.setVideoConfig(width, height, fullscreen, uiscale, vsync, msaa)
setVideoConfigFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
setVideoConfigFn env = do
    widthArg <- Lua.tointeger 1
    heightArg <- Lua.tointeger 2
    fullscreenArg <- Lua.toboolean 3
    uiScale <- Lua.tonumber 4
    vsyncArg <- Lua.toboolean 5
    framelimitArg <- Lua.tointeger 6
    msaaArg <- Lua.tointeger 7
    
    case (widthArg, heightArg, fullscreenArg, uiScale, vsyncArg,
         framelimitArg, msaaArg) of
        (Just w, Just h, fs, Just uis, vs, Just fl, Just m) -> do
            Lua.liftIO $ do
                oldConfig <- readIORef (videoConfigRef env)
                let newConfig = oldConfig
                      { vcWidth = fromIntegral w
                      , vcHeight = fromIntegral h
                      , vcFullscreen = fs
                      , vcUIScale = realToFrac uis
                      , vcVSync = vs
                      , vcFrameLimit = if fl > 0 then Just (fromIntegral fl)
                                                 else Nothing
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

-- | Set UI scale only
-- engine.setUIScale(scale)
setUIScaleFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
setUIScaleFn env = do
    scaleArg <- Lua.tonumber 1
    
    case scaleArg of
        Just scale -> do
            Lua.liftIO $ do
                oldConfig <- readIORef (videoConfigRef env)
                let newConfig = oldConfig { vcUIScale = realToFrac scale }
                writeIORef (videoConfigRef env) newConfig
            Lua.pushboolean True
        Nothing -> Lua.pushboolean False
    
    return 1
