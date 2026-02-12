{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Config
  ( -- Config functions
    getVideoConfigFn
  , setVideoConfigFn
  , saveVideoConfigFn
  , loadDefaultConfigFn
  , setUIScaleFn
  , setFrameLimitFn
  , setResolutionFn
  , setWindowModeFn
  , setVSyncFn
  , setMSAAFn
  , setBrightnessFn
  , setPixelSnapFn
  , setTextureFilterFn
  ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Core.Queue as Q
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logWarn, LogCategory(..))
import Engine.Graphics.Config
import Engine.Graphics.Window.Types (Window(..))
import Engine.Scripting.Lua.Types (LuaToEngineMsg(..))

-- | Get current video config settings
-- Returns: width, height, fullscreen, vsync, msaa
getVideoConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getVideoConfigFn env = do
    config ← Lua.liftIO $ readIORef (videoConfigRef env)
    let scale = realToFrac (vcUIScale config) ∷ Double
    Lua.pushinteger (fromIntegral $ vcWidth config)
    Lua.pushinteger (fromIntegral $ vcHeight config)
    Lua.pushstring (TE.encodeUtf8 $ windowModeToText $ vcWindowMode config)
    Lua.pushnumber (Lua.Number scale)
    Lua.pushboolean (vcVSync config)
    Lua.pushinteger (maybe 0 fromIntegral $ vcFrameLimit config)
    Lua.pushinteger (fromIntegral $ vcMSAA config)
    Lua.pushinteger (fromIntegral $ vcBrightness config)
    Lua.pushboolean (vcPixelSnap config)
    Lua.pushstring (TE.encodeUtf8 $ textureFilterToText $ vcTextureFilter config)
    return 10

-- | Set video config (doesn't save to file)
setVideoConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setVideoConfigFn env = do
    widthArg ← Lua.tointeger 1
    heightArg ← Lua.tointeger 2
    fullscreenArg ← Lua.tostring 3
    uiScaleArg ← Lua.tonumber 4
    vsyncArg ← Lua.toboolean 5
    framelimitArg ← Lua.tointeger 6
    msaaArg ← Lua.tointeger 7
    brightnessArg ← Lua.tointeger 8
    pixelSnapArg ← Lua.toboolean 9
    textureFilterArg ← Lua.tostring 10
    
    case (widthArg, heightArg, fullscreenArg, uiScaleArg, vsyncArg,
         framelimitArg, msaaArg, brightnessArg, pixelSnapArg, textureFilterArg) of
        (Just w, Just h, Just wmBS, Just uis, vs, Just fl, Just m, Just b, ps, Just tf) → do
            let wm = case windowModeFromText (TE.decodeUtf8 wmBS) of
                        Just mode → mode
                        Nothing   → Windowed
            Lua.liftIO $ do
                oldConfig ← readIORef (videoConfigRef env)
                let newConfig = oldConfig
                      { vcWidth = fromIntegral w
                      , vcHeight = fromIntegral h
                      , vcWindowMode = wm
                      , vcUIScale = realToFrac uis
                      , vcVSync = vs
                      , vcFrameLimit = if fl > 0 then Just (fromIntegral fl)
                                                 else Nothing
                      , vcMSAA = fromIntegral m
                      , vcBrightness = fromIntegral b
                      , vcPixelSnap = ps
                      , vcTextureFilter = case textureFilterFromText (TE.decodeUtf8 tf) of
                                          Just filter → filter
                                          Nothing     → vcTextureFilter oldConfig
                      }
                writeIORef (videoConfigRef env) newConfig
            Lua.pushboolean True
        _ → Lua.pushboolean False
    
    return 1

-- | Save current video config to file
saveVideoConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveVideoConfigFn env = do
    Lua.liftIO $ do
        config ← readIORef (videoConfigRef env)
        logger ← readIORef (loggerRef env)
        saveVideoConfig logger "config/video.yaml" config
    return 0

-- | Load default video config from video_default.yaml
-- Returns same values as getVideoConfigFn
loadDefaultConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadDefaultConfigFn env = do
    Lua.liftIO $ do
        logger ← readIORef (loggerRef env)
        defaultConfig ← loadVideoConfig logger "config/video_default.yaml"
        writeIORef (videoConfigRef env) defaultConfig
        logInfo logger CatInit "Loaded default video config"
    
    -- Return same values as getVideoConfigFn
    config ← Lua.liftIO $ readIORef (videoConfigRef env)
    let scale = realToFrac (vcUIScale config) ∷ Double
    Lua.pushinteger (fromIntegral $ vcWidth config)
    Lua.pushinteger (fromIntegral $ vcHeight config)
    Lua.pushstring (TE.encodeUtf8 $ windowModeToText $ vcWindowMode config)
    Lua.pushnumber (Lua.Number scale)
    Lua.pushboolean (vcVSync config)
    Lua.pushinteger (maybe 0 fromIntegral $ vcFrameLimit config)
    Lua.pushinteger (fromIntegral $ vcMSAA config)
    Lua.pushinteger (fromIntegral $ vcBrightness config)
    Lua.pushboolean (vcPixelSnap config)
    Lua.pushstring (TE.encodeUtf8 $ textureFilterToText $ vcTextureFilter config)
    return 10

-- | Set UI scale only
setUIScaleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setUIScaleFn env = do
    scaleArg ← Lua.tonumber 1
    
    case scaleArg of
        Just scale → do
            Lua.liftIO $ do
                oldConfig ← readIORef (videoConfigRef env)
                let newConfig = oldConfig { vcUIScale = realToFrac scale }
                writeIORef (videoConfigRef env) newConfig
            Lua.pushboolean True
        Nothing → Lua.pushboolean False
    return 1

-- | Set frame limit only
setFrameLimitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setFrameLimitFn env = do
    frameLimitArg ← Lua.tointeger 1
    case frameLimitArg of
        Just fl → do
            Lua.liftIO $ do
                oldConfig ← readIORef (videoConfigRef env)
                let newConfig = oldConfig { vcFrameLimit = if fl > 0 then
                                                             Just (fromIntegral fl)
                                                           else Nothing }
                writeIORef (videoConfigRef env) newConfig
            Lua.pushboolean True
        Nothing → Lua.pushboolean False
    return 1

-- | Set resolution - sends message to engine thread to resize window
-- engine.setResolution(width, height)
setResolutionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setResolutionFn env = do
    widthArg ← Lua.tointeger 1
    heightArg ← Lua.tointeger 2
    
    case (widthArg, heightArg) of
        (Just w, Just h) → do
            Lua.liftIO $ do
                -- Update the config
                oldConfig ← readIORef (videoConfigRef env)
                let newConfig = oldConfig
                      { vcWidth = fromIntegral w
                      , vcHeight = fromIntegral h
                      }
                writeIORef (videoConfigRef env) newConfig
                -- Send resize request to main thread
                Q.writeQueue (luaToEngineQueue env) 
                    (LuaSetResolution (fromIntegral w) (fromIntegral h))
            Lua.pushboolean True
        _ → Lua.pushboolean False
    return 1

-- | engine.setWindowMode(modeString)
-- modeString: "fullscreen", "borderless", or "windowed"
setWindowModeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setWindowModeFn env = do
    modeArg ← Lua.tostring 1
    
    case modeArg of
        Just modeBS → do
            let modeText = TE.decodeUtf8 modeBS
            case windowModeFromText modeText of
                Just wm → Lua.liftIO $ do
                    let lteq = luaToEngineQueue env
                    Q.writeQueue lteq (LuaSetWindowMode wm)
                    oldConfig ← readIORef (videoConfigRef env)
                    writeIORef (videoConfigRef env) $ oldConfig { vcWindowMode = wm }
                Nothing → pure ()
        Nothing → pure ()
    
    return 0

-- | engine.setVSync(enabled)
setVSyncFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setVSyncFn env = do
    vsyncArg ← Lua.toboolean 1
    Lua.liftIO $ do
            let lteq = luaToEngineQueue env
            Q.writeQueue lteq (LuaSetVSync vsyncArg)
            oldConfig ← readIORef (videoConfigRef env)
            writeIORef (videoConfigRef env) $ oldConfig { vcVSync = vsyncArg }
    return 0

setMSAAFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setMSAAFn env = do
    msaaArg ← Lua.tointeger 1
    msaa ← case msaaArg of
        Just m  → pure (fromIntegral m)
        Nothing → pure 0
    Lua.liftIO $ do
            let lteq = luaToEngineQueue env
            Q.writeQueue lteq (LuaSetMSAA msaa)
            oldConfig ← readIORef (videoConfigRef env)
            writeIORef (videoConfigRef env) $ oldConfig { vcMSAA = msaa }
    return 0

setBrightnessFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setBrightnessFn env = do
    brightnessArg ← Lua.tointeger 1
    brightness ← case brightnessArg of
        Just b  → pure $ fromIntegral b
        Nothing → pure 100
    Lua.liftIO $ do
            let lteq = luaToEngineQueue env
            Q.writeQueue lteq (LuaSetBrightness brightness)
            oldConfig ← readIORef (videoConfigRef env)
            writeIORef (videoConfigRef env) $ oldConfig { vcBrightness = brightness }
    return 0

setPixelSnapFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setPixelSnapFn env = do
    enabled ← Lua.toboolean 1
    Lua.liftIO $ do
        writeIORef (pixelSnapRef env) enabled
        oldConfig ← readIORef (videoConfigRef env)
        writeIORef (videoConfigRef env) $ oldConfig { vcPixelSnap = enabled }
    return 0

setTextureFilterFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setTextureFilterFn env = do
    filterArg ← Lua.tostring 1
    case filterArg of
        Just filterBS → do
            let filterText = TE.decodeUtf8 filterBS
            case textureFilterFromText filterText of
                Just tf → Lua.liftIO $ do
                    writeIORef (textureFilterRef env) tf
                    oldConfig ← readIORef (videoConfigRef env)
                    writeIORef (videoConfigRef env) $ oldConfig { vcTextureFilter = tf }
                    let lteq = luaToEngineQueue env
                    Q.writeQueue lteq (LuaSetTextureFilter tf)
                Nothing → pure ()
        Nothing → pure ()
    return 0
