{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.World
    ( worldInitFn
    , worldShowFn
    , worldHideFn
    , worldSetTextureFn
    , worldSetCameraFn
    , worldSetSunAngleFn
    , worldSetTimeFn
    , worldSetDateFn
    , worldSetTimeScaleFn
    , worldSetMapModeFn
    , worldSetZoomCursorHoverFn
    , worldSetZoomCursorSelectFn
    , worldClearZoomCursorSelectFn
    , worldSetZoomCursorSelectTextureFn
    , worldSetZoomCursorHoverTextureFn
    , worldSetWorldCursorSelectTextureFn
    , worldSetWorldCursorHoverTextureFn
    , worldSetWorldCursorHoverFn
    , worldSetWorldCursorSelectFn
    , worldClearWorldCursorSelectFn
    , worldSetWorldCursorSelectBgTextureFn
    , worldSetWorldCursorHoverBgTextureFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.Material (parseTextureType)
import World.Types (WorldCommand(..), WorldPageId(..), WorldTextureType(..))
import World.Render.Zoom.Types (ZoomMapMode(..), textToMapMode)
import Data.IORef (atomicModifyIORef')

-- | world.init(pageId, seed, worldSizeInChunks)
worldInitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitFn env = do
    pageIdArg ← Lua.tostring 1
    seedArg   ← Lua.tointeger 2
    sizeArg   ← Lua.tointeger 3
    platesArg ← Lua.tointeger 4
    
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                seed   = maybe 42 fromIntegral seedArg
                size   = maybe 64 fromIntegral sizeArg
                plates = maybe 10 fromIntegral platesArg
            Q.writeQueue (worldQueue env) (WorldInit pageId seed size plates)
        Nothing → pure ()
    
    return 0

-- | world.show(pageId)
worldShowFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldShowFn env = do
    pageIdArg ← Lua.tostring 1
    
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) (WorldShow pageId)
        Nothing → pure ()
    
    return 0

-- | world.hide(pageId)
worldHideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldHideFn env = do
    pageIdArg ← Lua.tostring 1
    
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) (WorldHide pageId)
        Nothing → pure ()
    
    return 0

-- | world.setTexture(pageId, textureType, textureHandle)
worldSetTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureTypeArg ← Lua.tostring 2
    textureHandleArg ← Lua.tointeger 3
    
    case (pageIdArg, textureTypeArg, textureHandleArg) of
        (Just pageIdBS, Just typeBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texType = parseTextureType (TE.decodeUtf8 typeBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) (WorldSetTexture pageId texType texHandle)
        _ → pure ()
    
    return 0

-- | world.setCamera(pageId, x, y)
worldSetCameraFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetCameraFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3

    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just (Lua.Number x), Just (Lua.Number y)) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env)
                (WorldSetCamera pageId (realToFrac x) (realToFrac y))
        _ → pure ()

    return 0

-- | world.setSunAngle(angle)
-- Direct override of sun angle (0..1), bypasses time system
worldSetSunAngleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetSunAngleFn env = do
    angleArg ← Lua.tonumber 1

    case angleArg of
        Just (Lua.Number angle) → Lua.liftIO $ do
            atomicModifyIORef' (sunAngleRef env) $ \_ → (realToFrac angle, ())
        _ → pure ()

    return 0

-- | world.setTime(pageId, hour, minute)
-- Set the world clock. The world thread will compute sun angle from this.
worldSetTimeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetTimeFn env = do
    pageIdArg ← Lua.tostring 1
    hourArg   ← Lua.tointeger 2
    minuteArg ← Lua.tointeger 3

    case (pageIdArg, hourArg, minuteArg) of
        (Just pageIdBS, Just h, Just m) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env)
                (WorldSetTime pageId (fromIntegral h) (fromIntegral m))
        _ → pure ()

    return 0

-- | world.setDate(pageId, year, month, day)
-- Set the world date. Currently unused for sun angle (placeholder for seasons).
worldSetDateFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetDateFn env = do
    pageIdArg ← Lua.tostring 1
    yearArg   ← Lua.tointeger 2
    monthArg  ← Lua.tointeger 3
    dayArg    ← Lua.tointeger 4

    case (pageIdArg, yearArg, monthArg, dayArg) of
        (Just pageIdBS, Just y, Just mo, Just d) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env)
                (WorldSetDate pageId (fromIntegral y) (fromIntegral mo) (fromIntegral d))
        _ → pure ()

    return 0

-- | world.setTimeScale(pageId, scale)
-- Set how fast time passes: game-minutes per real-second.
-- 1.0 = real-time, 60.0 = 1 game-hour per real-second, 0.0 = paused
worldSetTimeScaleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetTimeScaleFn env = do
    pageIdArg ← Lua.tostring 1
    scaleArg  ← Lua.tonumber 2

    case (pageIdArg, scaleArg) of
        (Just pageIdBS, Just (Lua.Number s)) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env)
                (WorldSetTimeScale pageId (realToFrac s))
        _ → pure ()

    return 0

-- | world.setMapMode(pageId, mode)
worldSetMapModeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetMapModeFn env = do
    pageIdArg ← Lua.tostring 1
    modeArg   ← Lua.tostring 2

    case (pageIdArg, modeArg) of
        (Just pageIdBS, Just modeBS) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                mode = textToMapMode (TE.decodeUtf8 modeBS)
            Q.writeQueue (worldQueue env)
                (WorldSetMapMode pageId mode)
        _ → pure ()

    return 0

-- | world.setZoomCursorHover(pageId, x, y)
worldSetZoomCursorHoverFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorHoverFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3

    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorHover pageId (round x) (round y)
        _ → pure ()
    return 0

-- | world.setZoomCursorSelect(pageId, x, y)
worldSetZoomCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldSetZoomCursorSelect pageId
        _ → pure ()
    return 0

-- | world.clearZoomCursorDeselect(pageId)
worldClearZoomCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldClearZoomCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorDeselect pageId
        _ → pure ()
    return 0

worldSetZoomCursorSelectTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorSelectTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2

    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorSelectTexture pageId texHandle
        _ → pure ()
    return 0

worldSetZoomCursorHoverTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorHoverTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2

    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorHoverTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorHoverFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorHover pageId (round x) (round y)
        _ → pure ()
    return 0

worldSetWorldCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldSetWorldCursorSelect pageId
        _ → pure ()
    return 0

worldClearWorldCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldClearWorldCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldSetWorldCursorDeselect pageId
        _ → pure ()
    return 0

worldSetWorldCursorSelectTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorSelectTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorHoverTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorHoverTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorHoverBgTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverBgTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorHoverBgTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorSelectBgTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectBgTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorSelectBgTexture pageId texHandle
        _ → pure ()
    return 0
