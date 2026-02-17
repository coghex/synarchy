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
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import World.Types (WorldCommand(..), WorldPageId(..), WorldTextureType(..))
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

parseTextureType ∷ Text → WorldTextureType
parseTextureType "granite"        = GraniteTexture
parseTextureType "diorite"        = DioriteTexture
parseTextureType "gabbro"         = GabbroTexture
parseTextureType "glacier"        = GlacierTexture
parseTextureType "ocean"          = OceanTexture
parseTextureType "lava"           = LavaTexture
parseTextureType "blank"          = BlankTexture
parseTextureType "notexture"      = NoTexture
parseTextureType "iso_facemap"             = IsoFaceMap
parseTextureType "iso_slope_facemap_n"      = SlopeFaceMapN
parseTextureType "iso_slope_facemap_e"      = SlopeFaceMapE
parseTextureType "iso_slope_facemap_ne"     = SlopeFaceMapNE
parseTextureType "iso_slope_facemap_s"      = SlopeFaceMapS
parseTextureType "iso_slope_facemap_ns"     = SlopeFaceMapNS
parseTextureType "iso_slope_facemap_es"     = SlopeFaceMapES
parseTextureType "iso_slope_facemap_nes"    = SlopeFaceMapNES
parseTextureType "iso_slope_facemap_w"      = SlopeFaceMapW
parseTextureType "iso_slope_facemap_nw"     = SlopeFaceMapNW
parseTextureType "iso_slope_facemap_ew"     = SlopeFaceMapEW
parseTextureType "iso_slope_facemap_new"    = SlopeFaceMapNEW
parseTextureType "iso_slope_facemap_sw"     = SlopeFaceMapSW
parseTextureType "iso_slope_facemap_nsw"    = SlopeFaceMapNSW
parseTextureType "iso_slope_facemap_esw"    = SlopeFaceMapESW
parseTextureType "iso_slope_facemap_nesw"   = SlopeFaceMapNESW
parseTextureType "nofacemap"               = NoFaceMap
parseTextureType "zoom_granite"   = ZoomGraniteTexture
parseTextureType "zoom_diorite"   = ZoomDioriteTexture
parseTextureType "zoom_gabbro"    = ZoomGabbroTexture
parseTextureType "zoom_ocean"     = ZoomOceanTexture
parseTextureType "zoom_glacier"   = ZoomGlacierTexture
parseTextureType "zoom_lava"      = ZoomLavaTexture
parseTextureType "zoom_sandstone" = ZoomSandstoneTexture
parseTextureType "zoom_limestone" = ZoomLimestoneTexture
parseTextureType "zoom_shale"     = ZoomShaleTexture
parseTextureType "zoom_iron"      = ZoomIronTexture
parseTextureType "zoom_olivine"   = ZoomOlivineTexture
parseTextureType "zoom_pyroxene"  = ZoomPyroxeneTexture
parseTextureType "zoom_feldspar"  = ZoomFeldsparTexture
parseTextureType "bg_granite"     = BgGraniteTexture
parseTextureType "bg_diorite"     = BgDioriteTexture
parseTextureType "bg_gabbro"      = BgGabbroTexture
parseTextureType "bg_ocean"       = BgOceanTexture
parseTextureType "bg_glacier"     = BgGlacierTexture
parseTextureType "bg_lava"        = BgLavaTexture
parseTextureType "basalt"         = BasaltTexture
parseTextureType "obsidian"       = ObsidianTexture
parseTextureType "sandstone"      = SandstoneTexture
parseTextureType "limestone"      = LimestoneTexture
parseTextureType "shale"          = ShaleTexture
parseTextureType "impactite"      = ImpactiteTexture
parseTextureType "iron"           = IronTexture
parseTextureType "olivine"        = OlivineTexture
parseTextureType "pyroxene"       = PyroxeneTexture
parseTextureType "feldspar"       = FeldsparTexture
parseTextureType "zoom_basalt"    = ZoomBasaltTexture
parseTextureType "zoom_obsidian"  = ZoomObsidianTexture
parseTextureType "zoom_impactite" = ZoomImpactiteTexture
parseTextureType "bg_basalt"      = BgBasaltTexture
parseTextureType "bg_impactite"   = BgImpactiteTexture
parseTextureType "bg_obsidian"    = BgObsidianTexture
parseTextureType "bg_sandstone"   = BgSandstoneTexture
parseTextureType "bg_limestone"   = BgLimestoneTexture
parseTextureType "bg_shale"       = BgShaleTexture
parseTextureType "bg_iron"        = BgIronTexture
parseTextureType "bg_olivine"     = BgOlivineTexture
parseTextureType "bg_pyroxene"    = BgPyroxeneTexture
parseTextureType "bg_feldspar"    = BgFeldsparTexture
parseTextureType _                = NoTexture
