module World.Thread.Command.Texture
    ( handleWorldSetTextureCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, logWarn
                       , LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Constants (seaLevel)
import World.Generate (generateChunk)
import World.Generate.Constants (chunkLoadRadius)
import World.Generate.Timeline (applyTimelineFast)
import World.Geology (buildTimeline)
import World.Geology.Log (formatTimeline, formatPlatesSummary)
import World.Fluids (computeOceanMap, isOceanChunk)
import World.Plate (generatePlates, elevationAtGlobal)
import World.Preview (buildPreviewImage, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCache)
import World.Save.Serialize (saveWorld)
import World.Weather (initEarlyClimate, formatWeather, defaultClimateParams)
import World.Thread.Helpers (sendGenLog, unWorldPageId)
import World.Thread.ChunkLoading (maxChunksPerTick)

handleWorldSetTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → WorldTextureType → TextureHandle → IO ()
handleWorldSetTextureCommand env logger pageId texType texHandle = do
    logDebug logger CatWorld $ 
        "Setting texture for world: " <> unWorldPageId pageId 
        <> ", type: " <> T.pack (show texType)
        <> ", handle: " <> T.pack (show texHandle)
    
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            let updateTextures wt = case texType of
                  GraniteTexture      → wt { wtGraniteTexture   = texHandle }
                  DioriteTexture      → wt { wtDioriteTexture   = texHandle }
                  GabbroTexture       → wt { wtGabbroTexture    = texHandle }
                  OceanTexture        → wt { wtOceanTexture     = texHandle }
                  GlacierTexture      → wt { wtGlacierTexture   = texHandle }
                  LavaTexture         → wt { wtLavaTexture      = texHandle }
                  BlankTexture        → wt { wtBlankTexture     = texHandle }
                  NoTexture           → wt { wtNoTexture        = texHandle }
                  IsoFaceMap          → wt { wtIsoFaceMap       = texHandle }
                  SlopeFaceMapN       → wt { wtSlopeFaceMapN    = texHandle }
                  SlopeFaceMapE       → wt { wtSlopeFaceMapE    = texHandle }
                  SlopeFaceMapNE      → wt { wtSlopeFaceMapNE   = texHandle }
                  SlopeFaceMapS       → wt { wtSlopeFaceMapS    = texHandle }
                  SlopeFaceMapNS      → wt { wtSlopeFaceMapNS   = texHandle }
                  SlopeFaceMapES      → wt { wtSlopeFaceMapES   = texHandle }
                  SlopeFaceMapNES     → wt { wtSlopeFaceMapNES  = texHandle }
                  SlopeFaceMapW       → wt { wtSlopeFaceMapW    = texHandle }
                  SlopeFaceMapNW      → wt { wtSlopeFaceMapNW   = texHandle }
                  SlopeFaceMapEW      → wt { wtSlopeFaceMapEW   = texHandle }
                  SlopeFaceMapNEW     → wt { wtSlopeFaceMapNEW  = texHandle }
                  SlopeFaceMapSW      → wt { wtSlopeFaceMapSW   = texHandle }
                  SlopeFaceMapNSW     → wt { wtSlopeFaceMapNSW  = texHandle }
                  SlopeFaceMapESW     → wt { wtSlopeFaceMapESW  = texHandle }
                  SlopeFaceMapNESW    → wt { wtSlopeFaceMapNESW = texHandle }
                  NoFaceMap           → wt { wtNoFaceMap        = texHandle }
                  ZoomGraniteTexture  → wt { wtZoomGranite      = texHandle }
                  ZoomDioriteTexture  → wt { wtZoomDiorite      = texHandle }
                  ZoomGabbroTexture   → wt { wtZoomGabbro       = texHandle }
                  ZoomOceanTexture    → wt { wtZoomOcean        = texHandle }
                  ZoomGlacierTexture  → wt { wtZoomGlacier      = texHandle }
                  ZoomLavaTexture      → wt { wtZoomLava         = texHandle }
                  BgGraniteTexture    → wt { wtBgGranite        = texHandle }
                  BgGabbroTexture     → wt { wtBgGabbro         = texHandle }
                  BgDioriteTexture    → wt { wtBgDiorite        = texHandle }
                  BgOceanTexture      → wt { wtBgOcean          = texHandle }
                  BgGlacierTexture    → wt { wtBgGlacier        = texHandle }
                  BgLavaTexture       → wt { wtBgLava           = texHandle }
                  BasaltTexture       → wt { wtBasaltTexture    = texHandle }
                  ObsidianTexture     → wt { wtObsidianTexture  = texHandle }
                  SandstoneTexture    → wt { wtSandstoneTexture = texHandle }
                  LimestoneTexture    → wt { wtLimestoneTexture = texHandle }
                  ShaleTexture        → wt { wtShaleTexture     = texHandle }
                  ImpactiteTexture    → wt { wtImpactiteTexture = texHandle }
                  IronTexture         → wt { wtIronTexture      = texHandle }
                  OlivineTexture      → wt { wtOlivineTexture   = texHandle }
                  PyroxeneTexture     → wt { wtPyroxeneTexture  = texHandle }
                  FeldsparTexture     → wt { wtFeldsparTexture  = texHandle }
                  ZoomBasaltTexture   → wt { wtZoomBasalt       = texHandle }
                  ZoomObsidianTexture → wt { wtZoomObsidian     = texHandle }
                  ZoomImpactiteTexture → wt { wtZoomImpactite   = texHandle }
                  BgBasaltTexture     → wt { wtBgBasalt         = texHandle }
                  BgImpactiteTexture  → wt { wtBgImpactite      = texHandle }
                  BgObsidianTexture    → wt { wtBgObsidian       = texHandle }
                  ZoomSandstoneTexture  → wt { wtZoomSandstone  = texHandle }
                  ZoomLimestoneTexture  → wt { wtZoomLimestone  = texHandle }
                  ZoomShaleTexture      → wt { wtZoomShale      = texHandle }
                  ZoomIronTexture       → wt { wtZoomIron       = texHandle }
                  ZoomOlivineTexture    → wt { wtZoomOlivine    = texHandle }
                  ZoomPyroxeneTexture   → wt { wtZoomPyroxene   = texHandle }
                  ZoomFeldsparTexture   → wt { wtZoomFeldspar   = texHandle }
                  BgSandstoneTexture    → wt { wtBgSandstone    = texHandle }
                  BgLimestoneTexture    → wt { wtBgLimestone    = texHandle }
                  BgShaleTexture        → wt { wtBgShale        = texHandle }
                  BgIronTexture         → wt { wtBgIron         = texHandle }
                  BgOlivineTexture      → wt { wtBgOlivine      = texHandle }
                  BgPyroxeneTexture     → wt { wtBgPyroxene     = texHandle }
                  BgFeldsparTexture     → wt { wtBgFeldspar     = texHandle }
            atomicModifyIORef' (wsTexturesRef worldState) 
                (\wt → (updateTextures wt, ()))
            logDebug logger CatWorld $ 
                "Texture updated for world: " <> unWorldPageId pageId
        Nothing → 
            logDebug logger CatWorld $ 
                "World not found for texture update: " <> unWorldPageId pageId
 
