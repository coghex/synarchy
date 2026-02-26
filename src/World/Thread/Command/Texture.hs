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
                  -- Structural
                  OceanTexture        → wt { wtOceanTexture     = texHandle }
                  GlacierTexture      → wt { wtGlacierTexture   = texHandle }
                  LavaTexture         → wt { wtLavaTexture      = texHandle }
                  BlankTexture        → wt { wtBlankTexture     = texHandle }
                  NoTexture           → wt { wtNoTexture        = texHandle }
                  -- Facemaps
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
                  -- Material layers → insert into the right HashMap
                  MatTileTexture mid  → wt { wtTileTextures = HM.insert mid texHandle (wtTileTextures wt) }
                  MatZoomTexture mid  → wt { wtZoomTextures = HM.insert mid texHandle (wtZoomTextures wt) }
                  MatBgTexture   mid  → wt { wtBgTextures   = HM.insert mid texHandle (wtBgTextures wt) }
            atomicModifyIORef' (wsTexturesRef worldState) 
                (\wt → (updateTextures wt, ()))
            logDebug logger CatWorld $ 
                "Texture updated for world: " <> unWorldPageId pageId
        Nothing → 
            logDebug logger CatWorld $ 
                "World not found for texture update: " <> unWorldPageId pageId
 
