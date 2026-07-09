{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Buildings.Yaml
    ( loadBuildingYamlFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo, logDebug)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister, resolveTexturePath)
import Engine.Asset.YamlBuildings (BuildingYamlDef(..), BuildingYamlAnim(..),
                                   BuildingYamlTileSize(..), loadBuildingYaml)
import Building.Types
import Unit.Direction (Direction(..))
import Unit.Types (Animation(..))

-- * YAML loading

loadBuildingYamlFn ∷ EngineEnv → LuaBackendState
                   → Lua.LuaE Lua.Exception Lua.NumResults
loadBuildingYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadBuildingYaml logger filePath

                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc def → do
                    let name      = bydName def
                        spritePath = T.unpack (bydSprite def)
                        unknownBuilding = "assets/textures/buildings/unknown_building.png"

                    resolvedSprite ← resolveTexturePath env "Building sprite"
                                          unknownBuilding spritePath
                    handle ← loadAndRegister env backendState lteq
                                 ("building_" <> name) resolvedSprite

                    -- Build animations: frame textures are loaded via
                    -- the same loader. We only key by the single
                    -- direction "default" (mapped to DirS internally).
                    animMap ← foldM (\accA (animName, animDef) → do
                        frameMap ← foldM (\accF (_dirKey, framePaths) → do
                            handles ← mapM (\(i, p) → do
                                resolved ← resolveTexturePath env "Building animation frame"
                                               unknownBuilding (T.unpack p)
                                loadAndRegister env backendState lteq
                                    ("building_" <> name
                                     <> "_" <> animName
                                     <> "_" <> T.pack (show i))
                                    resolved
                                ) (zip [(0 ∷ Int)..] framePaths)
                            return (Map.insert DirS
                                      (V.fromList handles) accF)
                            ) Map.empty (Map.toList (byaFrames animDef))
                        let anim = Animation
                                { aFps    = byaFps animDef
                                , aLoop   = byaLoop animDef
                                , aFlip   = False  -- buildings have a
                                                   -- single direction
                                                   -- (DirS); the mirror
                                                   -- path never fires
                                                   -- for them.
                                , aFrames = frameMap
                                }
                        return (HM.insert animName anim accA)
                        ) HM.empty (Map.toList (bydAnimations def))

                    let stateAnims = HM.fromList (Map.toList (bydStateAnims def))

                    -- Default display_name to the raw name if YAML
                    -- didn't supply one — keeps older defs renderable
                    -- in the build menu without forcing a YAML edit.
                    let displayName = if T.null (bydDisplayName def)
                                      then name
                                      else bydDisplayName def
                    let bdef = BuildingDef
                            { bdName            = name
                            , bdDisplayName     = displayName
                            , bdCategory        = bydCategory def
                            , bdDescription     = bydDescription def
                            , bdTexture         = handle
                            , bdTileW           = bytsX (bydTileSize def)
                            , bdTileH           = bytsY (bydTileSize def)
                            , bdPlacement       = bydPlacement def
                            , bdIsStarting      = bydIsStarting def
                            , bdRace            = bydRace def
                            , bdSpriteAnchor    = bydSpriteAnchor def
                            , bdBuildWork       = bydBuildWork def
                            , bdMaterials       = HM.fromList (Map.toList (bydMaterials def))
                            , bdStorageCapacity = bydStorageCapacity def
                            , bdOperations      = bydOperations def
                            , bdAnimations      = animMap
                            , bdStateAnims      = stateAnims
                            , bdPowerDrain      = bydPowerDrain def
                            }
                    atomicModifyIORef' (buildingManagerRef env) $ \bm →
                        (bm { bmDefs = HM.insert name bdef (bmDefs bm) }, ())

                    logDebug logger CatAsset $
                        "Registered building def: " <> name
                        <> " (" <> T.pack (show (HM.size animMap))
                        <> " animations, " <> T.pack (show (bytsX (bydTileSize def)))
                        <> "x" <> T.pack (show (bytsY (bydTileSize def))) <> ")"

                    return (acc + 1)
                    ) (0 ∷ Int) defs

                logInfo logger CatAsset $
                    "loadBuildingYaml: loaded " <> T.pack (show total)
                    <> " building definitions from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1
