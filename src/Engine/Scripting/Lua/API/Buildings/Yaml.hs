{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Buildings.Yaml
    ( loadBuildingYamlFn
    ) where

import UPrelude
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv, loggerRef)
import Engine.Core.Log (LogCategory(..), logDebug)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlResult (pushYamlResult)
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister, resolveTexturePath)
import Engine.Graphics.Vulkan.Texture.Policy (UploadSampler(..))
import Engine.Asset.YamlBuildings (BuildingYamlDef(..), BuildingYamlAnim(..),
                                   BuildingYamlTileSize(..),
                                   loadBuildingYamlOutcome)
import Engine.Graphics.Camera (CameraFacing(..))
import Building.Schema
import Building.Types

-- * YAML loading

loadBuildingYamlFn ∷ EngineEnv → LuaBackendState
                   → Lua.LuaE Lua.Exception Lua.NumResults
loadBuildingYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → pushYamlResult False 0
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            (parsed, count) ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                mDefs ← loadBuildingYamlOutcome logger filePath
                let defs = fromMaybe [] mDefs

                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc def → do
                    let name      = bydName def
                        spritesDecl = bydSprites def
                        southSprite = T.unpack (facingAsset FaceSouth spritesDecl)
                        unknownBuilding = "assets/textures/buildings/unknown_building.png"
                        -- Requirement 9: the registry name carries the
                        -- facing (and, for a frame, its index), so two
                        -- views of one building can never claim the
                        -- same key and overwrite each other.
                        spriteName f = "building_" <> name <> "_" <> facingKey f
                        frameName animName f i =
                            "building_" <> name <> "_" <> animName
                                        <> "_" <> facingKey f <> "_" <> tshow i
                        loadSprite f = do
                            resolved ← resolveTexturePath env "Building sprite"
                                unknownBuilding
                                (T.unpack (facingAsset f spritesDecl))
                            loadAndRegister env backendState lteq
                                UploadGlobalSampler (spriteName f) resolved

                    -- Four independently addressable static handles. A
                    -- CANONICAL declaration loads each view's own path;
                    -- the legacy compatibility branch loads its single
                    -- path ONCE and exposes that one handle through all
                    -- four views, so an unmigrated definition costs
                    -- exactly the uploads it always did and nothing can
                    -- be overwritten (there is only one asset).
                    spriteViews ← case faSource spritesDecl of
                        AssetLegacy → do
                            h ← loadSprite FaceSouth
                            return (FacingSet h h h h)
                        AssetCanonical →
                            FacingSet ⊚ loadSprite FaceSouth
                                      ⊛ loadSprite FaceWest
                                      ⊛ loadSprite FaceNorth
                                      ⊛ loadSprite FaceEast

                    -- Dual-use (#2075): Building.Render draws the south
                    -- view in the world, and building.listDefs hands
                    -- the same art to the build menu as `iconTex`.
                    -- Loaded under BOTH policies so the world quad
                    -- follows the player's filter while the menu icon
                    -- stays nearest; the def carries both handles.
                    resolvedIcon ← resolveTexturePath env "Building sprite"
                                       unknownBuilding southSprite
                    iconHandle ← loadAndRegister env backendState lteq
                                 UploadPinnedNearest
                                 ("building_" <> name <> "_ui") resolvedIcon

                    -- Build animations: one ordered frame list per
                    -- camera facing, loaded through the same loader.
                    animMap ← foldM (\accA (animName, animDef) → do
                        let framesDecl = byaFrames animDef
                            loadFrames f = V.fromList ⊚ mapM (\(i, p) → do
                                resolved ← resolveTexturePath env
                                    "Building animation frame"
                                    unknownBuilding (T.unpack p)
                                loadAndRegister env backendState lteq
                                    UploadGlobalSampler
                                    (frameName animName f i) resolved
                                ) (zip [(0 ∷ Int)..] (facingAsset f framesDecl))
                        frameViews ← case faSource framesDecl of
                            AssetLegacy → do
                                fs ← loadFrames FaceSouth
                                return (FacingSet fs fs fs fs)
                            AssetCanonical →
                                FacingSet ⊚ loadFrames FaceSouth
                                          ⊛ loadFrames FaceWest
                                          ⊛ loadFrames FaceNorth
                                          ⊛ loadFrames FaceEast
                        -- Buildings keep the per-frame representation
                        -- (D-8): the atlas compiler covers unit
                        -- animations only, nothing here reads a
                        -- compiled index, and #1261 moved this record
                        -- to `Building.Types` when units retired theirs
                        -- rather than changing what buildings load.
                        let anim = BuildingAnimation
                                { banFps    = byaFps animDef
                                , banLoop   = byaLoop animDef
                                , banFrames = FacingAssets
                                    (faSource framesDecl) frameViews }
                        return (HM.insert animName anim accA)
                        ) HM.empty (Map.toList (bydAnimations def))

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
                            , bdTextures        = FacingAssets
                                  (faSource spritesDecl) spriteViews
                            , bdIconTexture     = iconHandle
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
                            , bdRoleAnims       = bydRoleAnims def
                            , bdVisualClass     = bydVisualClass def
                            , bdPowerDrain      = bydPowerDrain def
                            , bdPowerNode       = bydPowerNode def
                            }
                    atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
                        (bm { bmDefs = HM.insert name bdef (bmDefs bm) }, ())

                    logDebug logger CatAsset $
                        "Registered building def: " <> name
                        <> " (" <> tshow (HM.size animMap)
                        <> " animations, " <> tshow (bytsX (bydTileSize def))
                        <> "x" <> tshow (bytsY (bydTileSize def)) <> ")"

                    return (acc + 1)
                    ) (0 ∷ Int) defs

                logDebug logger CatAsset $
                    "loadBuildingYaml: loaded " <> tshow total
                    <> " building definitions from " <> T.pack filePath
                return (isJust mDefs, total)

            pushYamlResult parsed count
