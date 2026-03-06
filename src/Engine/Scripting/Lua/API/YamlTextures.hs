{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.YamlTextures
    ( loadMaterialYamlFn
    , loadVegetationYamlFn
    , getTextureHandleFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo, logDebug, logWarn)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Asset.Handle (TextureHandle(..), AssetState(..))
import Engine.Asset.Manager (generateTextureHandle, updateTextureState)
import Engine.Asset.YamlTextures
import qualified Engine.Core.Queue as Q

-----------------------------------------------------------
-- engine.loadMaterialYaml(filePath)
--
-- Parses a single .yaml file,
-- loads every texture referenced (tile/zoom/bg),
-- registers name → handle mappings in the registry,
-- and queues LuaLoadTextureRequest for each.
--
-- Returns: number of textures queued for loading.
-----------------------------------------------------------

loadMaterialYamlFn ∷ EngineEnv → LuaBackendState
                   → Lua.LuaE Lua.Exception Lua.NumResults
loadMaterialYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                -- Parse the single YAML file
                defs ← loadMaterialYaml logger filePath

                -- For each MaterialDef, load 3 textures and register names
                let (lteq, _) = lbsMsgQueues backendState
                total ← foldM (\acc def → do
                    let name = mdName def
                    tileH ← loadAndRegister env backendState lteq
                                ("mat_tile_" <> name) (T.unpack (mdTile def))
                    zoomH ← loadAndRegister env backendState lteq
                                ("mat_zoom_" <> name) (T.unpack (mdZoom def))
                    bgH   ← loadAndRegister env backendState lteq
                                ("mat_bg_"   <> name) (T.unpack (mdBg def))

                    -- Also register by numeric ID for world.setTexture
                    -- compatibility: "mat_tile_56" etc.
                    let idStr = T.pack (show (mdId def))
                    registerTextureName (textureNameRegistryRef env)
                        ("mat_tile_" <> idStr) tileH
                    registerTextureName (textureNameRegistryRef env)
                        ("mat_zoom_" <> idStr) zoomH
                    registerTextureName (textureNameRegistryRef env)
                        ("mat_bg_"   <> idStr) bgH

                    return (acc + 3)
                    ) (0 ∷ Int) defs

                logInfo logger CatAsset $
                    "loadMaterialYaml: loaded " <> T.pack (show total)
                    <> " textures from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

-----------------------------------------------------------
-- engine.loadVegetationYaml(filePath)
--
-- Parses a single vegetation .yaml file,
-- loads every variant texture referenced,
-- registers name → handle mappings in the registry as
-- "veg_tile_<vegId>" for each variant,
-- and queues LuaLoadTextureRequest for each.
--
-- Returns: number of textures queued for loading.
-----------------------------------------------------------

loadVegetationYamlFn ∷ EngineEnv → LuaBackendState
                     → Lua.LuaE Lua.Exception Lua.NumResults
loadVegetationYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                -- Parse the single vegetation YAML file
                defs ← loadVegetationYaml logger filePath

                -- For each VegetationDef, load 1 texture per variant
                let (lteq, _) = lbsMsgQueues backendState
                total ← foldM (\acc def → do
                    let baseId = vdIdStart def
                        variants = vdVariants def
                    varCount ← foldM (\vacc (idx, texPath) → do
                        let vegId = baseId + fromIntegral idx
                            regName = "veg_tile_" <> T.pack (show vegId)
                        _ ← loadAndRegister env backendState lteq
                                regName (T.unpack texPath)
                        return (vacc + 1)
                        ) (0 ∷ Int) (zip [0..] variants)
                    return (acc + varCount)
                    ) (0 ∷ Int) defs

                logInfo logger CatAsset $
                    "loadVegetationYaml: loaded " <> T.pack (show total)
                    <> " textures from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

-- | Helper: generate a handle, register the name, queue the load request.
loadAndRegister ∷ EngineEnv → LuaBackendState → Q.Queue LuaToEngineMsg
                → Text → FilePath → IO TextureHandle
loadAndRegister env backendState lteq name path = do
    pool ← readIORef (lbsAssetPool backendState)
    handle ← generateTextureHandle pool
    updateTextureState handle (AssetLoading path [] 0.0) pool
    writeIORef (lbsAssetPool backendState) pool
    -- Register name → handle
    registerTextureName (textureNameRegistryRef env) name handle
    -- Queue for actual GPU loading on the engine thread
    Q.writeQueue lteq (LuaLoadTextureRequest handle path)
    return handle

-----------------------------------------------------------
-- engine.getTextureHandle(name)
--
-- Looks up a texture handle by its registered name.
-- Returns the handle integer, or -1 if not found.
-----------------------------------------------------------

getTextureHandleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getTextureHandleFn env = do
    nameArg ← Lua.tostring 1
    case nameArg of
        Nothing → do
            Lua.pushnumber (-1)
            return 1
        Just nameBS → do
            let name = TE.decodeUtf8 nameBS
            result ← Lua.liftIO $ do
                registry ← readIORef (textureNameRegistryRef env)
                return $ lookupTextureName name registry
            case result of
                Just (TextureHandle n) →
                    Lua.pushnumber (Lua.Number (fromIntegral n))
                Nothing →
                    Lua.pushnumber (-1)
            return 1
