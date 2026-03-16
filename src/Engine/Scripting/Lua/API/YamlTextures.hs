{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.YamlTextures
    ( loadMaterialYamlFn
    , loadVegetationYamlFn
    , loadFloraYamlFn
    , loadAndRegister
    , getTextureHandleFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef', newIORef, IORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo, logDebug, logWarn)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Asset.Handle (TextureHandle(..), AssetState(..))
import Engine.Asset.Manager (generateTextureHandle, updateTextureState)
import Engine.Asset.YamlTextures
import Engine.Asset.YamlFlora
import qualified Engine.Core.Queue as Q
import World.Flora.Types

-- | Parse a material YAML, load all referenced textures (tile/zoom/bg),
--   register name-to-handle mappings, and queue load requests.
--   Returns number of textures queued.
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

-- | Parse a vegetation YAML, load variant textures as @veg_tile_\<vegId\>@,
--   and queue load requests. Returns number of textures queued.
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

-- | Parse a flora YAML: load textures, build species and world-gen entries,
--   insert into the FloraCatalog. Returns number of textures queued.
loadFloraYamlFn ∷ EngineEnv → LuaBackendState
                → Lua.LuaE Lua.Exception Lua.NumResults
loadFloraYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadFloraYaml logger filePath

                let (lteq, _) = lbsMsgQueues backendState
                    catRef = floraCatalogRef env

                total ← foldM (\acc def → do
                    texCount ← registerFloraSpecies env backendState lteq catRef def
                    return (acc + texCount)
                    ) (0 ∷ Int) defs

                logInfo logger CatAsset $
                    "loadFloraYaml: loaded " <> T.pack (show (length defs))
                    <> " species (" <> T.pack (show total)
                    <> " textures) from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

registerFloraSpecies ∷ EngineEnv → LuaBackendState → Q.Queue LuaToEngineMsg
                     → IORef FloraCatalog → FloraYamlDef → IO Int
registerFloraSpecies env backendState lteq catRef def = do
    let texDir = T.unpack (fydTexDir def)
        name   = fydName def

    -- Allocate a FloraId
    fid ← atomicModifyIORef' catRef $ \cat →
        let (newId, cat') = nextFloraId cat
        in (cat', newId)

    -- Determine the "base" texture: first phase's texture, or matured.png
    let baseTexPath = case fydPhases def of
            (p:_) → texDir <> "/" <> T.unpack (fypTexture p)
            []    → texDir <> "/matured.png"

    -- Load base texture
    baseH ← loadAndRegister env backendState lteq
                ("flora_base_" <> name) baseTexPath
    texCount ← newIORef (1 ∷ Int)

    -- Build lifecycle
    let lifecycle = case fydLifecycle def of
            "perennial" → Perennial
                (maybe 1800.0 id (fydMinLife def))
                (maybe 3600.0 id (fydMaxLife def))
                (maybe 0.2 id (fydDeathChance def))
            "annual"   → Annual
            "biennial" → Biennial
            _          → Evergreen

    -- Build life phases (load a texture for each)
    phases ← foldM (\phaseMap yp → do
        case parsePhaseTag (fypTag yp) of
            Nothing → return phaseMap
            Just tag → do
                let path = texDir <> "/" <> T.unpack (fypTexture yp)
                h ← loadAndRegister env backendState lteq
                        ("flora_phase_" <> name <> "_" <> fypTag yp) path
                atomicModifyIORef' texCount (\n → (n + 1, ()))
                let phase = LifePhase
                        { lpTag     = tag
                        , lpAge     = fypAge yp
                        , lpTexture = h
                        }
                return (HM.insert tag phase phaseMap)
        ) HM.empty (fydPhases def)

    -- Build annual cycle stages
    cycleStages ← foldM (\stages ycs → do
        case parseCycleTag (fycsTag ycs) of
            Nothing → return stages
            Just tag → do
                let path = texDir <> "/" <> T.unpack (fycsTexture ycs)
                h ← loadAndRegister env backendState lteq
                        ("flora_cycle_" <> name <> "_" <> fycsTag ycs) path
                atomicModifyIORef' texCount (\n → (n + 1, ()))
                let stage = AnnualStage
                        { asTag      = tag
                        , asStartDay = fycsStartDay ycs
                        , asTexture  = h
                        }
                return (stages ++ [stage])
        ) [] (fydAnnualCycle def)

    -- Build cycle overrides
    overrides ← foldM (\ovMap yco → do
        case (parsePhaseTag (fycoPhase yco), parseCycleTag (fycoCycle yco)) of
            (Just pTag, Just cTag) → do
                let path = texDir <> "/" <> T.unpack (fycoTexture yco)
                h ← loadAndRegister env backendState lteq
                        ("flora_ov_" <> name <> "_" <> fycoPhase yco
                         <> "_" <> fycoCycle yco) path
                atomicModifyIORef' texCount (\n → (n + 1, ()))
                return (HM.insert (AnnualCycleKey pTag cTag) h ovMap)
            _ → return ovMap
        ) HM.empty (fydCycleOverrides def)

    -- Assemble the FloraSpecies
    let species = FloraSpecies
            { fsName           = name
            , fsBaseTexture    = baseH
            , fsLifecycle      = lifecycle
            , fsPhases         = phases
            , fsAnnualCycle    = cycleStages
            , fsCycleOverrides = overrides
            }

    -- Insert species into catalog
    atomicModifyIORef' catRef $ \cat →
        (insertSpecies fid species cat, ())

    -- Build and insert FloraWorldGen
    let wg = fydWorldGen def
        minAlt   = maybe (-100) id (fywMinAlt wg)
        maxAlt   = maybe 800    id (fywMaxAlt wg)
        idealAlt = maybe ((minAlt + maxAlt) `div` 2) id (fywIdealAlt wg)
        minHum   = maybe 0.0 id (fywMinHumidity wg)
        maxHum   = maybe 1.0 id (fywMaxHumidity wg)
        idealHum = maybe ((minHum + maxHum) / 2.0) id (fywIdealHumidity wg)
        floraWG = FloraWorldGen
            { fwCategory      = fywCategory wg
            , fwMinTemp       = fywMinTemp wg
            , fwMaxTemp       = fywMaxTemp wg
            , fwIdealTemp     = fywIdealTemp wg
            , fwMinPrecip     = fywMinPrecip wg
            , fwMaxPrecip     = fywMaxPrecip wg
            , fwIdealPrecip   = fywIdealPrecip wg
            , fwMinAlt        = minAlt
            , fwMaxAlt        = maxAlt
            , fwIdealAlt      = idealAlt
            , fwMinHumidity   = minHum
            , fwMaxHumidity   = maxHum
            , fwIdealHumidity = idealHum
            , fwMaxSlope      = maybe 15 fromIntegral (fywMaxSlope wg)
            , fwDensity       = maybe 0.1 id (fywDensity wg)
            , fwSoils         = []
            , fwFootprint     = maybe 0.0 id (fywFootprint wg)
            }

    atomicModifyIORef' catRef $ \cat →
        (insertWorldGen fid floraWG cat, ())

    readIORef texCount

-- | Look up a texture handle by registered name. Returns -1 if not found.
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
