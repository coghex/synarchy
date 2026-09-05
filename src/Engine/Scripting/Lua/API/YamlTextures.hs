{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.YamlTextures
    ( loadMaterialYamlFn
    , loadVegetationYamlFn
    , loadFloraYamlFn
    , loadAndRegister
    , loadAndRegisterWithPool
    , loadAndRegisterAtlasWithPool
    , isTextureNameRegistered
    , resolveTexturePath
    , getTextureHandleFn
    ) where

import UPrelude
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef', newIORef, IORef)
import System.Directory (doesFileExist)
import Engine.Core.State (EngineEnv, loggerRef
   )
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Log (LogCategory(..), logDebug, logError, logWarn)
import Engine.Scripting.Lua.API.YamlResult
    (pushYamlRefusal, pushYamlResult)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Asset.Handle (TextureHandle(..), AssetState(..))
import Engine.Asset.Types (AssetPool)
import Engine.Asset.Manager (generateTextureHandle, updateTextureState)
import Engine.Asset.TextureNameRegistry (lookupTextureName, registerTextureName)
import Engine.Graphics.Vulkan.Texture.Policy (UploadSampler(..))
import Engine.Asset.YamlMaterials
    (MaterialDef(..), loadMaterialYamlOutcome, materialPropsFromDef)
import Engine.Asset.YamlVegetation
    (VegetationDef(..), loadVegetationYamlOutcome)
import Engine.Asset.YamlFlora
import qualified Engine.Core.Queue as Q
import World.Flora.Types
import World.Flora.Growth (lifePhaseText, annualStageText)
import World.Material (MaterialId(..), registerMaterial, materialIdByName)

-- | If a yaml-declared texture path doesn't exist on disk, substitute the
--   given subset fallback so 'loadAndRegister' has something to queue,
--   instead of crashing at draw time (#478). Logged so missing assets are
--   visible during iteration. The fallback path itself isn't checked — if
--   you delete it too, you'll get the usual broken-texture behaviour.
resolveTexturePath ∷ EngineEnv → Text → FilePath → FilePath → IO FilePath
resolveTexturePath env label fallback preferred = do
    exists ← doesFileExist preferred
    if exists then return preferred else do
        logger ← readIORef (loggerRef env)
        logWarn logger CatAsset $
            label <> " texture missing: " <> T.pack preferred
            <> " — substituting " <> T.pack fallback
        return fallback

-- | Parse a material YAML, load all referenced textures (tile/zoom/bg),
--   register name-to-handle mappings, and queue load requests.
--   Returns number of textures queued.
loadMaterialYamlFn ∷ EngineEnv → LuaBackendState
                   → Lua.LuaE Lua.Exception Lua.NumResults
loadMaterialYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → pushYamlResult False 0
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            (parsed, count) ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                -- Parse the single YAML file
                mDefs ← loadMaterialYamlOutcome logger filePath
                let defs = fromMaybe [] mDefs

                -- For each MaterialDef, load 3 textures and register names
                let (lteq, _) = lbsMsgQueues backendState
                total ← foldM (\acc def → do
                    let name = mdName def
                        unknownMaterial = "assets/textures/utility/notexture.png"
                    tilePath ← resolveTexturePath env "Material tile"
                                   unknownMaterial (T.unpack (mdTile def))
                    zoomPath ← resolveTexturePath env "Material zoom"
                                   unknownMaterial (T.unpack (mdZoom def))
                    bgPath   ← resolveTexturePath env "Material background"
                                   unknownMaterial (T.unpack (mdBg def))
                    tileH ← loadAndRegister env backendState lteq UploadGlobalSampler
                                ("mat_tile_" <> name) tilePath
                    zoomH ← loadAndRegister env backendState lteq UploadGlobalSampler
                                ("mat_zoom_" <> name) zoomPath
                    bgH   ← loadAndRegister env backendState lteq UploadGlobalSampler
                                ("mat_bg_"   <> name) bgPath

                    -- Also register by numeric ID for world.setTexture
                    -- compatibility: "mat_tile_56" etc.
                    let idStr = tshow (mdId def)
                    let rv = toRenderViewCapability env
                    registerTextureName (rvTextureNameRegistryRef rv)
                        ("mat_tile_" <> idStr) tileH
                    registerTextureName (rvTextureNameRegistryRef rv)
                        ("mat_zoom_" <> idStr) zoomH
                    registerTextureName (rvTextureNameRegistryRef rv)
                        ("mat_bg_"   <> idStr) bgH

                    return (acc + 3)
                    ) (0 ∷ Int) defs

                -- Register material properties (name + physical params)
                -- in the world's MaterialRegistry. Without this the
                -- info-tool readout falls back to "unknown" for every
                -- tile, since the registry stays at defaults.
                atomicModifyIORef' (wsMaterialRegistryRef (toWorldSimCapability env)) $ \reg →
                    -- Through the SAME conversion
                    -- 'loadPopulatedMaterialRegistry' uses, over defs the
                    -- SAME 'loadMaterialYaml' already brought inside the
                    -- documented field domains (#1734): a custom material
                    -- registered from Lua cannot enter the registry
                    -- validated differently from a shipped one.
                    let reg' = foldl' (\r def →
                            registerMaterial (mdId def)
                                (materialPropsFromDef def) r
                            ) reg defs
                    in (reg', ())

                logDebug logger CatAsset $
                    "loadMaterialYaml: loaded " <> tshow total
                    <> " textures from " <> T.pack filePath
                return (isJust mDefs, total)

            pushYamlResult parsed count

-- | Parse a vegetation YAML, load variant textures as @veg_tile_\<vegId\>@,
--   and queue load requests. Returns number of textures queued.
loadVegetationYamlFn ∷ EngineEnv → LuaBackendState
                     → Lua.LuaE Lua.Exception Lua.NumResults
loadVegetationYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → pushYamlResult False 0
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            (parsed, count) ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                -- Parse the single vegetation YAML file
                mDefs ← loadVegetationYamlOutcome logger filePath
                let defs = fromMaybe [] mDefs

                -- For each VegetationDef, load 1 texture per variant
                let (lteq, _) = lbsMsgQueues backendState
                total ← foldM (\acc def → do
                    let baseId = vdIdStart def
                        variants = vdVariants def
                    varCount ← foldM (\vacc (idx, texPath) → do
                        let vegId = baseId + fromIntegral idx
                            regName = "veg_tile_" <> tshow vegId
                        resolved ← resolveTexturePath env "Vegetation variant"
                                       "assets/textures/utility/blanktexture.png"
                                       (T.unpack texPath)
                        _ ← loadAndRegister env backendState lteq UploadGlobalSampler
                                regName resolved
                        return (vacc + 1)
                        ) (0 ∷ Int) (zip [0..] variants)
                    return (acc + varCount)
                    ) (0 ∷ Int) defs

                logDebug logger CatAsset $
                    "loadVegetationYaml: loaded " <> tshow total
                    <> " textures from " <> T.pack filePath
                return (isJust mDefs, total)

            pushYamlResult parsed count

-- | Helper: generate a handle, register the name, queue the load
--   request under an EXPLICIT upload policy.
--
--   The policy is a parameter, not a constant, for the same reason
--   @engine.loadTexture@'s is (#2075, D-4): what a texture is FOR is
--   known at the declaring call site and nowhere else. Most YAML art is
--   world-drawn and passes 'UploadGlobalSampler'; the families whose
--   only consumer is a UI panel — a unit's authored portrait, an
--   equipment silhouette — pass 'UploadPinnedNearest'; and genuinely
--   dual-use art (an item sprite, a building sprite) is loaded TWICE,
--   once per policy, so the world quad and the inventory icon each get
--   the sampler they need.
loadAndRegister ∷ EngineEnv → LuaBackendState → Q.Queue LuaToEngineMsg
                → UploadSampler → Text → FilePath → IO TextureHandle
loadAndRegister env backendState =
    loadAndRegisterWithPool env (lbsAssetPool backendState)

-- | 'loadAndRegister' against an asset pool directly.
--
--   The 'LuaBackendState' is only ever consulted for its pool, so
--   taking the pool instead lets a caller outside the Lua thread — an
--   asset-loading test with a real 'EngineEnv' but no Lua state — drive
--   the same registration path.
loadAndRegisterWithPool ∷ EngineEnv → IORef AssetPool → Q.Queue LuaToEngineMsg
                        → UploadSampler → Text → FilePath → IO TextureHandle
loadAndRegisterWithPool env poolRef lteq samplerPolicy name path = do
    pool ← readIORef poolRef
    handle ← generateTextureHandle pool
    updateTextureState handle (AssetLoading path [] 0.0) pool
    -- Register name → handle
    registerTextureName (rvTextureNameRegistryRef (toRenderViewCapability env)) name handle
    -- Queue for actual GPU loading on the engine thread
    Q.writeQueue lteq (LuaLoadTextureRequest handle path samplerPolicy)
    return handle

-- | 'loadAndRegisterWithPool' for a compiled unit-animation atlas
--   (#1259).
--
--   Identical bookkeeping — ONE handle, ONE name, ONE queued upload per
--   animation (D-2\/D-10) — but kept as a distinct request so the
--   one-image-per-animation contract stays explicit. The engine registers
--   atlas slots on the player-selected global sampler (#2085); the one-texel
--   extrusion ring around every cell (#2076) keeps linear taps isolated,
--   and the preview browser's existing forced-global-nearest setting keeps
--   preview presentation pixel-crisp without a second loader path.
loadAndRegisterAtlasWithPool ∷ EngineEnv → IORef AssetPool
                             → Q.Queue LuaToEngineMsg
                             → Text → FilePath → IO TextureHandle
loadAndRegisterAtlasWithPool env poolRef lteq name path = do
    pool ← readIORef poolRef
    handle ← generateTextureHandle pool
    updateTextureState handle (AssetLoading path [] 0.0) pool
    registerTextureName (rvTextureNameRegistryRef (toRenderViewCapability env)) name handle
    Q.writeQueue lteq (LuaLoadAtlasTextureRequest handle path)
    return handle

-- | Has @name@ already been registered in the shared texture-name
--   registry? Lets a caller outside this module's capability
--   ('Engine.Scripting.Lua.API.Items.Defs', narrowed by #890) make a
--   register-once decision without dereferencing a @render-gpu-asset@
--   'EngineEnv' field of its own.
isTextureNameRegistered ∷ EngineEnv → Text → IO Bool
isTextureNameRegistered env name =
    isJust . lookupTextureName name
        <$> readIORef (rvTextureNameRegistryRef (toRenderViewCapability env))

-- | Parse a flora YAML: load textures, build species and world-gen entries,
--   insert into the FloraCatalog. Returns number of textures queued.
--
--   #2241: the authored @name@ is flora's stable species key — it keys
--   the placement salts, the per-plant identity and, after #2243, a
--   save's own references — so two species may not share one. The whole
--   file is PREFLIGHTED against that before any of it is registered
--   (see 'duplicateFloraNames'); a collision refuses the file entire and
--   answers through 'pushYamlRefusal', leaving the catalog, the id
--   allocator, the texture registry and the load queue untouched.
loadFloraYamlFn ∷ EngineEnv → LuaBackendState
                → Lua.LuaE Lua.Exception Lua.NumResults
loadFloraYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → pushYamlResult False 0
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            outcome ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                mDefs ← loadFloraYamlOutcome logger filePath
                let defs = fromMaybe [] mDefs

                let (lteq, _) = lbsMsgQueues backendState
                    catRef = wsFloraCatalogRef (toWorldSimCapability env)

                -- Preflight BEFORE anything observable happens.
                -- 'registerFloraSpecies' allocates an id and queues
                -- textures well before its catalog insert, so a refusal
                -- decided partway through would already have advanced
                -- fcNextId and registered texture names for the
                -- definitions ahead of the collision.
                existing ← readIORef catRef
                case duplicateFloraNames existing defs of
                    Just clash → do
                        logError logger CatAsset $
                            "loadFloraYaml: refused " <> T.pack filePath
                            <> " entirely: duplicate flora name '"
                            <> clash <> "' — the authored name is a "
                            <> "species' stable key and must be unique"
                        return (Left clash)
                    Nothing → do
                        total ← foldM (\acc def → do
                            texCount ← registerFloraSpecies env backendState lteq catRef def
                            return (acc + texCount)
                            ) (0 ∷ Int) defs

                        logDebug logger CatAsset $
                            "loadFloraYaml: loaded " <> tshow (length defs)
                            <> " species (" <> tshow total
                            <> " textures) from " <> T.pack filePath
                        return (Right (isJust mDefs, total))

            case outcome of
                Left clash          → pushYamlRefusal clash
                Right (parsed, cnt) → pushYamlResult parsed cnt

-- | The first authored name in @defs@ that cannot be admitted: one
--   already registered in @cat@, or one that appears twice within the
--   file itself. 'Nothing' when the whole file may be registered.
--
--   Both halves matter and neither implies the other: a file may
--   duplicate a shipped name without repeating itself, and a file may
--   repeat itself while colliding with nothing already loaded. Checked
--   in the file's own document order so the reported name is the first
--   one an author would find.
duplicateFloraNames ∷ FloraCatalog → [FloraYamlDef] → Maybe Text
duplicateFloraNames cat = go []
  where
    go _    []           = Nothing
    go seen (def : rest)
        | name `elem` seen                            = Just name
        | isJust (findSpeciesByName name cat)         = Just name
        | otherwise                                   = go (name : seen) rest
      where name = fydName def

unknownFloraTexture ∷ FilePath
unknownFloraTexture = "assets/textures/flora/unknown_flora.png"

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
    resolvedBase ← resolveTexturePath env "Flora base"
                       unknownFloraTexture baseTexPath
    baseH ← loadAndRegister env backendState lteq UploadGlobalSampler
                ("flora_base_" <> name) resolvedBase
    texCount ← newIORef (1 ∷ Int)

    -- Build lifecycle. Total over the four authored spellings, with no
    -- catch-all: an unrecognized one never reaches here because
    -- @Engine.Asset.YamlFlora.requireLifecycle@ refuses the whole file
    -- at decode time (#2315), and 'FloraLifecycle' has no fifth
    -- constructor for one to arrive as.
    let lifecycle = case fydLifecycle def of
            LifecyclePerennial → Perennial
                (maybe 1800.0 id (fydMinLife def))
                (maybe 3600.0 id (fydMaxLife def))
                (maybe 0.2 id (fydDeathChance def))
            LifecycleAnnual    → Annual
            LifecycleBiennial  → Biennial
            LifecycleEvergreen → Evergreen

    -- Build life phases (load a texture for each). Every tag arrives
    -- already parsed and every one is registered: the unrecognized
    -- token that used to be dropped here without a word now refuses the
    -- whole file at decode time (#2315). The texture-registry name is
    -- rendered back through 'lifePhaseText', the exact inverse of the
    -- parser the author's token went through, so the registered names
    -- are byte-for-byte the ones they were.
    phases ← foldM (\phaseMap yp → do
        let tag  = fypTag yp
            path = texDir <> "/" <> T.unpack (fypTexture yp)
        resolved ← resolveTexturePath env "Flora phase"
                       unknownFloraTexture path
        h ← loadAndRegister env backendState lteq UploadGlobalSampler
                ("flora_phase_" <> name <> "_" <> lifePhaseText tag) resolved
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
        let tag  = fycsTag ycs
            path = texDir <> "/" <> T.unpack (fycsTexture ycs)
        resolved ← resolveTexturePath env "Flora annual-cycle stage"
                       unknownFloraTexture path
        h ← loadAndRegister env backendState lteq UploadGlobalSampler
                ("flora_cycle_" <> name <> "_" <> annualStageText tag) resolved
        atomicModifyIORef' texCount (\n → (n + 1, ()))
        let stage = AnnualStage
                { asTag      = tag
                , asStartDay = fycsStartDay ycs
                , asTexture  = h
                }
        return (stages ++ [stage])
        ) [] (fydAnnualCycle def)

    -- Build cycle overrides. The decoder has already required each pair
    -- to name a phase and a stage THIS species declares, so every one
    -- registered here is reachable by 'AnnualCycleKey' lookup.
    overrides ← foldM (\ovMap yco → do
        let pTag = fycoPhase yco
            cTag = fycoCycle yco
            path = texDir <> "/" <> T.unpack (fycoTexture yco)
        resolved ← resolveTexturePath env "Flora cycle override"
                       unknownFloraTexture path
        h ← loadAndRegister env backendState lteq UploadGlobalSampler
                ("flora_ov_" <> name <> "_" <> lifePhaseText pTag
                 <> "_" <> annualStageText cTag) resolved
        atomicModifyIORef' texCount (\n → (n + 1, ()))
        return (HM.insert (AnnualCycleKey pTag cTag) h ovMap)
        ) HM.empty (fydCycleOverrides def)

    -- Build the harvestable block (#94), loading the depleted texture
    -- when the YAML names one (a berry bush with the fruit stripped).
    harvest ← case fydHarvest def of
        Nothing → return Nothing
        Just yh → do
            depletedH ← case fyhHarvestedTexture yh of
                Nothing → return (TextureHandle 0)
                Just tex → do
                    let path = texDir <> "/" <> T.unpack tex
                    resolved ← resolveTexturePath env "Flora harvested"
                                   unknownFloraTexture path
                    h ← loadAndRegister env backendState lteq UploadGlobalSampler
                            ("flora_harvested_" <> name) resolved
                    atomicModifyIORef' texCount (\n → (n + 1, ()))
                    return h
            -- #2212: the two authored policy fields carry straight
            -- across. The decoder has already required every ungated
            -- tag to be one this block declares and every phase_yield
            -- key to name a phase this species enters, so nothing
            -- registered here is unreachable.
            let yieldsOf ys = [ (fyyId y, fyyMin y, fyyMax y) | y ← ys ]
            return $ Just FloraHarvest
                { fhTags             = fyhTags yh
                , fhUngatedTags      = fyhUngatedTags yh
                , fhYield            = yieldsOf (fyhYield yh)
                , fhPhaseYields      = HM.map yieldsOf (fyhPhaseYield yh)
                , fhRegrowth         = fyhRegrowthTime yh
                , fhHarvestedTexture = depletedH
                }

    -- Assemble the FloraSpecies
    let species = FloraSpecies
            { fsName           = name
            , fsBaseTexture    = baseH
            , fsLifecycle      = lifecycle
            , fsPhases         = phases
            , fsAnnualCycle    = cycleStages
            , fsCycleOverrides = overrides
            , fsHarvest        = harvest
            }

    -- Insert species into catalog
    atomicModifyIORef' catRef $ \cat →
        (insertSpecies fid species cat, ())

    -- Resolve soil preference NAMES (data/materials/*.yaml's `name`
    -- field) to raw material ids via the already-loaded material
    -- registry (data/materials loads before data/flora, see
    -- scripts/startup_loader.lua) — same "Text in YAML, resolved once
    -- at content-load time" idiom, just eager instead of
    -- mpDigSpoil/mpDigChunk's lazy-at-use-time resolution (this feeds
    -- FloraWorldGen, queried every world-gen fitness check, so
    -- resolving once here avoids a registry lookup per tile).
    registry ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
    logger ← readIORef (loggerRef env)
    let wg = fydWorldGen def
        soilIds =
            [ unMaterialId mid
            | soilName ← fywSoils wg
            , Just mid ← [materialIdByName registry soilName]
            ]
        unresolvedSoils =
            [ soilName | soilName ← fywSoils wg
            , materialIdByName registry soilName ≡ Nothing ]
    mapM_ (\soilName → logWarn logger CatAsset $
              "Flora '" <> name <> "' soils entry '" <> soilName
              <> "' does not match any registered material name")
          unresolvedSoils

    -- Build and insert FloraWorldGen
    let minAlt   = maybe (-100) id (fywMinAlt wg)
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
            , fwSoils         = soilIds
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
            let name = TE.decodeUtf8Lenient nameBS
            result ← Lua.liftIO $ do
                registry ← readIORef (rvTextureNameRegistryRef (toRenderViewCapability env))
                return $ lookupTextureName name registry
            case result of
                Just (TextureHandle n) →
                    Lua.pushnumber (Lua.Number (fromIntegral n))
                Nothing →
                    Lua.pushnumber (-1)
            return 1
