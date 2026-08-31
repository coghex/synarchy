module World.Thread.Command.Init
    ( handleWorldInitCommand
    , handleWorldInitArenaCommand
    , handleWorldInitArenaDoneCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import World.Blood.Teardown (enqueueBloodDisposalForPage)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Engine.Asset.YamlMaterials (loadPopulatedMaterialRegistry)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.InputView
    (InputViewCapability(..), toInputViewCapability)
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (logInfo, logDebug, logWarn, LogCategory(..), LoggerState)
import Engine.Graphics.Solar (maxSolarPages)
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Types
import Structure.Types (emptyChunkStructures)
import World.Generate (generateChunk)
import World.Generate.Arena (generateArenaChunks, arenaGenForSeed)
import World.Chunk.Queue (initialChunkQueue)
import World.Chunk.Residency (canonicalChunkCoord)
import World.Chunk.Admit (registerChunkDemand, seedResidentChunks)
import World.Geology (buildTimeline)
import World.Geology.Log (formatPlatesSummary)
import World.Plate (generatePlates, elevationAtGlobal)
import Language.Generated.Types (generatorErrorText)
import Language.Semantic.Catalogue ( conceptCataloguePath
                                   , conceptOrdinalPath, loadCatalogue )
import Language.Semantic.Types (catalogueErrorText)
import Location.Types (allLocations)
import Location.Instance
    (buildLocationInstancesWithSeed, emptyLocationInstances
    , locationGeometryErrorText)
import Language.Naming (Namer, mkNamer)
import World.River.Identity (timelineRiverFeatureIds)
import World.River.Naming (buildRiverNames)
import Location.Overlay ( computeLocationPlacement, LocationPlacement(..)
                        , PlacementOutcome(..) )
import Location.Overlay.Types (emptyLocationOverlay)
import World.Preview (buildPreviewFromPixels, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap.Cache (buildZoomCacheWithPixels)
import World.ZoomMap.ColorPalette (buildColorPalette)
import World.ZoomMap.ChunkTexture (buildZoomAtlas, ZoomAtlasData(..))
import World.Weather (initEarlyClimate, formatWeather)
import World.Weather.Types (ClimateState(..))
import World.Generate.Config (WorldGenConfig(..)
                              , ResourcesYaml(..)
                              , applyConfigToParams
                              , timelineParamsOf
                              , minimumWorldSize
                              , normalizeWorldGenInputs)
import World.Geology.Ore.Types (OreLevers(..))
import World.Thread.Helpers (sendGenLog)
import World.Thread.ChunkLoading (dispatchLocationStamps)

handleWorldInitCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Word64 → Int → Int → Maybe WorldIdentity → IO ()
handleWorldInitCommand env logger pageId seed rawWorldSize rawPlaceCount
                       identity = do
    let worldSim = toWorldSimCapability env
        handoff  = toRenderHandoffCapability env
        (worldSize, placeCount) =
            normalizeWorldGenInputs rawWorldSize rawPlaceCount
    when (worldSize ≢ rawWorldSize ∨ placeCount ≢ rawPlaceCount) $ do
        let msg = "Normalized worldgen inputs: worldSize "
                <> tshow rawWorldSize <> " → "
                <> tshow worldSize <> ", plateCount "
                <> tshow rawPlaceCount <> " → "
                <> tshow placeCount
                <> " (worldSize minimum/multiple "
                <> tshow minimumWorldSize
                <> ", plateCount min 1)."
        logWarn logger CatWorld msg
        sendGenLog env msg
    logDebug logger CatWorld $ "Initializing world: " <> unWorldPageId pageId
        <> " (seed=" <> tshow seed
        <> ", size=" <> tshow worldSize
        <> ", places=" <> tshow placeCount <> ")"
    
    sendGenLog env "Initializing world state..."
    
    worldState ← emptyWorldState
    let phaseRef = wsLoadPhaseRef worldState
        totalSteps = 8

    -- Player-facing identity (#707): recorded once at creation, before
    -- the page is even registered — it never changes afterward. Nothing
    -- for every caller that doesn't name its world (arena, headless,
    -- dump, the 4-argument world.init).
    writeIORef (wsIdentityRef worldState) identity

    -- Re-initialising an existing page id replaces (and orphans) its old
    -- WorldState; reclaim that old page's blood-texture GPU resources
    -- (#788) before it drops out of wmWorlds below. No-op the common case
    -- where no page yet exists under this id.
    do preMgr ← readIORef (wsWorldManagerRef worldSim)
       enqueueBloodDisposalForPage (rhBloodDisposeQueue handoff) preMgr pageId

    -- register early so lua can read the loading phase
    atomicModifyIORef' (wsWorldManagerRef worldSim) $ \mgr →
        -- Dedup by page id: re-initialising an existing page (the common
        -- "main_world" reuse after Exit to Menu) must REPLACE its entry,
        -- not stack a second one in wmWorlds (#58).
        --
        -- #1602: replacing the visible HEAD's WorldState under the SAME
        -- id is a selection change even though wmVisible is untouched —
        -- a binding captured against the old page would otherwise keep
        -- matching, and a placement validated against the old terrain
        -- would commit into the replacement. Replacing a hidden page, a
        -- visible-but-not-head one, or registering a new id invalidates
        -- nothing: a binding only ever names the head. The request is
        -- discharged either way.
        ((if selectionHead (wmVisible mgr) ≡ Just pageId
            then bumpSelectionGen else id)
            (completeSelectionChange mgr)
            { wmWorlds = (pageId, worldState)
                       : filter ((≢ pageId) . fst) (wmWorlds mgr) }, ())

    -- Step 0.5: Populate the material registry from data/materials/*.yaml.
    -- The registry was initialized empty at engine startup; without this
    -- pass every material would use defaultMaterialProps (uniform
    -- hardness/density/drainage), making per-material differentiation
    -- in erosion / water-table / etc. a no-op. Idempotent — reloading on
    -- successive world inits just rewrites the same data.
    -- Shared with the whole-session LOAD path (issue #763) via
    -- 'Engine.Asset.YamlMaterials.loadPopulatedMaterialRegistry' — a
    -- headless boot that goes straight to engine.loadSave with no prior
    -- world.init in the same process needs this SAME population before
    -- it can validate a save's material references.
    sendGenLog env "Loading material registry from data/materials..."
    populatedReg ← loadPopulatedMaterialRegistry logger "data/materials"
    writeIORef (wsMaterialRegistryRef worldSim) populatedReg

    -- Step 1: Timeline (now co-evolves climate)
    writeIORef phaseRef (LoadPhase1 1 totalSteps)
    sendGenLog env "Building geological timeline..."
    worldGenCfg0 ← readIORef (wsWorldGenConfigRef worldSim)
    let erosionIntensity = wgcErosionIntensity worldGenCfg0
        volcanicActivity = wgcVolcanicActivity worldGenCfg0
        lavaPoolDepth    = wgcLavaPoolDepth worldGenCfg0
        lavaPoolRadius   = wgcLavaPoolRadius worldGenCfg0
        waterfallQuantum = wgcWaterfallQuantum worldGenCfg0
        resourcesCfg     = wgcResources worldGenCfg0
        oreLevers        = OreLevers
            { olGlobal = ryOreAbundance resourcesCfg
            , olIron   = ryIronAbundance resourcesCfg
            , olCopper = ryCopperAbundance resourcesCfg
            }
    let (timeline, timelineClimate, borderedCache, oceanMap, oceanDist) = buildTimeline populatedReg seed worldSize placeCount erosionIntensity volcanicActivity lavaPoolDepth lavaPoolRadius waterfallQuantum oreLevers (timelineParamsOf worldGenCfg0)
    _ ← evaluate (force timeline)
    _ ← evaluate (force timelineClimate)
    _ ← evaluate (force borderedCache)
    registry ← readIORef (wsMaterialRegistryRef worldSim)
    let !_ = registry `seq` ()  -- ensure registry is read before logging timeline info
    let plateLines = formatPlatesSummary seed worldSize placeCount registry
    forM_ plateLines $ \line → do
        logInfo logger CatWorld line
        sendGenLog env line

    -- Step 2: Ocean map — reuse the map buildTimeline already
    -- computed (and that the lake/seabed passes used), so every
    -- consumer shares ONE chunk-level ocean classification.
    writeIORef phaseRef (LoadPhase1 2 totalSteps)
    let plates = generatePlates seed worldSize placeCount
    _ ← evaluate (force plates)
    sendGenLog env $ "Ocean flood fill: "
        <> tshow (HS.size oceanMap) <> " ocean chunks"

    -- Step 3: Climate — refine the timeline's co-evolved climate
    --   with the precise chunk-resolution ocean map. The timeline's own
    --   final CO2/solar constant are threaded through so the rebuilt
    --   regional grid and its csGlobalCO2/csGlobalTemp/csSolarConst
    --   summary fields all come from ONE coherent forcing pass, rather
    --   than a baseline-forcing grid with evolved summary fields
    --   patched on afterward (#785).
    writeIORef phaseRef (LoadPhase1 3 totalSteps)
    sendGenLog env "Refining climate with ocean data..."
    let climateState' = initEarlyClimate worldSize oceanMap timeline
            (csGlobalCO2 timelineClimate) (csSolarConst timelineClimate)
    _ ← evaluate (force climateState')

    let weatherLines = formatWeather climateState'
    forM_ weatherLines $ \line → do
        logInfo logger CatWorld line
        sendGenLog env line

    floraCat ← readIORef (wsFloraCatalogRef worldSim)
    logInfo logger CatWorld $ "Flora catalog snapshot: "
        <> tshow (HM.size (fcSpecies floraCat)) <> " species, "
        <> tshow (HM.size (fcWorldGen floraCat)) <> " worldgen entries"

    -- Use world gen config (already read for erosion intensity).
    -- 'withVolcanoCtx' populates the Magma context now that
    -- gtFeatures is final, so chunk-gen sees a built spatial index.
    let baseParams = applyConfigToParams worldGenCfg0
        params0 = withVolcanoCtx $ baseParams
            { wgpSeed        = seed
            , wgpWorldSize   = worldSize
            , wgpPlateCount  = placeCount
            , wgpPlates      = plates
            , wgpGeoTimeline = timeline
            , wgpOceanMap    = oceanMap
            , wgpOceanDist   = oceanDist
            , wgpClimateState = climateState'
            }

    -- Location overlay (#89): deterministically choose which chunks
    -- host the registered locations, from the just-finalised plates +
    -- ocean + lake/river data (locations keep clear of water, #414).
    -- Empty when no defs are loaded — the common headless-dump path
    -- stays byte-identical, and the placement and settlement work is
    -- skipped. The land detection is not: 'Location.Overlay' is
    -- {-# LANGUAGE Strict #-}, so its per-chunk scan is forced on every
    -- path, which is what lets a landless world report NoLand even
    -- there (#1414).
    locRegistry ← readIORef (crLocationDefsRef (toContentRegistriesCapability env))
    -- #1101/#1102: this page's placed locations AND its rivers are both
    -- named in THIS page's own generated language, resolved from the
    -- identity's #1092 provenance recorded a few lines above. One namer
    -- serves both, which is what makes a root recur across them. A page
    -- with no provenance (a custom-named world, an unnamed one)
    -- genuinely has no language and gets 'Nothing' — its locations keep
    -- their definition labels and its rivers stay unnamed.
    namer ← resolvePageNamer logger identity
    let locDefs = allLocations locRegistry
        placement = computeLocationPlacement seed worldSize plates oceanMap oceanDist
                      (gtWorldLakes timeline) (gtWorldRivers timeline) locDefs
        placedOverlay = lpOverlay placement
    -- Instance ids (#911) are allocated HERE, at placement time, from
    -- the deterministic overlay's canonical order — not at stamp time —
    -- so an id is stable across save/load and across chunk
    -- eviction/reload. Names (#1101) are rendered from those same ids,
    -- once, and never re-derived.
    --
    -- #1796: that construction is CHECKED, and its failure is reported
    -- rather than swallowed. There is no world-init failure LoadPhase
    -- and this handler has no failure return, so an unrepresentable
    -- placement takes the same degraded-but-continue shape the #997
    -- outcomes below already use — a loud warning on both the log and
    -- the generation feed — and drops BOTH the overlay and the instance
    -- table together, so no wrapped or inverted box is ever written to
    -- wsGenParamsRef and the two never disagree about what was placed.
    (overlay, instances) ← case buildLocationInstancesWithSeed
                                      (wgpSeed params0) namer locRegistry
                                      placedOverlay of
        Right built → pure (placedOverlay, built)
        Left err → do
            let msg = "Placed-location geometry is not representable — \
                      \placing no locations (#1796): "
                      <> locationGeometryErrorText err
            logWarn logger CatWorld msg
            sendGenLog env msg
            pure (emptyLocationOverlay, emptyLocationInstances)
    let params = params0
            { wgpLocationOverlay   = overlay
            , wgpLocationInstances = instances
            -- River names (#1102) are rendered from the ids the
            -- timeline already allocated, once, and never re-derived.
            -- The table is empty without a language; the ids stay
            -- available either way.
            , wgpRiverNames =
                buildRiverNames namer (timelineRiverFeatureIds timeline)
            }
    _ ← evaluate (force (wgpLocationOverlay params))
    _ ← evaluate (force (wgpLocationInstances params))
    _ ← evaluate (force (wgpRiverNames params))
    -- #997: a world with no locations at all makes the expedition arc
    -- unplayable on that save, so the two interesting outcomes are
    -- reported rather than passing silently. NoLand is the explicit
    -- no-location result — Create World surfaces it to the player from
    -- the (necessarily) empty placed-location list at LoadDone.
    case lpOutcome placement of
        PlacedGuaranteed → do
            let msg = "Strict location placement found no suitable chunk; \
                      \placed one guaranteed location (#997)."
            logWarn logger CatWorld msg
            sendGenLog env msg
        NoLand → do
            let msg = "World contains no land — no locations placed (#997)."
            logWarn logger CatWorld msg
            sendGenLog env msg
        _ → pure ()

    writeIORef (wsGenParamsRef worldState) (Just params)
    
    -- Step 4: Zoom cache + texture atlas
    writeIORef phaseRef (LoadPhase1 4 totalSteps)
    sendGenLog env "Building zoom color palette..."
    palette ← buildColorPalette logger "data/materials" "data/vegetation"
    _ ← evaluate (force palette)

    sendGenLog env "Building zoom cache with per-chunk textures..."
    let (zoomCache, chunkPixels) =
            buildZoomCacheWithPixels params registry palette
                                     (Just borderedCache)
    _ ← evaluate (force zoomCache)
    _ ← evaluate (force chunkPixels)
    writeIORef (wsZoomCacheRef worldState) zoomCache

    sendGenLog env "Assembling zoom texture atlas..."
    let atlas = buildZoomAtlas (V.length zoomCache) chunkPixels
    _ ← evaluate (force atlas)
    -- Issue #763: pair the atlas with the EXACT
    -- WorldState it belongs to (this init's own page), mirroring
    -- World.Load.Publish's identical fix -- see EngineEnv.zoomAtlasDataRef.
    writeIORef (rhZoomAtlasDataRef handoff) $
        Just (zadWidth atlas, zadHeight atlas, zadPixelData atlas, [worldState])
    -- Store atlas metadata (chunksPerRow) for UV computation during baking
    writeIORef (wsZoomAtlasRef worldState) Nothing  -- will be filled after GPU upload
    -- Store chunksPerRow for later use
    logInfo logger CatWorld $ "Zoom atlas: "
        <> tshow (zadWidth atlas) <> "×"
        <> tshow (zadHeight atlas) <> " ("
        <> tshow (V.length zoomCache) <> " chunks)"
    
    -- Step 5: Preview
    writeIORef phaseRef (LoadPhase1 5 totalSteps)
    sendGenLog env "Rendering world preview..."
    let preview = buildPreviewFromPixels params zoomCache chunkPixels
    _ ← evaluate (force preview)
    -- Stamp with a fresh generation (see
    -- Engine.Core.State.worldPreviewGenerationRef / World.Load.Publish).
    previewGen ← atomicModifyIORef' (rhWorldPreviewGenerationRef handoff)
                    (\g → (g + 1, g + 1))
    writeIORef (rhWorldPreviewRef handoff) $
        Just (piWidth preview, piHeight preview, piData preview, previewGen)
    sendGenLog env "World preview ready."
    
    -- Step 6: Center chunk
    writeIORef phaseRef (LoadPhase1 6 totalSteps)
    -- The load-radius box around the centre, counted as PHYSICAL
    -- chunks: on a small world the box aliases against itself across
    -- the seam, and this total is what LoadPhase2 progresses towards
    -- (#1723). The centre is generated synchronously just below and so
    -- is excluded from the queue but counted in the total.
    let centerCoord = ChunkCoord 0 0
        (remainingCoords, totalInitialChunks) =
            initialChunkQueue (canonicalChunkCoord params) centerCoord
    sendGenLog env $ "Generating initial chunks ("
        <> tshow totalInitialChunks <> ")..."
    
    catalog ← readIORef (wsFloraCatalogRef worldSim)
    let (ct, cs, cterrain, cf, cice, cflora, cwt, cmagma) =
            generateChunk registry catalog params centerCoord
        seededSurf = VU.imap (\idx surfZ →
            case cf V.! idx of
                Just fc → max surfZ (fcSurface fc)
                Nothing → surfZ
            ) cs
        centerChunk = LoadedChunk
            { lcCoord      = centerCoord
            , lcTiles      = ct
            , lcSurfaceMap = seededSurf
            , lcTerrainSurfaceMap = cterrain
            , lcFluidMap   = cf
            , lcIceMap     = cice
            , lcFlora      = cflora
            , lcSideDeco   = VU.replicate (chunkSize * chunkSize) 0
            , lcWaterTableMap = cwt
            , lcMagma      = cmagma
            , lcStructures = emptyChunkStructures
            }

    atomicModifyIORef' (wsTilesRef worldState) $ \_ →
        (WorldTileData { wtdChunks = HM.singleton centerCoord centerChunk
                       , wtdMaxChunks = 200 }, ())
    -- The centre is new residency like any other chunk (#2001), so it
    -- reaches the owner through the SAME admission boundary the camera
    -- and init-queue batches use — on a brand-new page whose owner is
    -- empty, hence the claim-then-admit seed form.
    seedResidentChunks worldState pageId params [centerCoord]

    -- Stamp any placed location on the synchronously-generated centre
    -- chunk (#89). It is written straight to wsTilesRef and excluded from
    -- the init queue, so the chunk-loading dispatch never sees it.
    dispatchLocationStamps env params pageId [centerChunk]

    -- Step 7: Queue remaining chunks
    writeIORef phaseRef (LoadPhase1 7 totalSteps)
    -- Register the initial box as durable demand, then APPEND exactly
    -- what that call says still needs scheduling (#2001).
    --
    -- Appending rather than replacing is load-bearing. This page was
    -- registered in wmWorlds near the top of this function so Lua could
    -- watch the loading phase, and its generation params went in before
    -- the expensive cache/preview work above — so a world.loadChunksInRegion
    -- can be accepted, counted and registered on the owner during that
    -- window. A wholesale write would drop its coords from the queue
    -- while leaving them requested on the owner, which deduplicates
    -- every later request for them: the region would be reported as
    -- queued and then never load, unrepairably.
    needed ← registerChunkDemand worldState pageId params remainingCoords
    queuedNow ← atomicModifyIORef' (wsInitQueueRef worldState) $ \q →
        let q' = q ⧺ needed in (q', length q')
    
    -- Now switch to Phase 2 tracking. The remaining count is the queue's
    -- real length, which an accepted concurrent request makes larger than
    -- this page's own box; drainInitQueues recomputes it every tick from
    -- the same source, so the two never disagree.
    writeIORef phaseRef (LoadPhase2 queuedNow totalInitialChunks)
    
    sendGenLog env "Calculating surface elevation..."
    let (surfaceElev, _mat) = elevationAtGlobal seed (wgpPlates params)
                                                worldSize 0 0
        startZSlice = surfaceElev + surfaceHeadroom
    atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
        (cam { camZSlice = startZSlice, camZTracking = True }, ())
    
    sendGenLog env $ "World initialized: "
        <> tshow totalInitialChunks <> " chunks queued"
    
    logInfo logger CatWorld $ "World initialized: "
        <> tshow totalInitialChunks <> " chunks, "
        <> "surface at z=" <> tshow surfaceElev
        <> ": " <> unWorldPageId pageId

-- | The namer this page's generated names are rendered through — its
--   placed locations (#1101) and its rivers (#1102) — or 'Nothing' when
--   the page has no language to name them in.
--
--   'Nothing' is the ordinary, expected result for every page whose
--   identity carries no #1092 provenance — a custom-named world, an
--   unnamed one, an arena — and is silent. The two ways a page that
--   DOES declare a language can still end up unnamed are both logged:
--   a catalogue that will not load, and a provenance naming a generator
--   version this build cannot construct. Neither substitutes another
--   language; the locations fall back to their definition labels and
--   the rivers to no name at all, which is what "this world has no
--   language" already means everywhere else.
resolvePageNamer
    ∷ LoggerState → Maybe WorldIdentity → IO (Maybe Namer)
resolvePageNamer logger identity = case wiLanguage =≪ identity of
    Nothing   → pure Nothing
    Just prov → do
        eCat ← loadCatalogue conceptCataloguePath conceptOrdinalPath
        case eCat of
            Left cErr → do
                logWarn logger CatWorld $
                    "Name generation disabled for this world: concept "
                    <> "catalogue " <> T.pack conceptCataloguePath
                    <> " could not be loaded: " <> catalogueErrorText cErr
                pure Nothing
            Right cat → case mkNamer cat prov of
                Left gErr → do
                    logWarn logger CatWorld $
                        "Name generation disabled for this world: "
                        <> generatorErrorText gErr
                    pure Nothing
                Right namer → pure (Just namer)

handleWorldInitArenaCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldInitArenaCommand env logger pageId = do
    let worldSim = toWorldSimCapability env
        handoff  = toRenderHandoffCapability env
    logInfo logger CatWorld $ "Initializing test arena: " <> unWorldPageId pageId

    worldState ← emptyWorldState

    -- Replacing an existing page id orphans its old WorldState; reclaim
    -- its blood-texture GPU resources (#788) before it drops out below.
    do preMgr ← readIORef (wsWorldManagerRef worldSim)
       enqueueBloodDisposalForPage (rhBloodDisposeQueue handoff) preMgr pageId

    -- Register early so textures sent after this command are routed correctly
    atomicModifyIORef' (wsWorldManagerRef worldSim) $ \mgr →
        -- Dedup by page id: re-initialising an existing page (the common
        -- "main_world" reuse after Exit to Menu) must REPLACE its entry,
        -- not stack a second one in wmWorlds (#58).
        -- #1602: as in handleWorldInitCommand — replacing the visible
        -- HEAD is a selection change; the request is discharged either
        -- way.
        ((if selectionHead (wmVisible mgr) ≡ Just pageId
            then bumpSelectionGen else id)
            (completeSelectionChange mgr)
            { wmWorlds = (pageId, worldState)
                       : filter ((≢ pageId) . fst) (wmWorlds mgr) }, ())

    -- Minimal WorldGenParams so the render pipeline doesn't bail on
    -- Nothing. Built BEFORE the chunks (#1718) because the base is
    -- generated from the seed recorded here, not the other way round:
    -- 'isArenaParams' recognises an arena by an empty timeline and
    -- wgpSeed 0, so 0 stays the canonical arena seed and the base has to
    -- be the base that seed produces.
    let arenaParams = defaultWorldGenParams
            { wgpSeed      = 0
            , wgpWorldSize = 100000 -- arena is very big
            }

    -- Arena chunk set: shared with the save-load restore path (#365) so a
    -- loaded arena page is rebuilt exactly like a fresh one — which needs
    -- the SAME generator on both sides (#1718), hence the recorded seed.
    let arenaZ    = seaLevel    -- z = 0 (surface)
        allChunks = generateArenaChunks (arenaGenForSeed (wgpSeed arenaParams))
        chunkMap  = HM.fromList [ (lcCoord c, c) | c ← allChunks ]

    -- Write tile data
    atomicModifyIORef' (wsTilesRef worldState) $ \_ →
        (WorldTileData { wtdChunks = chunkMap, wtdMaxChunks = 100 }, ())
    -- Arena chunks are residency too (#2001): same admission boundary,
    -- and 'canonicalChunkCoord' is the identity on an arena page, so the
    -- sentinel wgpWorldSize never reaches 'wrapChunkCoordU'.
    seedResidentChunks worldState pageId arenaParams (map lcCoord allChunks)

    -- Force the arena chunks to NF so the LoadDone below is honest (same
    -- contract as the progressive loader). Tiny 5×5 arena, negligible cost.
    _ ← evaluate (force allChunks)

    writeIORef (wsGenParamsRef worldState) (Just arenaParams)

    -- Mark as fully loaded immediately (no progressive loading needed)
    writeIORef (wsLoadPhaseRef worldState) LoadDone

    -- Set camera z-slice to just above the surface
    atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
        (cam { camZSlice = arenaZ + surfaceHeadroom
             , camZTracking = True
             , camPosition = (0, 0)
             , camZoom = 0.5
             }, ())

    let totalChunks = length allChunks
    logInfo logger CatWorld $ "Test arena initialized: "
        <> tshow totalChunks
        <> " flat loam chunks at z=" <> tshow arenaZ

handleWorldInitArenaDoneCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldInitArenaDoneCommand env logger pageId = do
    logInfo logger CatWorld $ "Arena textures ready, showing: " <> unWorldPageId pageId
    
    -- Now safe to make visible — all texture commands have been processed
    atLimit ← atomicModifyIORef' (wsWorldManagerRef (toWorldSimCapability env)) $ \mgr' →
      -- #1602: the request is discharged either way — it tracks
      -- requests, not effects — while the GENERATION moves only when the
      -- visible list actually changes, exactly as in
      -- handleWorldShowCommand.
      let mgr = completeSelectionChange mgr' in
        if pageId `elem` wmVisible mgr
        then (mgr, False)
        -- The same visible-page limit handleWorldShowCommand enforces,
        -- for the same reason (#1869): one frame can light exactly
        -- 'maxSolarPages' pages by their own clocks, and a page past
        -- that would be drawn with another page's sun.
        else if length (wmVisible mgr) ≥ maxSolarPages
        then (mgr, True)
        else (bumpSelectionGen (mgr { wmVisible = pageId : wmVisible mgr }), False)

    when atLimit $ logWarn logger CatWorld $
        "Arena " <> unWorldPageId pageId <> " stays hidden: already "
        <> tshow maxSolarPages
        <> " visible worlds, the most one frame can light individually"
    
    -- Broadcast to Lua that the arena is ready to display
    let lteq = ivLuaQueue (toInputViewCapability env)
    Q.writeQueue lteq (LuaArenaReady (unWorldPageId pageId))
