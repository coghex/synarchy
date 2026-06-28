{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Command.Save
    ( handleWorldSaveCommand
    , handleWorldLoadSaveCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, logWarn
                       , LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import Structure.Types (emptyChunkStructures)
import World.Constants (seaLevel)
import World.Generate (generateChunk, cameraChunkCoord)
import World.Grid (worldToGrid)
import World.Generate.Constants (chunkLoadRadius)
import World.Generate.Timeline (applyTimelineFast)
import World.Geology (buildTimeline)
import World.Geology.Log (formatTimeline, formatPlatesSummary)
import World.Fluids (computeOceanMap, isOceanChunk)
import World.Plate (generatePlates, elevationAtGlobal)
import World.Preview (buildPreviewFromPixels, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCacheWithPixels)
import World.ZoomMap.ColorPalette (buildColorPalette)
import World.ZoomMap.ChunkTexture (buildZoomAtlas, ZoomAtlasData(..))
import World.Save.Serialize (saveWorld)
import World.Save.Types (toBuildingSnapshot, fromBuildingSnapshot
                        , toUnitSnapshot, fromUnitSnapshot)
import World.Tool.Types (ToolMode(DefaultTool))
import World.Edit.Apply (replayEdits)
import World.Mine.Apply (applyDigSlopes)
import Building.Types (BuildingManager(..), unBuildingId, biPage)
import Unit.Types (UnitManager(..), unUnitId, unitsOnPage, uiPage)
import Unit.Sim.Types (UnitThreadState(..))
import World.Weather (initEarlyClimate, formatWeather, defaultClimateParams)
import World.Thread.Helpers (sendGenLog, sendSaveLoaded, unWorldPageId)
import Engine.PlayerEvent.Emit (emitEvent)
import World.Thread.ChunkLoading (maxChunksPerTick)


-- | Order-preserving de-duplication of a page-id list (keeps the first
--   occurrence). Used to build the restored visibility list without dupes.
dedupPages ∷ [WorldPageId] → [WorldPageId]
dedupPages = go HS.empty
  where go _    []       = []
        go seen (p : ps)
            | p `HS.member` seen = go seen ps
            | otherwise          = p : go (HS.insert p seen) ps

-- | Save: snapshot the live WorldState and write to disk ──logInfo logger CatWorld $ "Saving world: " <> unWorldPageId pageId
handleWorldSaveCommand ∷ EngineEnv → LoggerState → WorldPageId → Text
                       → Text → HM.HashMap Text Text → IO ()
handleWorldSaveCommand env logger pageId saveName timestampTxt luaBlobs = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for save: " <> unWorldPageId pageId
        Just activeWs → do
            -- Auto-pause BEFORE reading state so the snapshot
            -- captures pause = True (DF convention — saved worlds
            -- load paused so the player can plan the next move).
            writeIORef (enginePausedRef env) True
            -- Globals: read once, shared across every page (we're on the
            -- world thread, so no races with worldLoop writes).
            cam        ← readIORef (cameraRef env)
            gameTime   ← readIORef (gameTimeRef env)
            paused     ← readIORef (enginePausedRef env)
            -- v54 (structure persistence): the texture palette is global.
            texPalette ← readIORef (texPaletteRef env)
            -- v56 (item-instance identity, #67): persist the allocator so
            -- new items created after a reload keep unique ids.
            nextItemId ← readIORef (nextItemInstanceIdRef env)
            -- The entity managers are global across worlds (#76/#78); read
            -- them once and slice per page below.
            bm         ← readIORef (buildingManagerRef env)
            um         ← readIORef (unitManagerRef env)
            uts        ← readIORef (utsRef env)
            -- The active page's gen params drive the save-listing metadata.
            mActiveParams ← readIORef (wsGenParamsRef activeWs)
            case mActiveParams of
                Nothing →
                    logWarn logger CatWorld
                        "Cannot save: active world has no gen params"
                Just activeParams → do
                    -- #216: snapshot EVERY live page in wmWorlds, not just
                    -- the active one. A page with no gen params (e.g. an
                    -- arena still mid-init) is not a real, persistable world
                    -- — skip it rather than abort the whole save.
                    pageSaves ← forM (wmWorlds mgr) $ \(pid, ws) → do
                        mParams ← readIORef (wsGenParamsRef ws)
                        case mParams of
                            Nothing → pure Nothing
                            Just params → do
                                WorldTime h m    ← readIORef (wsTimeRef ws)
                                WorldDate y mo d ← readIORef (wsDateRef ws)
                                tScale    ← readIORef (wsTimeScaleRef ws)
                                -- Freeze each page's world clock to match the
                                -- auto-pause above. 'tScale' (the player's
                                -- chosen speed) is captured for wpsTimeScale
                                -- first, so zeroing the ref loses nothing.
                                -- Without this the engine reports paused
                                -- (enginePausedRef) while World.Thread.Time
                                -- keeps advancing a page's time of day off its
                                -- wsTimeScaleRef — a "paused" world whose
                                -- clock runs (#42).
                                writeIORef (wsTimeScaleRef ws) 0
                                mapMode   ← readIORef (wsMapModeRef ws)
                                toolMode  ← readIORef (wsToolModeRef ws)
                                edits     ← readIORef (wsEditsRef ws)
                                mineDesigs ← readIORef (wsMineDesignationsRef ws)
                                groundItems ← readIORef (wsGroundItemsRef ws)
                                spoilPiles ← readIORef (wsSpoilRef ws)
                                WorldCamera wcx wcy ← readIORef (wsCameraRef ws)
                                -- Camera: the active page uses the live global
                                -- Camera2D (authoritative position/zoom/facing
                                -- the player sees). Other pages carry only a
                                -- WorldCamera (x, y) in their own state — no
                                -- per-page zoom/facing exists — so they save
                                -- their stored position with the global
                                -- zoom/facing as the best available value.
                                let isActive = pid ≡ pageId
                                    (cx, cy) = if isActive
                                               then camPosition cam
                                               else (wcx, wcy)
                                    buildings = toBuildingSnapshot pid bm
                                    units     = toUnitSnapshot pid um
                                    -- Keep only this page's units' sim states.
                                    savedUids = HM.keysSet
                                        (unitsOnPage pid (umInstances um))
                                    simStates = HM.filterWithKey
                                        (\uid _ → uid `HS.member` savedUids)
                                        (utsSimStates uts)
                                pure $ Just WorldPageSave
                                    { wpsPageId     = pid
                                    , wpsGenParams  = params
                                    , wpsCameraX    = cx
                                    , wpsCameraY    = cy
                                    , wpsCameraZoom = camZoom cam
                                    , wpsCameraFacing = camFacing cam
                                    , wpsTimeHour   = h
                                    , wpsTimeMinute = m
                                    , wpsDateYear   = y
                                    , wpsDateMonth  = mo
                                    , wpsDateDay    = d
                                    , wpsTimeScale  = tScale
                                    , wpsMapMode    = mapMode
                                    , wpsToolMode   = toolMode
                                    , wpsEdits      = edits
                                    , wpsMineDesignations = mineDesigs
                                    , wpsGroundItems = groundItems
                                    , wpsSpoilPiles  = spoilPiles
                                    , wpsBuildings   = buildings
                                    , wpsUnits       = units
                                    , wpsUnitSimStates = simStates
                                    }
                    -- UTC ISO 8601 microsecond precision, captured and
                    -- monotonically clamped at the API request time (see
                    -- saveWorldFn) — NOT here, so two saves queued
                    -- back-to-back don't get the same wall-second
                    -- timestamp from world-thread processing latency.
                    -- Lexicographic sort by this fixed-width string is
                    -- chronologically correct, so the
                    -- Lua-side `a.timestamp > b.timestamp` in
                    -- main_menu works without further wrapping.
                    let meta = SaveMetadata
                            { smName       = saveName
                            , smSeed       = wgpSeed activeParams
                            , smWorldSize  = wgpWorldSize activeParams
                            , smPlateCount = wgpPlateCount activeParams
                            , smTimestamp  = timestampTxt
                            }
                        sd = SaveData
                            { sdMetadata   = meta
                            , sdGameTime     = gameTime
                            , sdEnginePaused = paused
                            , sdLuaModules   = luaBlobs
                            , sdTexPalette   = texPalette
                            , sdNextItemInstanceId = nextItemId
                            , sdActivePage   = pageId
                            -- Record visibility so the loaded game comes up
                            -- showing what the player last saw (#216).
                            , sdVisiblePages = wmVisible mgr
                            , sdWorlds       = catMaybes pageSaves
                            }
                    result ← saveWorld saveName sd
                    case result of
                        Right () → do
                            logInfo logger CatWorld $
                                "World saved successfully: " <> saveName
                            emitEvent env "save_load" "World.Save" $
                                "Game saved: " <> saveName
                        Left err → do
                            logError logger CatWorld $
                                "Failed to save world: " <> err
                            emitEvent env "save_load" "World.Save" $
                                "Save failed: " <> err

-- | Load: reconstruct WorldState from SaveData ──
--   This is essentially WorldInit but skipping the expensive
--   buildTimeline / computeOceanMap / initEarlyClimate steps
--   because those are already baked into sdGenParams.
handleWorldLoadSaveCommand ∷ EngineEnv → LoggerState → WorldPageId → SaveData → IO ()
handleWorldLoadSaveCommand env logger pageId saveData
  | (firstWps : _) ← sdWorlds saveData = do
    logInfo logger CatWorld $ "Loading save into world: "
        <> unWorldPageId pageId

    -- #217/#218: restore EVERY saved page in sdWorlds, not just the active
    -- one. The active page (the one whose id matches 'sdActivePage') restores
    -- under the load-target id 'pageId' (always "main_world" — the documented
    -- convention Lua/headless code assumes); additional pages restore under
    -- their own saved ids. The pattern guard binds 'firstWps' as the fallback
    -- for a save whose recorded active id names no page.
    let activeWps   = fromMaybe firstWps (activeWorldPage saveData)
        activeWpsId = wpsPageId activeWps
        -- Restore-target id for a saved page: the active page → 'pageId'
        -- (main_world); every other page keeps its own id.
        restoreId w = if wpsPageId w ≡ activeWpsId then pageId else wpsPageId w
        -- Process non-active pages first and the active page LAST so it ends
        -- up at the head of wmWorlds — that keeps 'resolveActiveWorld' (and
        -- hence world.getInitProgress / waitForInit, which poll the active
        -- world) pointed at main_world's load phase throughout the restore.
        orderedPages = filter ((≢ activeWpsId) . wpsPageId) (sdWorlds saveData)
                         ++ [activeWps]

    -- 0. Restore the genuinely global state ONCE (shared by every page).
    --    enginePaused is normally True here (auto-pause-on-save), so the
    --    loaded world starts paused and the player must explicitly resume.
    --    gameTime restoration keeps every saved *Until timer coherent —
    --    without it, anim expiries computed against gameTimeRef would all
    --    fire instantly after load.
    writeIORef (gameTimeRef env)     (sdGameTime saveData)
    writeIORef (enginePausedRef env) (sdEnginePaused saveData)
    -- v54 (structures): restore the texture palette BEFORE any chunk replays
    -- its WeSetStructure edits, so palette-id → path resolution is available.
    -- Structures themselves ride each page's edits (replayed per chunk).
    writeIORef (texPaletteRef env) (sdTexPalette saveData)
    -- The paletteId → runtime-handle map is session-local (handles differ per
    -- run). Clear it so the Lua resolve tick re-loads every palette texture
    -- for THIS session and the renderer can resolve loaded pieces.
    writeIORef (texPaletteHandlesRef env) HM.empty
    -- v56 (item-instance identity, #67): advance the allocator past every
    -- saved iiInstanceId. max (never lower) so a within-session load over a
    -- session that already minted higher ids can't recycle a live id.
    atomicModifyIORef' (nextItemInstanceIdRef env) $ \cur →
        (max cur (sdNextItemInstanceId saveData), ())

    -- Shared, world-independent inputs to the per-chunk zoom pipeline.
    registry ← readIORef (materialRegistryRef env)
    let !_ = registry `seq` ()
    palette ← buildColorPalette logger "data/materials" "data/vegetation"
    _ ← evaluate (force palette)
    catalog ← readIORef (floraCatalogRef env)
    -- Snapshot the live managers BEFORE restoring any page, so the
    -- off-page merge (#218) sees pre-load state and the def maps are stable.
    currentBm ← readIORef (buildingManagerRef env)
    currentUm ← readIORef (unitManagerRef env)

    -- 1. Restore each saved page: WorldState + refs + zoom cache + center
    --    chunk + chunk queue (the historical single-page body, looped). Each
    --    iteration returns this page's restored building/unit slices (stamped
    --    with the page's restore id) for the manager merge in step 2.
    perPage ← forM orderedPages $ \wps → do
        let rid       = restoreId wps
            isActive  = wpsPageId wps ≡ activeWpsId
            params    = wpsGenParams wps
            seed      = wgpSeed params
            worldSize = wgpWorldSize params

        logInfo logger CatWorld $ "Restoring saved page: "
            <> unWorldPageId (wpsPageId wps) <> " → " <> unWorldPageId rid

        worldState ← emptyWorldState
        let phaseRef   = wsLoadPhaseRef worldState
            totalSteps = 4

        -- Register early so the render thread can find this world when
        -- uploading the zoom atlas (same as init path). Dedup by restore id
        -- so loading over an existing page replaces it rather than stacking a
        -- duplicate entry (#58).
        atomicModifyIORef' (worldManagerRef env) $ \mgr →
            (mgr { wmWorlds = (rid, worldState)
                            : filter ((/= rid) . fst) (wmWorlds mgr) }, ())

        -- 1a. Restore gen params (plates, timeline, ocean map, climate are
        --     all baked inside) + the per-page mutable game state.
        when isActive $ writeIORef phaseRef (LoadPhase1 1 totalSteps)
        when isActive $ sendGenLog env "Loading saved world state..."
        writeIORef (wsGenParamsRef worldState) (Just params)
        writeIORef (wsCameraRef worldState)
            (WorldCamera (wpsCameraX wps) (wpsCameraY wps))
        writeIORef (wsTimeRef worldState)
            (WorldTime (wpsTimeHour wps) (wpsTimeMinute wps))
        writeIORef (wsDateRef worldState)
            (WorldDate (wpsDateYear wps) (wpsDateMonth wps) (wpsDateDay wps))
        -- Keep wsTimeScaleRef synchronized with the restored pause flag: a
        -- paused save (the normal auto-pause-on-save case) must load with the
        -- live clock frozen, not running at the player's saved speed. The
        -- chosen speed is preserved in wpsTimeScale and reapplied when the
        -- player resumes (scripts/pause.lua prevTimeScale) (#42).
        writeIORef (wsTimeScaleRef worldState)
            (if sdEnginePaused saveData then 0 else wpsTimeScale wps)
        writeIORef (wsMapModeRef worldState) (wpsMapMode wps)
        -- A loaded world starts on the default tool, NOT the tool that was
        -- active at save time. The HUD toolbar always comes up on the default
        -- slot after a load, so the engine ToolMode must match or
        -- world.getToolMode() and the toolbar would disagree. wpsToolMode is
        -- still recorded at save time but intentionally not restored. (#103)
        writeIORef (wsToolModeRef worldState) DefaultTool
        -- Restore edit log BEFORE chunk generation so the synchronous center
        -- chunk + every chunk pulled off the init queue replay the player's
        -- edits. Mine designations / ground items / spoil piles render from
        -- stored data and need no chunk loading first.
        writeIORef (wsEditsRef worldState) (wpsEdits wps)
        writeIORef (wsMineDesignationsRef worldState) (wpsMineDesignations wps)
        writeIORef (wsGroundItemsRef worldState) (wpsGroundItems wps)
        writeIORef (wsSpoilRef worldState) (wpsSpoilPiles wps)

        -- 1b. Rebuild this page's zoom cache with per-chunk textures.
        --     'buildTimeline' didn't run on this code path, so we don't have
        --     the init-time bordered cache; pass Nothing and let
        --     generateZoomTerrain recompute the per-chunk pipeline.
        when isActive $ writeIORef phaseRef (LoadPhase1 2 totalSteps)
        let (zoomCache, chunkPixels) =
                buildZoomCacheWithPixels params registry palette Nothing
        _ ← evaluate (force zoomCache)
        writeIORef (wsZoomCacheRef worldState) zoomCache
        writeIORef (wsZoomAtlasRef worldState) Nothing
        -- The zoom atlas + preview stage through GLOBAL env refs, and the GPU
        -- upload writes one shared atlas to every world's wsZoomAtlasRef
        -- (handleZoomAtlasUpload). So only the ACTIVE/visible page stages its
        -- atlas; background pages keep their per-page zoom cache and adopt the
        -- shared atlas if later shown (the existing single-atlas limitation —
        -- background worlds are latent today).
        when isActive $ do
            _ ← evaluate (force chunkPixels)
            sendGenLog env "Assembling zoom texture atlas..."
            let atlas = buildZoomAtlas (V.length zoomCache) chunkPixels
            _ ← evaluate (force atlas)
            writeIORef (zoomAtlasDataRef env) $
                Just (zadWidth atlas, zadHeight atlas, zadPixelData atlas)
            sendGenLog env "Rendering world preview..."
            let preview = buildPreviewFromPixels params zoomCache chunkPixels
            _ ← evaluate (force preview)
            writeIORef (worldPreviewRef env) $
                Just (piWidth preview, piHeight preview, piData preview)

        -- 1c. Generate the center chunk synchronously for immediate display.
        --     Use the canonical screen→grid→chunk conversion so the saved
        --     camera facing targets the right chunk for synchronous regen.
        when isActive $ writeIORef phaseRef (LoadPhase1 3 totalSteps)
        when isActive $ sendGenLog env "Generating initial chunks..."
        let centerCoord@(ChunkCoord camCX camCY) =
                cameraChunkCoord (wpsCameraFacing wps)
                                 (wpsCameraX wps)
                                 (wpsCameraY wps)
            (ct, cs, cterrain, cf, cice, cflora, cwt, cmagma) =
                generateChunk registry catalog params centerCoord
            seededSurf = VU.imap (\idx surfZ →
                case cf V.! idx of
                    Just fc → max surfZ (fcSurface fc)
                    Nothing → surfZ
                ) cs
            centerChunkRaw = LoadedChunk
                { lcCoord             = centerCoord
                , lcTiles             = ct
                , lcSurfaceMap        = seededSurf
                , lcTerrainSurfaceMap = cterrain
                , lcFluidMap          = cf
                , lcIceMap            = cice
                , lcFlora             = cflora
                , lcSideDeco          = VU.replicate (chunkSize * chunkSize) 0
                , lcWaterTableMap    = cwt
                , lcMagma            = cmagma
                , lcStructures       = emptyChunkStructures
                }
        edits ← readIORef (wsEditsRef worldState)
        desigs ← readIORef (wsMineDesignationsRef worldState)
        let centerChunk = applyDigSlopes desigs (replayEdits edits centerChunkRaw)
        atomicModifyIORef' (wsTilesRef worldState) $ \_ →
            (WorldTileData { wtdChunks    = HM.singleton centerCoord centerChunk
                           , wtdMaxChunks = 200 }, ())

        -- 1d. Queue the remaining initial chunks for progressive loading.
        when isActive $ writeIORef phaseRef (LoadPhase1 4 totalSteps)
        let remainingCoords =
                [ ChunkCoord cx cy
                | cx ← [camCX - chunkLoadRadius .. camCX + chunkLoadRadius]
                , cy ← [camCY - chunkLoadRadius .. camCY + chunkLoadRadius]
                , not (cx ≡ camCX ∧ cy ≡ camCY)
                ]
            totalInitialChunks =
                (2 * chunkLoadRadius + 1) * (2 * chunkLoadRadius + 1)
        writeIORef (wsInitQueueRef worldState) remainingCoords
        writeIORef phaseRef (LoadPhase2 (length remainingCoords) totalInitialChunks)

        -- 1e. Active page only: set the global camera (position/zoom/facing)
        --     and its z-slice. elevationAtGlobal takes grid coords (gx, gy),
        --     not world coords — go through worldToGrid (Render.hs:136 etc.).
        when isActive $ do
            let (camGX, camGY) = worldToGrid (wpsCameraFacing wps)
                                             (wpsCameraX wps)
                                             (wpsCameraY wps)
                (surfaceElev, _mat) =
                    elevationAtGlobal seed (wgpPlates params) worldSize camGX camGY
                startZSlice = surfaceElev + surfaceHeadroom
            -- Record-construction (not update) so every Camera2D field is set
            -- explicitly: an update would preserve the prior camera's
            -- transient state (pan/zoom velocity, mid-drag flag), and the
            -- compiler now forces a decision if Camera2D gains a field.
            atomicModifyIORef' (cameraRef env) $ \_ →
                (Camera2D
                    { camPosition     = (wpsCameraX wps, wpsCameraY wps)
                    , camVelocity     = (0, 0)
                    , camZoom         = wpsCameraZoom wps
                    , camZoomVelocity = 0
                    , camRotation     = 0
                    , camFacing       = wpsCameraFacing wps
                    , camDragging     = False
                    , camDragOrigin   = (0, 0)
                    , camZSlice       = startZSlice
                    , camZTracking    = True
                    }, ())
            sendGenLog env $ "Save loaded: "
                <> T.pack (show totalInitialChunks) <> " chunks queued"
            logInfo logger CatWorld $ "Save loaded: "
                <> T.pack (show totalInitialChunks) <> " chunks, "
                <> "surface at z=" <> T.pack (show surfaceElev)
                <> ": " <> unWorldPageId rid

        -- 1f. Resolve this page's building/unit slices, stamped with its
        --     restore id, for the manager merge below. Buildings/units carry
        --     stored gridZ so this is independent of chunk loading order.
        let (restoredBm, bOrphans) =
                fromBuildingSnapshot rid (bmDefs currentBm) (wpsBuildings wps)
            (restoredUm, uOrphans) =
                fromUnitSnapshot rid (umDefs currentUm) (wpsUnits wps)
            liveUids = HM.keysSet (umInstances restoredUm)
            -- Drop sim states whose owning unit was orphaned.
            simStates' = HM.filterWithKey (\uid _ → uid `HS.member` liveUids)
                                          (wpsUnitSimStates wps)
        pure (rid, restoredBm, bOrphans, restoredUm, uOrphans, simStates')

    -- 2. Merge every restored page's entities into the GLOBAL managers
    --    (#218, closes #191). The managers span all worlds, so a load must
    --    replace ONLY the restored pages' slices and keep every other live
    --    page's buildings/units/sim-states intact. nextId = max so the saved
    --    counter can't reuse a still-live off-page id.
    let restoredPageIds = HS.fromList [ rid | (rid,_,_,_,_,_) ← perPage ]
        allRestoredB = HM.unions [ bmInstances b | (_,b,_,_,_,_) ← perPage ]
        allRestoredU = HM.unions [ umInstances u | (_,_,_,u,_,_) ← perPage ]
        allSimStates = HM.unions [ s | (_,_,_,_,_,s) ← perPage ]
        bOrphansAll  = concat [ o | (_,_,o,_,_,_) ← perPage ]
        uOrphansAll  = concat [ o | (_,_,_,_,o,_) ← perPage ]
        bMaxNextId   = maximum (bmNextId currentBm
                                  : [ bmNextId b | (_,b,_,_,_,_) ← perPage ])
        uMaxNextId   = maximum (umNextId currentUm
                                  : [ umNextId u | (_,_,_,u,_,_) ← perPage ])
        -- Off-page = entities whose page is NOT among the restored pages.
        offPageB     = HM.filter (\bi → not (HS.member (biPage bi) restoredPageIds))
                                 (bmInstances currentBm)
        offPageU     = HM.filter (\ui → not (HS.member (uiPage ui) restoredPageIds))
                                 (umInstances currentUm)
        offPageUids  = HM.keysSet offPageU
        -- An off-page id that collides with a restored id can't share the
        -- global map: the loaded save wins and the off-page entity is dropped.
        -- Unreachable in normal play (one global id counter per session keeps
        -- live ids disjoint); only a cross-session load of an *older* save can
        -- hit it. Logged below so it's diagnosable, not silent.
        collidingB   = HM.intersection offPageB allRestoredB
        collidingU   = HM.intersection offPageU allRestoredU
        mergedBm     = currentBm
            { bmInstances = HM.union allRestoredB offPageB
            , bmNextId    = bMaxNextId
            , bmSelected  = Nothing
            }
        mergedUm     = currentUm
            { umInstances = HM.union allRestoredU offPageU
            , umNextId    = uMaxNextId
            , umSelected  = mempty
            }
    writeIORef (buildingManagerRef env) mergedBm
    -- Race note: there's a window between writing unitManagerRef and utsRef.
    -- The unit thread keeps running publishToRender every tick during load
    -- (it gates only tickAllMovement on enginePausedRef). A read in between
    -- sees post-load instances with pre-load simStates, but publishToRender's
    -- `Nothing → inst` fallback (Unit/Thread.hs) keeps any uid not in the
    -- simStates map unchanged — one frame of loaded animation state without
    -- sim-driven overrides, imperceptible.
    writeIORef (unitManagerRef env) mergedUm
    atomicModifyIORef' (utsRef env) $ \old →
        -- Preserve off-page units' sim states; replace the restored pages'.
        let keptSim = HM.filterWithKey (\uid _ → uid `HS.member` offPageUids)
                                       (utsSimStates old)
        in (UnitThreadState { utsSimStates = HM.union allSimStates keptSim }, ())

    -- Collision + orphan logging (combined across every restored page).
    case HM.size collidingB of
        0 → pure ()
        n → logWarn logger CatWorld $
                "Save load: " <> T.pack (show n)
                <> " off-page building id(s) collided with a loaded page and "
                <> "were dropped (ids: "
                <> T.pack (show (map unBuildingId (HM.keys collidingB)))
                <> ") — see #214"
    case HM.size collidingU of
        0 → pure ()
        n → logWarn logger CatWorld $
                "Save load: " <> T.pack (show n)
                <> " off-page unit id(s) collided with a loaded page and were "
                <> "dropped (ids: "
                <> T.pack (show (map unUnitId (HM.keys collidingU)))
                <> ") — see #214"
    forM_ bOrphansAll $ \bid →
        logWarn logger CatWorld $
            "Save load: dropping building id="
              <> T.pack (show (unBuildingId bid))
              <> " — its def is no longer registered"
    forM_ uOrphansAll $ \uid →
        logWarn logger CatWorld $
            "Save load: dropping unit id="
              <> T.pack (show (unUnitId uid))
              <> " — its def is no longer registered"
    -- Player-facing summary via the existing onWorldGenLog pathway. Per-id
    -- detail stays in the engine log; one toast per category is enough.
    case length bOrphansAll of
        0 → pure ()
        n → sendGenLog env $
                "Save load: dropped " <> T.pack (show n)
                <> " building" <> (if n == 1 then "" else "s")
                <> " (def no longer registered)"
    case length uOrphansAll of
        0 → pure ()
        n → sendGenLog env $
                "Save load: dropped " <> T.pack (show n)
                <> " unit" <> (if n == 1 then "" else "s")
                <> " (def no longer registered)"

    -- 3. Restore visibility (#217): remap the active id → main_world and keep
    --    main_world at the head so 'resolveActiveWorld' lands on it. Filter to
    --    pages that actually registered above (defensive).
    let remapVis p   = if p ≡ activeWpsId then pageId else p
        savedVisible = map remapVis (sdVisiblePages saveData)
        wantVisible  = pageId : filter (/= pageId) savedVisible
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        ( mgr { wmVisible = filter (\p → isJust (lookup p (wmWorlds mgr)))
                                   (dedupPages wantVisible) }, () )

    -- 4. Signal Lua with the SURVIVORS across every restored page (union of
    --    each page's restored set). The Lua blob is a global singleton, so it
    --    carries off-page ids too; the reconcile rebuilds each table as
    --    "survivors restored from the blob + every other still-live entity's
    --    PRE-LOAD state", so the load replaces only restored-page state and
    --    other live pages are untouched (#195, #191).
    let bSurvivingIds = map (fromIntegral . unBuildingId)
                            (HM.keys allRestoredB) ∷ [Int]
        uSurvivingIds = map (fromIntegral . unUnitId)
                            (HM.keys allRestoredU) ∷ [Int]
    sendSaveLoaded env uSurvivingIds bSurvivingIds

  | otherwise =
      -- Defense-in-depth: loadWorld already rejects an empty sdWorlds at
      -- decode time (Serialize.hs), so engine.loadSave fails cleanly before
      -- queueing this command and this branch is unreachable via the normal
      -- path. Kept so a future direct caller can't crash the world thread.
      logError logger CatWorld
          "Cannot load save: save contains no world pages"
