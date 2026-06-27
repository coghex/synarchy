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
import World.Edit.Apply (replayEdits)
import World.Mine.Apply (applyDigSlopes)
import Building.Types (BuildingManager(..), unBuildingId, buildingsOnPage)
import Unit.Types (UnitManager(..), unUnitId, unitsOnPage)
import Unit.Sim.Types (UnitThreadState(..))
import World.Weather (initEarlyClimate, formatWeather, defaultClimateParams)
import World.Thread.Helpers (sendGenLog, unWorldPageId)
import Engine.PlayerEvent.Emit (emitEvent)
import World.Thread.ChunkLoading (maxChunksPerTick)


-- | Save: snapshot the live WorldState and write to disk ──logInfo logger CatWorld $ "Saving world: " <> unWorldPageId pageId
handleWorldSaveCommand ∷ EngineEnv → LoggerState → WorldPageId → Text
                       → Text → HM.HashMap Text Text → IO ()
handleWorldSaveCommand env logger pageId saveName timestampTxt luaBlobs = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for save: " <> unWorldPageId pageId
        Just worldState → do
            -- Auto-pause BEFORE reading state so the snapshot
            -- captures pause = True (DF convention — saved worlds
            -- load paused so the player can plan the next move).
            writeIORef (enginePausedRef env) True
            -- Read every IORef we care about (we're on the
            -- world thread, so no races with worldLoop writes)
            mParams   ← readIORef (wsGenParamsRef worldState)
            cam       ← readIORef (cameraRef env)
            let (cx, cy) = camPosition cam
            WorldTime h m     ← readIORef (wsTimeRef worldState)
            WorldDate y mo d  ← readIORef (wsDateRef worldState)
            tScale    ← readIORef (wsTimeScaleRef worldState)
            -- Freeze the live world clock to match the auto-pause above.
            -- 'tScale' (the player's chosen speed) was just captured for
            -- sdTimeScale, so zeroing wsTimeScaleRef here loses nothing.
            -- Without this the engine reports paused (enginePausedRef)
            -- while World.Thread.Time keeps advancing time of day off
            -- wsTimeScaleRef — i.e. a "paused" world whose clock runs (#42).
            writeIORef (wsTimeScaleRef worldState) 0
            mapMode   ← readIORef (wsMapModeRef worldState)
            toolMode  ← readIORef (wsToolModeRef worldState)
            -- v2 (Phase 1) additions
            gameTime  ← readIORef (gameTimeRef env)
            paused    ← readIORef (enginePausedRef env)
            -- v3 (Phase 2) additions
            edits     ← readIORef (wsEditsRef worldState)
            -- v31 (mining) additions
            mineDesigs ← readIORef (wsMineDesignationsRef worldState)
            -- v32 (ground items) additions
            groundItems ← readIORef (wsGroundItemsRef worldState)
            spoilPiles ← readIORef (wsSpoilRef worldState)
            -- v54 (structure persistence) additions
            texPalette ← readIORef (texPaletteRef env)
            -- v4 (Phase 3) additions
            bm        ← readIORef (buildingManagerRef env)
            -- Snapshot only THIS world's buildings/units — the managers are
            -- global across worlds (#76/#78).
            let buildings = toBuildingSnapshot pageId bm
            -- v5 (Phase 4) additions
            um        ← readIORef (unitManagerRef env)
            uts       ← readIORef (utsRef env)
            let units     = toUnitSnapshot pageId um
                -- Keep only the saved world's units' sim states.
                savedUids = HM.keysSet (unitsOnPage pageId (umInstances um))
                simStates = HM.filterWithKey (\uid _ → uid `HS.member` savedUids)
                                             (utsSimStates uts)

            case mParams of
                Nothing →
                    logWarn logger CatWorld
                        "Cannot save: world has no gen params"
                Just params → do
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
                            , smSeed       = wgpSeed params
                            , smWorldSize  = wgpWorldSize params
                            , smPlateCount = wgpPlateCount params
                            , smTimestamp  = timestampTxt
                            }
                        sd = SaveData
                            { sdMetadata   = meta
                            , sdGenParams  = params
                            , sdCameraX    = cx
                            , sdCameraY    = cy
                            , sdCameraZoom = camZoom cam
                            , sdCameraFacing = camFacing cam
                            , sdTimeHour   = h
                            , sdTimeMinute = m
                            , sdDateYear   = y
                            , sdDateMonth  = mo
                            , sdDateDay    = d
                            , sdTimeScale  = tScale
                            , sdMapMode    = mapMode
                            , sdToolMode   = toolMode
                            , sdGameTime     = gameTime
                            , sdEnginePaused = paused
                            , sdEdits        = edits
                            , sdBuildings    = buildings
                            , sdUnits        = units
                            , sdUnitSimStates = simStates
                            , sdLuaModules   = luaBlobs
                            , sdMineDesignations = mineDesigs
                            , sdGroundItems  = groundItems
                            , sdSpoilPiles   = spoilPiles
                            , sdTexPalette   = texPalette
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
handleWorldLoadSaveCommand env logger pageId saveData = do
    logInfo logger CatWorld $ "Loading save into world: "
        <> unWorldPageId pageId

    let params    = sdGenParams saveData
        seed      = wgpSeed params
        worldSize = wgpWorldSize params

    worldState ← emptyWorldState
    let phaseRef = wsLoadPhaseRef worldState
        totalSteps = 4

    -- Register early so the render thread can find this world
    -- when uploading the zoom atlas (same as init path). Dedup by page
    -- id so loading over an existing "main_world" replaces it rather than
    -- stacking a duplicate entry (#58).
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        (mgr { wmWorlds = (pageId, worldState)
                        : filter ((/= pageId) . fst) (wmWorlds mgr) }, ())

    -- 1. Restore gen params (the big one — plates, timeline,
    --    ocean map, climate are all inside here)
    writeIORef phaseRef (LoadPhase1 1 totalSteps)
    sendGenLog env "Loading saved world state..."
    writeIORef (wsGenParamsRef worldState) (Just params)

    -- 2. Restore mutable game state from the save
    writeIORef (wsCameraRef worldState)
        (WorldCamera (sdCameraX saveData) (sdCameraY saveData))
    writeIORef (wsTimeRef worldState)
        (WorldTime (sdTimeHour saveData) (sdTimeMinute saveData))
    writeIORef (wsDateRef worldState)
        (WorldDate (sdDateYear saveData)
                   (sdDateMonth saveData)
                   (sdDateDay saveData))
    -- Keep wsTimeScaleRef synchronized with the restored pause flag: a
    -- paused save (the normal auto-pause-on-save case) must load with the
    -- live clock frozen, not running at the player's saved speed. The
    -- chosen speed is preserved in sdTimeScale and reapplied when the
    -- player resumes (scripts/pause.lua prevTimeScale) (#42).
    writeIORef (wsTimeScaleRef worldState)
        (if sdEnginePaused saveData then 0 else sdTimeScale saveData)
    writeIORef (wsMapModeRef worldState) (sdMapMode saveData)
    writeIORef (wsToolModeRef worldState) (sdToolMode saveData)
    -- v2 (Phase 1): engine-level refs. enginePaused is normally True
    -- here (auto-pause-on-save), so the loaded world starts paused
    -- and the player has to explicitly resume. gameTime restoration
    -- keeps every saved *Until timer coherent — without it, anim
    -- expiries computed against gameTimeRef would all fire instantly
    -- after load.
    writeIORef (gameTimeRef env)     (sdGameTime saveData)
    writeIORef (enginePausedRef env) (sdEnginePaused saveData)
    -- v3 (Phase 2): restore edit log BEFORE chunk generation so the
    -- synchronous center chunk + every chunk pulled off the init queue
    -- replay any edits the player had made.
    writeIORef (wsEditsRef worldState) (sdEdits saveData)
    -- v31 (mining): restore designations (incl. mid-dig corner
    -- progress). Markers render from the stored z, so this needs no
    -- chunk loading to be visible.
    writeIORef (wsMineDesignationsRef worldState) (sdMineDesignations saveData)
    -- v32 (ground items): heights derive from terrain at render, so
    -- restoration is position-only and needs no chunk loading.
    writeIORef (wsGroundItemsRef worldState) (sdGroundItems saveData)
    -- v34 (dig yields): spoil fills are relative to tile surfaces;
    -- promoted cells replay from sdEdits independently.
    writeIORef (wsSpoilRef worldState) (sdSpoilPiles saveData)
    -- v54 (structures): restore the texture palette BEFORE any chunk
    -- replays its WeSetStructure edits, so palette-id → path resolution
    -- is available. Structures themselves ride sdEdits (replayed per chunk).
    writeIORef (texPaletteRef env) (sdTexPalette saveData)
    -- The paletteId → runtime-handle map is session-local (handles differ
    -- per run). Clear it so the Lua resolve tick re-loads every palette
    -- texture for THIS session and the renderer can resolve loaded pieces.
    writeIORef (texPaletteHandlesRef env) HM.empty

    -- 3. Rebuild zoom cache with per-chunk textures (matches init path)
    writeIORef phaseRef (LoadPhase1 2 totalSteps)
    sendGenLog env "Building zoom color palette..."
    registry ← readIORef (materialRegistryRef env)
    let !_ = registry `seq` ()
    palette ← buildColorPalette logger "data/materials" "data/vegetation"
    _ ← evaluate (force palette)

    sendGenLog env "Building zoom cache with per-chunk textures..."
    -- Loaded-save path: 'buildTimeline' didn't run on this code path,
    -- so we don't have the init-time bordered cache; pass Nothing and
    -- let generateZoomTerrain recompute the per-chunk pipeline.
    let (zoomCache, chunkPixels) =
            buildZoomCacheWithPixels params registry palette Nothing
    _ ← evaluate (force zoomCache)
    _ ← evaluate (force chunkPixels)
    writeIORef (wsZoomCacheRef worldState) zoomCache

    sendGenLog env "Assembling zoom texture atlas..."
    let atlas = buildZoomAtlas (V.length zoomCache) chunkPixels
    _ ← evaluate (force atlas)
    writeIORef (zoomAtlasDataRef env) $
        Just (zadWidth atlas, zadHeight atlas, zadPixelData atlas)
    writeIORef (wsZoomAtlasRef worldState) Nothing

    sendGenLog env "Rendering world preview..."
    let preview = buildPreviewFromPixels params zoomCache chunkPixels
    _ ← evaluate (force preview)
    writeIORef (worldPreviewRef env) $
        Just (piWidth preview, piHeight preview, piData preview)

    -- 4. Generate center chunk synchronously for immediate display
    writeIORef phaseRef (LoadPhase1 3 totalSteps)
    sendGenLog env "Generating initial chunks..."
    catalog ← readIORef (floraCatalogRef env)
    -- Use the canonical screen→grid→chunk conversion. The bug-version
    -- (floor camX `div` chunkSize) treated world coords as grid coords,
    -- skipping both the isometric projection inverse and the facing
    -- rotation — under FaceWest/North/East, this targets a totally
    -- wrong chunk for synchronous regen, producing a blank center tile
    -- on load until the async queue catches up.
    let centerCoord@(ChunkCoord camCX camCY) =
            cameraChunkCoord (sdCameraFacing saveData)
                             (sdCameraX saveData)
                             (sdCameraY saveData)
        (ct, cs, cterrain, cf, cice, cflora, cwt, cmagma) = generateChunk registry catalog params centerCoord
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
    -- Replay edits onto the freshly-generated center chunk. The edit
    -- log was restored from sdEdits earlier in this handler; if the
    -- player edited tiles in this chunk before saving, those edits
    -- need to show up immediately on load.
    edits ← readIORef (wsEditsRef worldState)
    desigs ← readIORef (wsMineDesignationsRef worldState)
    let centerChunk = applyDigSlopes desigs (replayEdits edits centerChunkRaw)
    atomicModifyIORef' (wsTilesRef worldState) $ \_ →
        (WorldTileData { wtdChunks    = HM.singleton centerCoord centerChunk
                       , wtdMaxChunks = 200 }, ())

    -- 5. Queue remaining initial chunks for progressive loading
    writeIORef phaseRef (LoadPhase1 4 totalSteps)
    let remainingCoords =
            [ ChunkCoord cx cy
            | cx ← [camCX - chunkLoadRadius .. camCX + chunkLoadRadius]
            , cy ← [camCY - chunkLoadRadius .. camCY + chunkLoadRadius]
            , not (cx ≡ camCX ∧ cy ≡ camCY)
            ]
    writeIORef (wsInitQueueRef worldState) remainingCoords

    -- v4 (Phase 3): restore buildings AFTER edits + center-chunk regen.
    -- Buildings store their biGridZ (the Z they were placed on, which
    -- reflects post-edit terrain). The chunk replay above ensures the
    -- center chunk's terrain matches what the player saw at save time;
    -- buildings landing in not-yet-loaded chunks will simply not render
    -- until those chunks come in via drainInitQueues.
    do
        currentBm ← readIORef (buildingManagerRef env)
        let (restored, orphans) =
                fromBuildingSnapshot pageId (bmDefs currentBm) (sdBuildings saveData)
            -- #191: the snapshot owns only THIS page. Replace just this
            -- page's slice of the global manager and keep other live
            -- pages' buildings intact — a wholesale write dropped them.
            offPage = HM.difference (bmInstances currentBm)
                                    (buildingsOnPage pageId (bmInstances currentBm))
            merged  = restored
                { bmInstances = HM.union (bmInstances restored) offPage
                -- Don't let the saved counter reuse an off-page id.
                , bmNextId    = max (bmNextId restored) (bmNextId currentBm)
                }
        writeIORef (buildingManagerRef env) merged
        forM_ orphans $ \bid →
            logWarn logger CatWorld $
                "Save load: dropping building id="
                  <> T.pack (show (unBuildingId bid))
                  <> " — its def is no longer registered"
        -- Player-facing summary via the existing onWorldGenLog pathway.
        -- Per-id detail stays in the engine log; one toast per category
        -- is enough for the player to notice "something dropped".
        case length orphans of
            0 → pure ()
            n → sendGenLog env $
                    "Save load: dropped " <> T.pack (show n)
                    <> " building" <> (if n == 1 then "" else "s")
                    <> " (def no longer registered)"

    -- v5 (Phase 4): restore units + sim state. Same orphan handling as
    -- buildings; sim states for orphaned uids are dropped.
    --
    -- Race note: there's a window between writing unitManagerRef (next
    -- line) and writing utsRef (line below). The unit thread keeps
    -- running publishToRender every tick during load — it only gates
    -- tickAllMovement on enginePausedRef, not the publish step. If
    -- publishToRender reads in between our two writes, it sees post-
    -- load instances with pre-load simStates, but its `Nothing → inst`
    -- fallback (Unit/Thread.hs:107-108) keeps any uid not in the
    -- simStates map unchanged. So the visible result is one frame of
    -- the loaded animation state without sim-driven overrides —
    -- imperceptible. A future reader that requires stricter consistency
    -- (e.g. asserts simStates ⊆ umInstances) would need either an
    -- explicit lock or merging both refs into a single IORef.
    do
        currentUm ← readIORef (unitManagerRef env)
        let (restoredUm, orphanUnits) =
                fromUnitSnapshot pageId (umDefs currentUm) (sdUnits saveData)
            liveUids = HM.keysSet (umInstances restoredUm)
            -- Drop sim states whose owning unit was orphaned.
            simStates' = HM.filterWithKey (\uid _ → uid `HS.member` liveUids)
                                          (sdUnitSimStates saveData)
            -- #191: keep units (and their sim states) belonging to OTHER
            -- live pages; replace only this page's slice of the global
            -- manager. A wholesale write dropped off-page units.
            offPageU    = HM.difference (umInstances currentUm)
                                        (unitsOnPage pageId (umInstances currentUm))
            offPageUids = HM.keysSet offPageU
            mergedUm    = restoredUm
                { umInstances = HM.union (umInstances restoredUm) offPageU
                -- Don't let the saved counter reuse an off-page id.
                , umNextId    = max (umNextId restoredUm) (umNextId currentUm)
                }
        writeIORef (unitManagerRef env) mergedUm
        atomicModifyIORef' (utsRef env) $ \old →
            -- Preserve off-page units' sim states; replace this page's.
            let keptSim = HM.filterWithKey (\uid _ → uid `HS.member` offPageUids)
                                           (utsSimStates old)
            in (UnitThreadState { utsSimStates = HM.union simStates' keptSim }, ())
        forM_ orphanUnits $ \uid →
            logWarn logger CatWorld $
                "Save load: dropping unit id="
                  <> T.pack (show (unUnitId uid))
                  <> " — its def is no longer registered"
        case length orphanUnits of
            0 → pure ()
            n → sendGenLog env $
                    "Save load: dropped " <> T.pack (show n)
                    <> " unit" <> (if n == 1 then "" else "s")
                    <> " (def no longer registered)"

    -- 6. Set camera z-slice from saved camera position. elevationAtGlobal
    -- takes grid coords (gx, gy), not world coords — go through
    -- worldToGrid so this matches the convention used everywhere else
    -- (Render.hs:136, Quads.hs:397, etc.).
    let (camGX, camGY) = worldToGrid (sdCameraFacing saveData)
                                     (sdCameraX saveData)
                                     (sdCameraY saveData)
        (surfaceElev, _mat) =
            elevationAtGlobal seed (wgpPlates params) worldSize camGX camGY
        startZSlice = surfaceElev + surfaceHeadroom
    -- Use record-construction (not record-update) so every Camera2D
    -- field is set explicitly. Record-update would silently preserve
    -- the prior camera's transient state — non-zero pan/zoom velocity,
    -- mid-drag flag with stale drag origin — which the next render
    -- frame would then act on. Construction also makes the compiler
    -- error if Camera2D gains a new field, forcing a deliberate
    -- decision at load time.
    atomicModifyIORef' (cameraRef env) $ \_ →
        (Camera2D
            { camPosition     = (sdCameraX saveData, sdCameraY saveData)
            , camVelocity     = (0, 0)
            , camZoom         = sdCameraZoom saveData
            , camZoomVelocity = 0
            , camRotation     = 0
            , camFacing       = sdCameraFacing saveData
            , camDragging     = False
            , camDragOrigin   = (0, 0)
            , camZSlice       = startZSlice
            , camZTracking    = True
            }, ())

    let totalInitialChunks =
            (2 * chunkLoadRadius + 1) * (2 * chunkLoadRadius + 1)

    writeIORef phaseRef (LoadPhase2 (length remainingCoords) totalInitialChunks)
    sendGenLog env $ "Save loaded: "
        <> T.pack (show totalInitialChunks) <> " chunks queued"

    logInfo logger CatWorld $ "Save loaded: "
        <> T.pack (show totalInitialChunks) <> " chunks, "
        <> "surface at z=" <> T.pack (show surfaceElev)
        <> ": " <> unWorldPageId pageId
