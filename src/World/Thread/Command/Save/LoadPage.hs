{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Per-page save-load restoration: rebuilds one saved 'WorldPageSave'
--   into a live 'WorldState' (zoom cache, center chunk, chunk queue, or
--   the arena rebuild special-case) and resolves its building/unit
--   slices. Split out of "World.Thread.Command.Save" (issue #561) — the
--   forM-loop body of the historical single-page load path, now a
--   standalone per-page worker called once per saved page by
--   "World.Thread.Command.Save.LoadWorld".
module World.Thread.Command.Save.LoadPage
    ( restoreSavedPage
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
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
import Engine.Core.Log (logInfo, logWarn, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import Structure.Types (emptyChunkStructures)
import World.Generate (generateChunk, cameraChunkCoord)
import World.Generate.Arena (generateArenaChunks)
import World.Grid (worldToGrid)
import World.Generate.Constants (chunkLoadRadius)
import System.Random (mkStdGen)
import World.Plate (elevationAtGlobal)
import World.Preview (buildPreviewFromPixels, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCacheWithPixels)
import World.ZoomMap.ColorPalette (ZoomColorPalette)
import World.ZoomMap.ChunkTexture (buildZoomAtlas, ZoomAtlasData(..))
import World.Edit.Apply (replayEdits)
import World.Mine.Apply (applyDigSlopes)
import World.Construct.Apply (applyConstructSlopes)
import Craft.Bills (pruneToStations)
import Power.Types (pruneToBuildings)
import Building.Types (BuildingManager(..), BuildingId)
import Unit.Types (UnitManager(..), UnitId)
import Unit.Sim.Types (UnitSimState)
import World.Material (MaterialRegistry)
import World.Thread.Helpers (sendGenLog, unWorldPageId)
import World.Thread.ChunkLoading (dispatchLocationStamps)

-- | Restore one saved page into a live WorldState: gen params + mutable
--   game state, zoom cache + center chunk + queued remainder (or the
--   arena rebuild special-case), and the resolved building/unit slices
--   for the caller's manager merge. 'restoreId' maps a saved page to its
--   collision-free restore id (see
--   "World.Thread.Command.Save.RestoreIds"); 'activeWpsId' is the saved
--   active page's own id, used to decide which page drives the global
--   camera/zoom-atlas/preview staging.
restoreSavedPage
    ∷ EngineEnv
    → LoggerState
    → MaterialRegistry
    → ZoomColorPalette
    → FloraCatalog
    → BuildingManager
    → UnitManager
    → (WorldPageSave → WorldPageId)
    → WorldPageId
    → WorldPageSave
    → IO (WorldPageId, BuildingManager, [BuildingId], UnitManager, [UnitId]
         , HM.HashMap UnitId UnitSimState)
restoreSavedPage env logger registry palette catalog currentBm currentUm
                 restoreId activeWpsId wps = do
        let rid       = restoreId wps
            isActive  = wpsPageId wps ≡ activeWpsId
            params    = wpsGenParams wps
            seed      = wgpSeed params
            worldSize = wgpWorldSize params

        logInfo logger CatWorld $ "Restoring saved page: "
            <> unWorldPageId (wpsPageId wps) <> " → " <> unWorldPageId rid
        -- Loudly flag a forced rename: a non-active page whose saved id
        -- collided with the active page's main_world remap kept its data but
        -- restores under a synthetic id (see 'assignRestoreIds').
        when (not isActive ∧ rid ≢ wpsPageId wps) $
            logWarn logger CatWorld $
                "Save load: page '" <> unWorldPageId (wpsPageId wps)
                <> "' collides with the active page's restore id; restoring it "
                <> "under '" <> unWorldPageId rid <> "' instead"

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

        -- Discard any sim (fluid) state still held under this restore id. A
        -- within-session load over an existing page (e.g. main_world) replaces
        -- its WorldState, but the sim thread keeps a page's chunks across
        -- deactivate/activate and reuses them on the next show — so without
        -- this drop the restored world would inherit STALE pre-load fluid
        -- chunks. The fresh chunks re-populate sim below (center chunk) and via
        -- drainInitQueues (the queued remainder). A no-op for a fresh-session
        -- load (no state under that id).
        Q.writeQueue (simQueue env) (SimDropWorld rid)

        -- 1a. Restore gen params (plates, timeline, ocean map, climate are
        --     all baked inside) + the per-page mutable game state.
        when isActive $ writeIORef phaseRef (LoadPhase1 1 totalSteps)
        when isActive $ sendGenLog env "Loading saved world state..."
        writeIORef (wsGenParamsRef worldState) (Just params)
        -- Player-facing identity (#707): follows the PAGE, not its id —
        -- an active page restoring as main_world (or a collision-renamed
        -- "<id>#N" page) keeps the identity it was saved with.
        writeIORef (wsIdentityRef worldState) (wpsIdentity wps)
        writeIORef (wsCameraRef worldState)
            (WorldCamera (wpsCameraX wps) (wpsCameraY wps))
        writeIORef (wsTimeRef worldState)
            (WorldTime (wpsTimeHour wps) (wpsTimeMinute wps))
        writeIORef (wsDateRef worldState)
            (WorldDate (wpsDateYear wps) (wpsDateMonth wps) (wpsDateDay wps))
        -- Restore each page's real saved clock speed (the player's chosen
        -- scale). The world is still frozen while paused — tickWorldTime gates
        -- advancement on enginePausedRef and only ticks wmVisible worlds — so
        -- holding the live speed here (rather than zeroing) costs no drift but
        -- lets scripts/pause.lua's onSaveLoaded read the ACTIVE world's real
        -- speed into prevTimeScale, so a resume restores THAT page's speed (not
        -- a stale global value) regardless of which page became main_world (#42,
        -- #214). pause.onSaveLoaded then zeros the active clock to mirror a
        -- normal pause; background pages keep their speed for when shown.
        writeIORef (wsTimeScaleRef worldState) (wpsTimeScale wps)
        writeIORef (wsMapModeRef worldState) (wpsMapMode wps)
        -- A loaded world starts on the default tool, NOT the tool that was
        -- active at save time. The HUD toolbar always comes up on the default
        -- slot after a load, so the engine ToolMode must match or
        -- world.getToolMode() and the toolbar would disagree. wpsToolMode is
        -- still recorded at save time but intentionally not restored. (#103)
        writeIORef (wsToolModeRef worldState) DefaultTool
        -- Restore edit log BEFORE chunk generation so the synchronous center
        -- chunk + every chunk pulled off the init queue replay the player's
        -- edits. Mine / construct designations, ground items, and spoil piles
        -- render from stored data and need no chunk loading first.
        writeIORef (wsEditsRef worldState) (wpsEdits wps)
        writeIORef (wsMineDesignationsRef worldState) (wpsMineDesignations wps)
        writeIORef (wsConstructDesignationsRef worldState)
            (wpsConstructDesignations wps)
        writeIORef (wsGroundItemsRef worldState) (wpsGroundItems wps)
        writeIORef (wsSpoilRef worldState) (wpsSpoilPiles wps)
        -- Flora harvest state (#94): like designations, timers render/query
        -- from the world-level map and need no chunk loading first.
        writeIORef (wsFloraHarvestsRef worldState) (wpsFloraHarvests wps)
        -- Chop designations (#97): markers render from the stored z.
        writeIORef (wsChopDesignationsRef worldState) (wpsChopDesignations wps)
        -- Till designations (#333): same shape, markers render from the
        -- stored z.
        writeIORef (wsTillDesignationsRef worldState) (wpsTillDesignations wps)
        -- Planted crop plots (#334): same shape, growth/texture derive
        -- from the stored species + planted day with no chunk loading.
        writeIORef (wsCropPlotsRef worldState) (wpsCropPlots wps)
        -- Plant designations (#335): markers render from the stored z.
        writeIORef (wsPlantDesignationsRef worldState)
            (wpsPlantDesignations wps)
        -- Craft bills (#329): stations restore under their saved
        -- BuildingIds (see 1f), so bills reconnect verbatim; prune the
        -- ones whose station isn't in this page's snapshot (a def
        -- deregistered between sessions orphans the building, which is
        -- dropped — its bills must go too).
        writeIORef (wsCraftBillsRef worldState)
            (pruneToStations (HM.keysSet (bsnInstances (wpsBuildings wps)))
                             (wpsCraftBills wps))
        -- Power nodes (#358): same reconnect-by-BuildingId + prune
        -- pattern as craft bills above.
        writeIORef (wsPowerNodesRef worldState)
            (pruneToBuildings (HM.keysSet (bsnInstances (wpsBuildings wps)))
                              (wpsPowerNodes wps))

        -- #365: an arena page (world.initArena) carries SYNTHETIC gen params
        -- (wgpSeed 0, empty timeline, no plates, worldSize 100000) that the
        -- real pipeline cannot run: buildZoomCacheWithPixels / generateChunk
        -- make no progress on them (world thread wedges, stranding every
        -- later page of the restore), and 1e's elevationAtGlobal errors on
        -- the empty plate list. Rebuild the flat arena exactly like
        -- handleWorldInitArenaCommand instead, then replay this page's
        -- saved edits onto it.
        if isArenaParams params
          then do
            when isActive $ writeIORef phaseRef (LoadPhase1 2 totalSteps)
            when isActive $ sendGenLog env "Rebuilding arena page..."
            edits  ← readIORef (wsEditsRef worldState)
            desigs ← readIORef (wsMineDesignationsRef worldState)
            cdesigs ← readIORef (wsConstructDesignationsRef worldState)
            -- Fixed StdGen: the gen only varies cosmetic surface grass, and
            -- a fixed seed keeps repeated loads of the same save identical.
            let arenaChunks = map ( applyConstructSlopes cdesigs
                                  . applyDigSlopes desigs . replayEdits edits)
                                  (generateArenaChunks (mkStdGen 0))
                chunkMap = HM.fromList [ (lcCoord c, c) | c ← arenaChunks ]
            _ ← evaluate (force arenaChunks)
            atomicModifyIORef' (wsTilesRef worldState) $ \_ →
                (WorldTileData { wtdChunks = chunkMap, wtdMaxChunks = 100 }, ())
            -- Seed the sim thread with every eager chunk: they bypass the
            -- init queue, so drainInitQueues never emits SimChunkLoaded for
            -- them, and replayed fluid edits (movement_arena water/lava)
            -- must reach the fluid sim.
            forM_ arenaChunks $ \c →
                Q.writeQueue (simQueue env) $
                    SimChunkLoaded rid (lcCoord c)
                        (lcFluidMap c) (lcTerrainSurfaceMap c)
            writeIORef (wsInitQueueRef worldState) []
            writeIORef phaseRef LoadDone
            -- Arena analogue of 1e: flat ground at seaLevel; no
            -- elevationAtGlobal (it errors on the arena's empty plates).
            when isActive $ do
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
                        , camZSlice       = seaLevel + surfaceHeadroom
                        , camZTracking    = True
                        }, ())
                sendGenLog env "Save loaded: arena page rebuilt"
                logInfo logger CatWorld $
                    "Save loaded: arena page rebuilt: " <> unWorldPageId rid
          else do
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
            cdesigs ← readIORef (wsConstructDesignationsRef worldState)
            let centerChunk = applyConstructSlopes cdesigs
                    (applyDigSlopes desigs (replayEdits edits centerChunkRaw))
            atomicModifyIORef' (wsTilesRef worldState) $ \_ →
                (WorldTileData { wtdChunks    = HM.singleton centerCoord centerChunk
                               , wtdMaxChunks = 200 }, ())
            -- Seed sim with the synchronously-loaded center chunk. drainInitQueues
            -- emits SimChunkLoaded only for the QUEUED remainder (below), so the
            -- center — written straight to wsTilesRef, not queued — would otherwise
            -- never reach the sim thread (after the SimDropWorld above cleared any
            -- stale copy). Matches the per-chunk message the queue loader sends.
            Q.writeQueue (simQueue env) $
                SimChunkLoaded rid centerCoord
                    (lcFluidMap centerChunk) (lcTerrainSurfaceMap centerChunk)

            -- Stamp any placed location on the synchronously-regenerated centre
            -- chunk (#89). Like Init's centre chunk it is written straight to
            -- wsTilesRef and excluded from the init queue, so the chunk-loading
            -- dispatch never sees it. A location saved un-stamped on the camera
            -- chunk thus still materializes on load.
            dispatchLocationStamps env params rid [centerChunk]

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
