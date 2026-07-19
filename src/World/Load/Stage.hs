{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Whole-session LOAD staging (issue #763, save-overhaul C2): rebuild
--   every saved page into a fresh 'World.State.Types.WorldState' (zoom
--   cache, center chunk, chunk queue, or the arena-rebuild special case)
--   plus the merged building/unit managers, entirely WITHOUT touching any
--   live 'Engine.Core.State.EngineEnv' ref (requirement 6). The
--   deliberate replacement for the old incrementally-mutating
--   "World.Thread.Command.Save.LoadPage" / "…LoadWorld" pair, which
--   registered each page into the live 'Engine.Core.State.worldManagerRef'
--   and queued sim/Lua work as it went.
--
--   Saved page ids are preserved EXACTLY (requirement 8) — there is no
--   restore-id remapping, no forced "main_world" rename, and no "<id>#N"
--   collision suffix: loading REPLACES the complete session, so a saved
--   page's own id can never collide with anything that survives to
--   publish. "World.Load.Publish" performs the actual live-ref swap
--   once staging succeeds and fires every deferred sim-seed /
--   location-stamp this module collects instead of sending.
module World.Load.Stage
    ( stageSession
    , StageError(..)
    , renderStageError
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Load.Types (StagedPage(..), StagedSession(..))
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
import World.ZoomMap.ColorPalette (ZoomColorPalette, buildColorPalette)
import World.ZoomMap.ChunkTexture (buildZoomAtlas, ZoomAtlasData(..))
import World.Edit.Apply (replayEdits)
import World.Mine.Apply (applyDigSlopes)
import World.Construct.Apply (applyConstructSlopes)
import Building.Types (BuildingManager(..), BuildingId, BuildingDef)
import Unit.Types (UnitManager(..), UnitId, UnitDef)
import Unit.Sim.Types (UnitSimState)
import World.Material (MaterialRegistry)
import World.Thread.Helpers (unWorldPageId)
import World.Thread.ChunkLoading (locationStampsFor)

newtype StageError = StageError Text deriving (Eq, Show)

renderStageError ∷ StageError → Text
renderStageError (StageError t) = t

-- | One page's staged result, plus the pieces 'stageSession' folds into
--   the whole-session aggregate. A plain record (not exported) rather
--   than a wide tuple, purely for readability at the call site.
data PageStageResult = PageStageResult
    { psrPage        ∷ !StagedPage
    , psrBuildings   ∷ !BuildingManager
    , psrBuildingOrphans ∷ ![BuildingId]
    , psrUnits       ∷ !UnitManager
    , psrUnitOrphans ∷ ![UnitId]
    , psrUnitSimStates ∷ !(HM.HashMap UnitId UnitSimState)
    , psrCamera      ∷ !(Maybe Camera2D)
    , psrZoomAtlas   ∷ !(Maybe (Int, Int, BS.ByteString))
    , psrPreview     ∷ !(Maybe (Int, Int, BS.ByteString))
    }

-- | Stage the complete replacement session from a decoded, already
--   content-validated 'SaveData'. Never touches a live 'EngineEnv' ref
--   at all (requirement 6 — staging must not send work through a live
--   queue either, so unlike the pre-#763 restore this never calls
--   'World.Thread.Helpers.sendGenLog'): the material registry to stage
--   against arrives as a PARAMETER — round 6 review: it's the SAME
--   off-session registry 'Engine.Scripting.Lua.API.Save.continueLoad'
--   already built and validated the save's material references
--   against (never the live 'Engine.Core.State.materialRegistryRef',
--   which "World.Load.Publish" only writes at actual commit) — while
--   'env' is still read for the OTHER registered (session-independent)
--   content: the flora catalog and the currently-registered
--   building/unit DEFS. "World.Load.Publish" fires the one user-facing
--   "Save loaded" toast once the session actually publishes.
stageSession ∷ EngineEnv → LoggerState → SaveData → MaterialRegistry
             → IO (Either StageError StagedSession)
stageSession env logger saveData registry = case sdWorlds saveData of
    [] → pure $ Left $ StageError
            "cannot stage: save contains no world pages"
    (firstWps : _) → do
        let !_ = registry `seq` ()
        palette ← buildColorPalette logger "data/materials" "data/vegetation"
        _ ← evaluate (force palette)
        catalog ← readIORef (floraCatalogRef env)
        buildingDefs ← bmDefs <$> readIORef (buildingManagerRef env)
        unitDefs     ← umDefs <$> readIORef (unitManagerRef env)

        let activeWps    = fromMaybe firstWps (activeWorldPage saveData)
            activeWpsId  = wpsPageId activeWps
            orderedPages = filter ((≢ activeWpsId) . wpsPageId) (sdWorlds saveData)
                             ⧺ [activeWps]

        results ← forM orderedPages $
            stagePage logger registry palette catalog
                      buildingDefs unitDefs activeWpsId

        let buildingOrphans = concatMap psrBuildingOrphans results
            unitOrphans     = concatMap psrUnitOrphans results
        if not (null buildingOrphans) ∨ not (null unitOrphans)
          then pure $ Left $ StageError $
                 "internal error: staging produced "
                 <> T.pack (show (length buildingOrphans))
                 <> " orphaned building(s) / "
                 <> T.pack (show (length unitOrphans))
                 <> " orphaned unit(s) after content validation already "
                 <> "passed — aborting rather than silently dropping them"
          else do
            let mergedBuildings = HM.unions (map (bmInstances . psrBuildings) results)
                mergedUnits     = HM.unions (map (umInstances . psrUnits) results)
                mergedSimStates = HM.unions (map psrUnitSimStates results)
                -- Every page's snapshot carries the SAME canonical
                -- session-wide allocator (issue #758's adapter — see
                -- 'World.Save.Snapshot.Adapter.pageToWorldPageSave')
                -- so any page's value is representative.
                nextBid = maybe 0 (bmNextId . psrBuildings) (listToMaybe results)
                nextUid = maybe 0 (umNextId . psrUnits) (listToMaybe results)
                finalBuildings = BuildingManager
                    { bmDefs = buildingDefs, bmInstances = mergedBuildings
                    , bmNextId = nextBid, bmSelected = Nothing }
                finalUnits = UnitManager
                    { umDefs = unitDefs, umInstances = mergedUnits
                    , umSelected = mempty, umNextId = nextUid }
                mCamera    = listToMaybe [ c | Just c ← map psrCamera results ]
                mZoomAtlas = listToMaybe [ z | Just z ← map psrZoomAtlas results ]
                mPreview   = listToMaybe [ p | Just p ← map psrPreview results ]
            pure $ case mCamera of
                -- Every staged session resolves exactly one active page
                -- (the fallback above), which always stages a camera —
                -- 'Nothing' here would mean 'orderedPages''s active-last
                -- ordering broke, an internal invariant violation rather
                -- than a real load failure. Reject cleanly instead of
                -- fabricating a camera value.
                Nothing → Left $ StageError
                    "internal error: no page staged as the active page"
                Just camera → Right StagedSession
                    { ssPages         = map psrPage results
                    , ssActivePage    = activeWpsId
                    , ssVisiblePages  = sdVisiblePages saveData
                    , ssBuildings     = finalBuildings
                    , ssUnits         = finalUnits
                    , ssUnitSimStates = mergedSimStates
                    , ssGameTime      = sdGameTime saveData
                    , ssTexPalette    = sdTexPalette saveData
                    , ssNextItemId    = sdNextItemInstanceId saveData
                    , ssCamera        = camera
                    , ssZoomAtlas     = mZoomAtlas
                    , ssPreview       = mPreview
                    , ssMaterialRegistry = registry
                    }

-- | Stage one saved page: gen params + mutable game state (own fresh
--   IORefs), zoom cache + center chunk + queued remainder (or the arena
--   rebuild special case), and the resolved building/unit slices. Mirrors
--   the pre-#763 'World.Thread.Command.Save.LoadPage.restoreSavedPage'
--   almost line for line — the only changes are: no restore-id remap (the
--   page keeps its own saved id), and every live-ref write (manager
--   registration, sim-queue seeding, camera/zoom-atlas/preview upload,
--   location-stamp dispatch) becomes a value collected onto the result
--   instead, deferred to "World.Load.Publish".
stagePage
    ∷ LoggerState → MaterialRegistry → ZoomColorPalette
    → FloraCatalog → HM.HashMap Text BuildingDef → HM.HashMap Text UnitDef
    → WorldPageId → WorldPageSave → IO PageStageResult
stagePage logger registry palette catalog buildingDefs unitDefs
          activeWpsId wps = do
    let pid      = wpsPageId wps
        isActive = pid ≡ activeWpsId
        params    = wpsGenParams wps
        seed      = wgpSeed params
        worldSize = wgpWorldSize params

    logInfo logger CatWorld $ "Staging saved page: " <> unWorldPageId pid

    worldState ← emptyWorldState
    let phaseRef   = wsLoadPhaseRef worldState
        totalSteps = 4

    when isActive $ writeIORef phaseRef (LoadPhase1 1 totalSteps)
    writeIORef (wsGenParamsRef worldState) (Just params)
    writeIORef (wsIdentityRef worldState) (wpsIdentity wps)
    writeIORef (wsCameraRef worldState)
        (WorldCamera (wpsCameraX wps) (wpsCameraY wps))
    writeIORef (wsTimeRef worldState)
        (WorldTime (wpsTimeHour wps) (wpsTimeMinute wps))
    writeIORef (wsDateRef worldState)
        (WorldDate (wpsDateYear wps) (wpsDateMonth wps) (wpsDateDay wps))
    -- Never restore a player's previous simulation speed from a save.
    writeIORef (wsTimeScaleRef worldState) 1
    writeIORef (wsMapModeRef worldState) (wpsMapMode wps)
    -- A loaded world always starts on the default tool (#103).
    writeIORef (wsToolModeRef worldState) DefaultTool
    writeIORef (wsEditsRef worldState) (wpsEdits wps)
    writeIORef (wsMineDesignationsRef worldState) (wpsMineDesignations wps)
    writeIORef (wsConstructDesignationsRef worldState)
        (wpsConstructDesignations wps)
    writeIORef (wsGroundItemsRef worldState) (wpsGroundItems wps)
    writeIORef (wsSpoilRef worldState) (wpsSpoilPiles wps)
    writeIORef (wsFloraHarvestsRef worldState) (wpsFloraHarvests wps)
    writeIORef (wsChopDesignationsRef worldState) (wpsChopDesignations wps)
    writeIORef (wsTillDesignationsRef worldState) (wpsTillDesignations wps)
    writeIORef (wsCropPlotsRef worldState) (wpsCropPlots wps)
    writeIORef (wsPlantDesignationsRef worldState) (wpsPlantDesignations wps)
    -- Round 9 review (issue #763): craft bills / power nodes are
    -- restored VERBATIM, never pruned against the save's own building
    -- snapshot. A bill/node whose station/building instance is absent
    -- (demolished before the save was ever taken) is EXPLICITLY
    -- documented, tolerated gameplay state per the #758-era contract —
    -- "a demolished station's bills lingering, visible + cancellable"
    -- (docs/persistence_state_inventory.md) — not corruption to clean
    -- up. 'Craft.Bills.pruneToStations'/'Power.Types.pruneToBuildings'
    -- exist for a DIFFERENT scenario their own doc comments name (a
    -- station's building DEFINITION deregistered between sessions,
    -- orphaning every instance of that type) — but that scenario is
    -- already unreachable by the time staging runs at all: this
    -- module's caller (Engine.Scripting.Lua.API.Save.continueLoad)
    -- rejects the WHOLE load outright via missingDefReferences before
    -- staging ever starts if any building instance references an
    -- unregistered definition, so every wpsBuildings instance here is
    -- already guaranteed to resolve. Applying the prune here only ever
    -- catches the FIRST (tolerated) case, silently discarding bills/
    -- nodes #763 requires to be restored.
    writeIORef (wsCraftBillsRef worldState) (wpsCraftBills wps)
    writeIORef (wsPowerNodesRef worldState) (wpsPowerNodes wps)

    (simSeeds, locStamps, mCamera, mZoomAtlas, mPreview) ←
      if isArenaParams params
        then do
          when isActive $ writeIORef phaseRef (LoadPhase1 2 totalSteps)
          edits   ← readIORef (wsEditsRef worldState)
          desigs  ← readIORef (wsMineDesignationsRef worldState)
          cdesigs ← readIORef (wsConstructDesignationsRef worldState)
          let arenaChunks = map ( applyConstructSlopes cdesigs
                                . applyDigSlopes desigs . replayEdits edits)
                                (generateArenaChunks (mkStdGen 0))
              chunkMap = HM.fromList [ (lcCoord c, c) | c ← arenaChunks ]
          _ ← evaluate (force arenaChunks)
          atomicModifyIORef' (wsTilesRef worldState) $ \_ →
              (WorldTileData { wtdChunks = chunkMap, wtdMaxChunks = 100 }, ())
          let seeds = [ (lcCoord c, lcFluidMap c, lcTerrainSurfaceMap c)
                      | c ← arenaChunks ]
          writeIORef (wsInitQueueRef worldState) []
          writeIORef phaseRef LoadDone
          mCam ← if isActive
            then do
              logInfo logger CatWorld $
                  "Save loaded: arena page rebuilt: " <> unWorldPageId pid
              pure $ Just Camera2D
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
                  }
            else pure Nothing
          pure (seeds, [], mCam, Nothing, Nothing)
        else do
          when isActive $ writeIORef phaseRef (LoadPhase1 2 totalSteps)
          let (zoomCache, chunkPixels) =
                  buildZoomCacheWithPixels params registry palette Nothing
          _ ← evaluate (force zoomCache)
          writeIORef (wsZoomCacheRef worldState) zoomCache
          writeIORef (wsZoomAtlasRef worldState) Nothing
          (mZoomAtlasVal, mPreviewVal) ← if isActive
            then do
              _ ← evaluate (force chunkPixels)
              let atlas = buildZoomAtlas (V.length zoomCache) chunkPixels
              _ ← evaluate (force atlas)
              let preview = buildPreviewFromPixels params zoomCache chunkPixels
              _ ← evaluate (force preview)
              pure ( Just (zadWidth atlas, zadHeight atlas, zadPixelData atlas)
                   , Just (piWidth preview, piHeight preview, piData preview) )
            else pure (Nothing, Nothing)

          when isActive $ writeIORef phaseRef (LoadPhase1 3 totalSteps)
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
          edits   ← readIORef (wsEditsRef worldState)
          desigs  ← readIORef (wsMineDesignationsRef worldState)
          cdesigs ← readIORef (wsConstructDesignationsRef worldState)
          let centerChunk = applyConstructSlopes cdesigs
                  (applyDigSlopes desigs (replayEdits edits centerChunkRaw))
          atomicModifyIORef' (wsTilesRef worldState) $ \_ →
              (WorldTileData { wtdChunks    = HM.singleton centerCoord centerChunk
                             , wtdMaxChunks = 200 }, ())
          let seeds = [ (centerCoord, lcFluidMap centerChunk
                        , lcTerrainSurfaceMap centerChunk) ]
              stamps = locationStampsFor params [centerChunk]

          let remainingCoords =
                  [ ChunkCoord cx cy
                  | cx ← [camCX - chunkLoadRadius .. camCX + chunkLoadRadius]
                  , cy ← [camCY - chunkLoadRadius .. camCY + chunkLoadRadius]
                  , not (cx ≡ camCX ∧ cy ≡ camCY)
                  ]
              totalInitialChunks =
                  (2 * chunkLoadRadius + 1) * (2 * chunkLoadRadius + 1)
          when isActive $ writeIORef phaseRef (LoadPhase1 4 totalSteps)
          writeIORef (wsInitQueueRef worldState) remainingCoords
          writeIORef phaseRef (LoadPhase2 (length remainingCoords) totalInitialChunks)

          mCam ← if isActive
            then do
              let (camGX, camGY) = worldToGrid (wpsCameraFacing wps)
                                               (wpsCameraX wps)
                                               (wpsCameraY wps)
                  (surfaceElev, _mat) =
                      elevationAtGlobal seed (wgpPlates params) worldSize camGX camGY
                  startZSlice = surfaceElev + surfaceHeadroom
              logInfo logger CatWorld $ "Save loaded: "
                  <> T.pack (show totalInitialChunks) <> " chunks, "
                  <> "surface at z=" <> T.pack (show surfaceElev)
                  <> ": " <> unWorldPageId pid
              pure $ Just Camera2D
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
                  }
            else pure Nothing
          pure (seeds, stamps, mCam, mZoomAtlasVal, mPreviewVal)

    let (restoredBm, bOrphans) = fromBuildingSnapshot pid buildingDefs (wpsBuildings wps)
        (restoredUm, uOrphans) = fromUnitSnapshot pid unitDefs (wpsUnits wps)
        liveUids   = HM.keysSet (umInstances restoredUm)
        simStates' = HM.filterWithKey (\uid _ → uid `HS.member` liveUids)
                                      (wpsUnitSimStates wps)

    pure PageStageResult
        { psrPage = StagedPage
            { spPageId        = pid
            , spWorldState    = worldState
            , spSimSeeds      = simSeeds
            , spLocationStamps = locStamps
            }
        , psrBuildings       = restoredBm
        , psrBuildingOrphans = bOrphans
        , psrUnits           = restoredUm
        , psrUnitOrphans     = uOrphans
        , psrUnitSimStates   = simStates'
        , psrCamera          = mCamera
        , psrZoomAtlas       = mZoomAtlas
        , psrPreview         = mPreview
        }
