{-# LANGUAGE Strict #-}

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
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logWarn, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Load.Types (StagedPage(..), StagedSession(..))
import Structure.Types (emptyChunkStructures)
import World.Generate (generateChunk, cameraChunkCoord)
import World.Generate.Arena (generateArenaChunks, arenaGenForSeed)
import World.Plant.Validate (revalidatePlantDesignations)
import World.Grid (worldToGrid)
import World.Chunk.Queue (initialChunkQueue)
import World.Chunk.Residency (canonicalChunkCoord)
import World.Chunk.Admit
    (claimChunkGeneration, publishSeedChunks, registerChunkDemand)
import World.Plate (elevationAtGlobal)
import World.Preview (buildPreviewFromPixels, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap.Cache (buildZoomCacheWithPixels)
import World.ZoomMap.ColorPalette (ZoomColorPalette, buildColorPalette)
import World.ZoomMap.ChunkTexture (buildZoomAtlas, ZoomAtlasData(..))
import World.Edit.Apply (replayEdits)
import World.Mine.Apply (applyDigSlopes)
import World.Construct.Apply (applyConstructSlopes)
import Building.Types (BuildingManager(..), BuildingId(..), BuildingDef)
import Building.Knowledge (prunedContainerIds, retainContainers)
import World.Save.Integrity
    ( pageEntitiesFrom, danglingOrderRefErrors, capIntegrityErrors
    , renderIntegrityReport, loadReconcileContextFrom )
-- The SaveData -> KnownEntities builder the Lua reference-edge
-- cross-validator already runs for this same load (issue #764). Reused
-- here rather than re-derived so the load-time "does this edge resolve?"
-- answer and the reconcile-time "should this reference be cleared?"
-- answer cannot drift apart (issue #1589).
import Engine.Scripting.Lua.API.Save.Integrity (knownEntitiesFromSaveData)
import Unit.Types (UnitManager(..), UnitId, UnitDef)
import Unit.Faction (fallbackFaction, factionTag)
import Unit.Sim.Types (UnitSimState)
import World.Material (MaterialRegistry)
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
    , psrUnitUnknownFactions ∷ ![Text]
      -- ^ Distinct unrecognized faction tags this page's units carried
      --   (#912). Those units load as 'Unit.Faction.fallbackFaction';
      --   the session aggregate warns once per distinct tag.
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
--   against arrives as a PARAMETER — it's the SAME
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
            -- Once per DISTINCT tag across the whole load transaction,
            -- however many units or pages carried it (#912). Those units
            -- are already loaded as the inert fallback — this is a
            -- diagnostic, never a load failure.
            unknownFactions = L.sort $ HS.toList $ HS.fromList $
                                concatMap psrUnitUnknownFactions results
        forM_ unknownFactions $ \tag →
            logWarn logger CatWorld $
                "Save load: unrecognized unit faction tag '" <> tag
                <> "' — those units load as '"
                <> factionTag fallbackFaction <> "'"
        if not (null buildingOrphans) ∨ not (null unitOrphans)
          then pure $ Left $ StageError $
                 "internal error: staging produced "
                 <> tshow (length buildingOrphans)
                 <> " orphaned building(s) / "
                 <> tshow (length unitOrphans)
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
                -- #1670: keep the atlas paired with the id of the page
                -- whose own zoom cache produced it. Only one staged
                -- page builds atlas pixels (the active one, below),
                -- but every non-arena page builds its own cache, so
                -- publish must know WHICH page this belongs to rather
                -- than handing it to all of them.
                mZoomAtlas = listToMaybe
                    [ (spPageId (psrPage r), w, h, bytes)
                    | r ← results, Just (w, h, bytes) ← [psrZoomAtlas r] ]
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
                    , ssReconcile     = loadReconcileContextFrom
                                          (knownEntitiesFromSaveData saveData)
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
    -- Issue #763: craft bills / power nodes are restored VERBATIM,
    -- never filtered against the save's own building snapshot. A
    -- bill/node whose station/building instance is absent (demolished
    -- before the save was ever taken) is EXPLICITLY documented,
    -- tolerated gameplay state per the #758 contract — "a demolished
    -- station's bills lingering, visible + cancellable"
    -- (docs/persistence_state_inventory.md) — not corruption to clean
    -- up. Pinned by "dangling craft bills / power nodes survive
    -- staging" in Test.Headless.World.Identity.
    writeIORef (wsCraftBillsRef worldState) (wpsCraftBills wps)
    writeIORef (wsPowerNodesRef worldState) (wpsPowerNodes wps)

    -- #1246: transfer orders restore VERBATIM for exactly the same
    -- reason, and get the diagnostic requirement 5 asks for on the way
    -- through. An order whose carrier died, whose destination was
    -- demolished, or whose item was consumed before the save was ever
    -- taken is TOLERATED gameplay, not corruption — it stays visible and
    -- cancellable like a bill does — so it is logged and KEPT, never
    -- pruned and never a load failure. (A reference that resolves on a
    -- DIFFERENT page is the separate, genuinely fatal case, already
    -- rejected by "World.Save.Integrity" before staging can run.) The
    -- resolution set comes from this page's OWN restored entities via
    -- the same 'pageEntitiesFrom' the pre-save boundary uses, so the two
    -- boundaries cannot disagree about what counts as present.
    let orderEntities = pageEntitiesFrom wpsGroundItems wpsUnits
                                         wpsBuildings wps
        danglingOrders = danglingOrderRefErrors pid orderEntities
                             (wpsTransferOrders wps)
    forM_ (renderIntegrityReport (capIntegrityErrors danglingOrders)) $ \m →
        logInfo logger CatWorld $
            "Save load: transfer-order integrity diagnostic: " <> m
    writeIORef (wsTransferOrdersRef worldState) (wpsTransferOrders wps)

    -- #1087: container knowledge is the ONE page-scoped layer that is
    -- deliberately NOT restored verbatim. A bill or node whose building
    -- is gone stays visible and cancellable, so keeping it is the
    -- player-facing right answer; a MEMORY of a container that no
    -- longer exists has no surface at all and nothing would ever clear
    -- it, so it is scrubbed here against this page's own restored
    -- building set. That is a tolerated, non-blocking DIAGNOSTIC — a
    -- demolished cargo's lingering memory is gameplay, not corruption
    -- (the same judgement "World.Save.Integrity" applies to a dangling
    -- reference) — never a load failure, so it is logged and dropped
    -- rather than reported.
    let liveBuildings = HM.keysSet (bsnInstances (wpsBuildings wps))
        staleKnowledge = prunedContainerIds liveBuildings
                             (wpsContainerKnowledge wps)
    unless (null staleKnowledge) $
        logInfo logger CatWorld $
            "Save load: dropping " <> tshow (length staleKnowledge)
            <> " container-knowledge record(s) on page "
            <> unWorldPageId pid <> " whose building no longer exists ("
            <> T.intercalate ", "
                   [ tshow (unBuildingId b) | b ← staleKnowledge ]
            <> ")"
    writeIORef (wsContainerKnowledgeRef worldState)
        (retainContainers liveBuildings (wpsContainerKnowledge wps))

    (simSeeds, locStamps, mCamera, mZoomAtlas, mPreview) ←
      if isArenaParams params
        then do
          when isActive $ writeIORef phaseRef (LoadPhase1 2 totalSteps)
          edits   ← readIORef (wsEditsRef worldState)
          desigs  ← readIORef (wsMineDesignationsRef worldState)
          cdesigs ← readIORef (wsConstructDesignationsRef worldState)
          -- #1718: the base comes from the LOADED page's own recorded
          -- seed, never a constant written in here. The save stores gen
          -- params and the edit overlay, not the base tile grid, so an
          -- untouched surface tile's grass variant is RECONSTRUCTED —
          -- generating it from any other value re-rolls every one of
          -- them on load.
          let arenaChunks = map ( applyConstructSlopes cdesigs
                                . applyDigSlopes desigs . replayEdits edits)
                                (generateArenaChunks (arenaGenForSeed seed))
              chunkMap = HM.fromList [ (lcCoord c, c) | c ← arenaChunks ]
          -- Claimed before the generation is forced, then admitted with
          -- that same claim — the lifecycle and the owner-before-payloads
          -- order a fresh arena uses (#2001). This staged WorldState is
          -- not published until World.Load.Publish, so nothing can
          -- observe either window here; sharing the one shape is what
          -- keeps the two seed paths from drifting, not a defence this
          -- one needs.
          arenaClaims ← claimChunkGeneration worldState pid params
                                             (map lcCoord arenaChunks)
          _ ← evaluate (force arenaChunks)
          publishSeedChunks worldState arenaClaims
              WorldTileData { wtdChunks = chunkMap, wtdMaxChunks = 100 }
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
          -- The SAVED camera chunk, canonicalised (#2001).
          -- 'cameraChunkCoord' does no wrapping, so a session saved past
          -- the seam names an ALIAS — and this centre is generated and
          -- inserted straight into wsTilesRef under whatever coord it is
          -- given. Storing it raw would put the page's one synchronously
          -- loaded chunk somewhere every canonicalising reader misses,
          -- and leave the camera loader generating a SECOND copy of the
          -- same physical chunk under the canonical key. Identity for
          -- every restore that is not near the seam, and for arena and
          -- zero-size pages.
          -- Claimed before generation, exactly as fresh world init does:
          -- a staged page cannot be reached by a request, but the two
          -- seed paths run the same lifecycle so neither can drift.
          let centerCoord = canonicalChunkCoord params $
                  cameraChunkCoord (wpsCameraFacing wps)
                                   (wpsCameraX wps)
                                   (wpsCameraY wps)
          centreClaims ← claimChunkGeneration worldState pid params
                                              [centerCoord]
          let (ct, cs, cterrain, cf, cice, cflora, cwt, cmagma) =
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
          -- The restored centre is new residency (#2001), claimed and
          -- admitted exactly as a fresh world's centre is.
          publishSeedChunks worldState centreClaims
              WorldTileData { wtdChunks    = HM.singleton centerCoord centerChunk
                            , wtdMaxChunks = 200 }
          let seeds = [ (centerCoord, lcFluidMap centerChunk
                        , lcTerrainSurfaceMap centerChunk) ]
              stamps = locationStampsFor params [centerChunk]

          -- The load-radius box around the SAVED camera chunk, counted
          -- as PHYSICAL chunks — a session saved near the seam restores
          -- a box that names one chunk twice, and this total is what
          -- LoadPhase2 progresses towards (#1723). Shared with fresh
          -- world init so the two seed the queue identically; the
          -- synchronously generated centre is excluded from the queue
          -- and counted once in the total.
          let (remainingCoords, totalInitialChunks) =
                  initialChunkQueue (canonicalChunkCoord params) centerCoord
          when isActive $ writeIORef phaseRef (LoadPhase1 4 totalSteps)
          -- Register the restored box as durable demand, then append
          -- exactly what still needs scheduling (#2001), exactly as
          -- fresh world init does. This staged page is not published
          -- until World.Load.Publish, so nothing can race the queue
          -- here — the append is the same shape as init's so the two
          -- cannot drift, not a defence this path needs.
          needed ← registerChunkDemand worldState pid params remainingCoords
          queuedNow ← atomicModifyIORef' (wsInitQueueRef worldState) $ \q →
              let q' = q ⧺ needed in (q', length q')
          writeIORef phaseRef (LoadPhase2 queuedNow totalInitialChunks)

          mCam ← if isActive
            then do
              let (camGX, camGY) = worldToGrid (wpsCameraFacing wps)
                                               (wpsCameraX wps)
                                               (wpsCameraY wps)
                  (surfaceElev, _mat) =
                      elevationAtGlobal seed (wgpPlates params) worldSize camGX camGY
                  startZSlice = surfaceElev + surfaceHeadroom
              logInfo logger CatWorld $ "Save loaded: "
                  <> tshow totalInitialChunks <> " chunks, "
                  <> "surface at z=" <> tshow surfaceElev
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

    -- #1858: the third publication boundary, and the one a save has to
    -- pass through. Designations restore VERBATIM above — the record
    -- keeps its serialized shape and its "Persist exactly"
    -- classification — and are reconciled HERE against the terrain this
    -- page just reconstructed, so a save whose ground is no longer
    -- tilled cannot restore a stranded designation. The arena branch
    -- rebuilt every chunk and the ordinary branch has only its centre,
    -- so most records are still UNKNOWN at this point and are retained
    -- for 'World.Thread.ChunkLoading' to resolve as the queue drains.
    _ ← revalidatePlantDesignations logger worldState

    let (restoredBm, bOrphans) = fromBuildingSnapshot pid buildingDefs (wpsBuildings wps)
        (restoredUm, uOrphans, uUnknownFactions) =
            fromUnitSnapshot pid unitDefs (wpsUnits wps)
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
        , psrUnitUnknownFactions = uUnknownFactions
        , psrUnitSimStates   = simStates'
        , psrCamera          = mCamera
        , psrZoomAtlas       = mZoomAtlas
        , psrPreview         = mPreview
        }
