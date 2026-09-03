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
import Data.IORef (readIORef, writeIORef)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logWarn, logError, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Load.Types (StagedPage(..), StagedSession(..))
import Structure.Types (emptyChunkStructures)
import World.Generate (generateChunk, cameraChunkCoord)
import World.Flora.Designation (admitChunkFlora)
import World.Generate.Arena (generateArenaChunks, arenaGenForSeed)
import World.Plant.Validate (revalidatePlantDesignations)
import World.Grid (worldToGrid)
import World.Chunk.Queue (initialChunkQueue, seedInitialQueue)
import World.Chunk.Residency (canonicalChunkCoord)
import World.Chunk.Admit (claimChunkGeneration, publishSeedChunks)
import World.Plate (elevationAtGlobal)
import World.Preview (buildPreviewFromPixels, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap.Cache (buildZoomCacheWithPixels)
import World.ZoomMap.Artifact
    ( ZoomArtifact(..), buildZoomArtifactKey, loadZoomArtifact
    , publishZoomArtifact )
import World.ZoomMap.ColorPalette (ZoomColorPalette, buildColorPalette)
import World.ZoomMap.ChunkTexture (buildZoomAtlas, ZoomAtlasData(..))
import World.Map.ImagePlan
    ( MapImageCeiling, MapImageFormat(..)
    , MapImageSource(..), admitMapImage, mapImageRefusalText )
import Engine.Map.ImageAdmission (readMapImageCeiling)
import World.Edit.Apply (replayEdits)
import World.Edit.Types (canonicalizeWorldEdits, WorldEdits)
import World.Flora.CropPlot (CropPlots)
import World.Plant.Types (PlantDesignations)
import World.Mine.Apply (applyDigSlopes)
import World.Construct.Apply (applyConstructSlopes)
import World.Construct.Reconcile
    (ConstructReconcileError(..), reconcileStagedConstructDesignations)
import World.Construct.Revalidate
    ( ConstructRefundDeps, ConstructScope(..), constructStagingRefundDeps
    , revalidateStagedConstructDesignations )
import Structure.ArtCatalog (StructureArtCatalog)
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
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
    , psrStageError  ∷ !(Maybe StageError)
      -- ^ #2020: a map-image validation failure this page hit AFTER its
      --   plan was admitted — a chunk-block count or block size that
      --   disagrees with the plan 'buildZoomAtlas' was handed. Staging
      --   collects it rather than throwing, and 'stageSession' turns it
      --   into a whole-transaction 'StageError', so a load either
      --   publishes a complete session or publishes nothing.
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
        -- #1844: the registered structure art/build catalogue and the
        -- item-minting dependencies a self-clear's refund needs, read
        -- ONCE here and passed down as values. 'stagePage' deliberately
        -- never touches a live env ref (see this module's haddock), and
        -- the catalogue is session-independent content exactly like the
        -- building and unit defs above.
        artCatalog ← readIORef (rhStructureArtCatalogRef
                                  (toRenderHandoffCapability env))
        -- Seeded from the SAVE's own item-instance allocator, never the
        -- live one, and its final value is what the session publishes —
        -- see 'constructStagingRefundDeps' for why a staged refund
        -- drawing from the live counter corrupts both sessions.
        (refundDeps, stagedItemIdRef) ←
            constructStagingRefundDeps env (sdNextItemInstanceId saveData)

        let activeWps    = fromMaybe firstWps (activeWorldPage saveData)
            activeWpsId  = wpsPageId activeWps
            orderedPages = filter ((≢ activeWpsId) . wpsPageId) (sdWorlds saveData)
                             ⧺ [activeWps]

        -- #2020: admit EVERY non-arena page's map image before any page
        -- is staged. 'stagePage' calls 'buildZoomCacheWithPixels' for
        -- every such page — active or not — and that call generates and
        -- forces the page's whole 4096-byte-per-chunk pixel corpus, so
        -- the check has to run out here, ahead of the loop, not inside
        -- it. The device ceiling applies uniformly: an inactive page
        -- whose atlas could never be created fails the load even though
        -- this load would not have uploaded it, because it becomes the
        -- active page the moment the player switches to it.
        --
        -- A refusal fails the whole transaction through
        -- 'World.Load.Stage''s existing 'StageError' path, so
        -- "World.Load.Publish" never runs and the current session stays
        -- exactly as it was.
        mapCeiling ← readMapImageCeiling env
        let planRefusals =
                [ (wpsPageId wps, refusal)
                | wps ← orderedPages
                , not (isArenaParams (wpsGenParams wps))
                , Left refusal ←
                    [ admitMapImage mapCeiling MapImageRGBA8
                        (ZoomAtlasSource
                            (wgpWorldSize (wpsGenParams wps))) ]
                ]
        -- #2243: every page's durable flora references become runtime
        -- handles here, against the catalog read above — the first point
        -- in the load transaction that has one, which is why a pure
        -- component migration could only carry a pre-name payload's
        -- ordinals forward rather than resolve them
        -- ("World.Save.Component.Page"'s @migrateWorldEditDTOv2@).
        --
        -- Resolved for the WHOLE session up here rather than per page
        -- inside 'stagePage', on exactly the terms the map-image plans
        -- above are admitted: a load publishes a complete session or
        -- publishes nothing, so a page whose species cannot be resolved
        -- must fail the transaction before any page's live refs are
        -- written.
        --
        -- Reaching the refusal below means
        -- 'World.Save.Types.missingFloraReferences' did not run, or ran
        -- against a different catalog: load acceptance already refuses
        -- exactly these references, through this same resolution rule,
        -- before staging is ever queued.
        let resolvedFlora =
                [ (wpsPageId wps
                  , resolveFloraReferences catalog (wpsPageId wps)
                        (wpsEdits wps) (wpsCropPlots wps)
                        (wpsPlantDesignations wps))
                | wps ← orderedPages ]
            -- Both pre-stage refusals in ONE list, in the order they are
            -- checked, so either fails the transaction identically and
            -- neither can shadow the other into a partial stage.
            stageRefusals =
                [ "cannot stage page " <> unWorldPageId pid <> ": "
                  <> T.intercalate "; " (map renderMissingFloraRef errs)
                | (pid, Left errs) ← resolvedFlora ]
                ⧺
                [ "cannot stage page " <> unWorldPageId refusedPid <> ": "
                  <> mapImageRefusalText refusal
                | (refusedPid, refusal) ← planRefusals ]
        case stageRefusals of
         (msg : _) → pure $ Left $ StageError msg
         [] → do
          results ← forM (zip orderedPages
                              [ flora | (_, Right flora) ← resolvedFlora ]) $
            \(wps, flora) →
              stagePage logger registry palette catalog
                        buildingDefs unitDefs artCatalog refundDeps
                        mapCeiling activeWpsId flora wps

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
          -- #2020: a post-admission map-image failure (a chunk-block
          -- count or size that disagrees with the accepted plan) fails
          -- the whole transaction here, before anything is published.
          case [ e | Just e ← map psrStageError results ] of
           (stageErr : _) → pure $ Left stageErr
           [] →
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
                  -- No destruction effects survive a load (#2091): a
                  -- save taken during playback holds neither the
                  -- demolished building nor its presentation, and the
                  -- replacement session starts with none.
                  finalBuildings = BuildingManager
                      { bmDefs = buildingDefs, bmInstances = mergedBuildings
                      , bmNextId = nextBid, bmSelected = Nothing
                      , bmDestructions = HM.empty }
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
              -- Every staged refund drew from this ref, so its value now
              -- is the allocator the published session must carry.
              -- Identity when nothing self-cleared.
              stagedNextItemId ← readIORef stagedItemIdRef
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
                      , ssNextItemId    = stagedNextItemId
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
    → StructureArtCatalog → ConstructRefundDeps
    → MapImageCeiling → WorldPageId
    → (WorldEdits, CropPlots, PlantDesignations)
      -- ^ #2243: this page's edit log, crop plots and plant designations
      --   with every species reference already resolved to a runtime
      --   handle by 'stageSession'. Handed in rather than read off
      --   @wps@, because the saved values name their species and only
      --   the catalog can turn a name into a handle — resolving it here
      --   would put a per-page failure inside a function whose contract
      --   is that a page always stages.
    → WorldPageSave → IO PageStageResult
stagePage logger registry palette catalog buildingDefs unitDefs
          artCatalog refundDeps mapCeiling activeWpsId
          (liveEdits, liveCropPlots, livePlantDesignations) wps = do
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
    -- #2021: the saved generated-world id, when the save has one
    -- (@world-pages@ v9 and later). When it does NOT — every save this
    -- build can still read from before v9 — the freshly-minted id
    -- 'emptyWorldState' already put on this staged page stands, which is
    -- requirement 7's "assigned a FRESH id during transactional load
    -- staging, not derived from anything in the legacy save".
    --
    -- The source file is untouched either way (requirement 8): this
    -- writes only into the staged, not-yet-published 'WorldState', so
    -- loading the same legacy save twice legitimately produces two
    -- different ids (D-21), and a staging failure publishes neither the
    -- session nor the id, exactly as it publishes nothing else.
    forM_ (wpsGeneratedId wps) $ writeIORef (wsGeneratedIdRef worldState)
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
    -- Normalized into this page's canonical chunk identity (#2001). A
    -- save written by the old restore path holds entries keyed to a
    -- seam ALIAS — including every settled-fluid snapshot, which
    -- appendFluidSnapshot keys by the chunk's own lcCoord — and
    -- applyEdit refuses an edit whose coords do not belong to the chunk
    -- it is handed. Normalizing ONCE here is what keeps them replaying
    -- for the life of the session: the initial centre is only the first
    -- of many loads, and the streaming loader regenerates that chunk
    -- canonically every time it is evicted and comes back.
    --
    -- Identity for every save whose keys are already canonical, which is
    -- every page away from the seam.
    writeIORef (wsEditsRef worldState)
        (canonicalizeWorldEdits (canonicalChunkCoord params) liveEdits)
    writeIORef (wsMineDesignationsRef worldState) (wpsMineDesignations wps)
    writeIORef (wsConstructDesignationsRef worldState)
        (wpsConstructDesignations wps)
    writeIORef (wsConstructAttemptRef worldState)
        (wpsConstructNextAttempt wps)
    writeIORef (wsGroundItemsRef worldState) (wpsGroundItems wps)
    -- #1844: reconcile the saved structure designations against the
    -- CURRENTLY registered content before anything is visible. It runs
    -- here — after the designations and this page's own ground items are
    -- in place, and BEFORE any chunk is generated — for two reasons: a
    -- self-clear's refund must land in the staged page's ground items
    -- (a live-session verb would deposit it into the session being
    -- replaced), and a job cleared now never has its progress slope
    -- stamped by the 'applyConstructSlopes' pass below at all.
    --
    -- Terrain-dependent reconciliation is deliberately NOT this
    -- boundary's job: a load publishes with almost nothing resident, so
    -- it belongs to the chunk-publication hook.
    mConstructErr ← reconcileStagedConstructDesignations
                        refundDeps artCatalog logger worldState
    writeIORef (wsSpoilRef worldState) (wpsSpoilPiles wps)
    writeIORef (wsFloraHarvestsRef worldState) (wpsFloraHarvests wps)
    writeIORef (wsChopDesignationsRef worldState) (wpsChopDesignations wps)
    -- #1854: deferred legacy migration state restores beside the real
    -- maps, so a session that saves before every chunk has been visited
    -- cannot lose a designation or a regrowth timer it never got to
    -- resolve. "World.Flora.Designation" drains these as chunks arrive.
    writeIORef (wsPendingChopMigrationRef worldState)
        (wpsPendingChopMigration wps)
    writeIORef (wsPendingFloraHarvestsRef worldState)
        (wpsPendingFloraHarvests wps)
    writeIORef (wsPlantedFloraCursorRef worldState)
        (wpsPlantedFloraCursor wps)
    writeIORef (wsTillDesignationsRef worldState) (wpsTillDesignations wps)
    writeIORef (wsCropPlotsRef worldState) liveCropPlots
    writeIORef (wsPlantDesignationsRef worldState) livePlantDesignations
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

    (simSeeds, locStamps, mCamera, mZoomAtlas, mPreview, mStageErr) ←
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
          pure (seeds, [], mCam, Nothing, Nothing, Nothing)
        else do
          when isActive $ writeIORef phaseRef (LoadPhase1 2 totalSteps)
          (zoomCache, chunkPixels) ← if not isActive
            then pure $ buildZoomCacheWithPixels params registry palette Nothing
            else do
              keyResult ← buildZoomArtifactKey params registry
              case keyResult of
                Left reason → do
                  logWarn logger CatWorld $
                      "Zoom artifact cache: unavailable (" <> reason
                      <> "); rebuilding"
                  pure $ buildZoomCacheWithPixels params registry palette Nothing
                Right key → do
                  cached ← loadZoomArtifact key
                  case cached of
                    Right artifact → do
                      _ ← evaluate $ force
                          (zaEntries artifact, zaPixels artifact)
                      logInfo logger CatWorld $
                          "Zoom artifact cache: hit ("
                          <> tshow (zaBytes artifact) <> " bytes)"
                      pure (zaEntries artifact, zaPixels artifact)
                    Left reason → do
                      logInfo logger CatWorld $
                          "Zoom artifact cache: miss (" <> reason
                          <> "); rebuilding"
                      let built@(builtEntries, builtPixels) =
                              buildZoomCacheWithPixels params registry palette Nothing
                      _ ← evaluate $ force (builtEntries, builtPixels)
                      published ← publishZoomArtifact key builtEntries builtPixels
                      case published of
                        Left publishReason →
                          logWarn logger CatWorld $
                              "Zoom artifact cache: publish skipped ("
                              <> publishReason <> ")"
                        Right bytes →
                          logInfo logger CatWorld $
                              "Zoom artifact cache: published "
                              <> tshow bytes <> " bytes"
                      pure built
          _ ← evaluate (force zoomCache)
          writeIORef (wsZoomCacheRef worldState) zoomCache
          writeIORef (wsZoomAtlasRef worldState) Nothing
          (mZoomAtlasVal, mPreviewVal, mAtlasErr) ← if isActive
            then do
              _ ← evaluate (force chunkPixels)
              -- #2020: the SAME pure admission 'stageSession' already ran
              -- for this page, re-derived from the same worldSize through
              -- the same function — it cannot disagree, and it is what
              -- makes the plan (not this module) the allocation
              -- authority. 'buildZoomAtlas' then verifies the cache
              -- count, the block count and every block's size against it
              -- before allocating or copying.
              let eAtlas = do
                      plan ← admitMapImage mapCeiling MapImageRGBA8
                                 (ZoomAtlasSource worldSize)
                      buildZoomAtlas plan (V.length zoomCache) chunkPixels
              case eAtlas of
                Left refusal → do
                  let msg = "cannot stage page " <> unWorldPageId pid
                            <> ": " <> mapImageRefusalText refusal
                  logError logger CatWorld ("Save load: " <> msg)
                  pure (Nothing, Nothing, Just (StageError msg))
                Right atlas → do
                  _ ← evaluate (force atlas)
                  let preview = buildPreviewFromPixels params zoomCache chunkPixels
                  _ ← evaluate (force preview)
                  pure ( Just (zadWidth atlas, zadHeight atlas, zadPixelData atlas)
                       , Just (piWidth preview, piHeight preview, piData preview)
                       , Nothing )
            else pure (Nothing, Nothing, Nothing)

          when isActive $ writeIORef phaseRef (LoadPhase1 3 totalSteps)
          -- The saved camera chunk, canonicalised. 'cameraChunkCoord'
          -- does no wrapping, so a session saved past the seam names a
          -- non-canonical ALIAS — and this centre is generated and
          -- inserted under whatever coord it is given, where every
          -- canonicalising reader would miss it.
          --
          -- Its saved replay entries reach it because the edit log was
          -- normalized into this same frame above, once, rather than
          -- replayed in the saved frame here: this centre is only the
          -- first of many loads of that chunk, and the streaming loader
          -- regenerates it canonically every time it is evicted and
          -- comes back.
          let centerCoord = canonicalChunkCoord params $
                  cameraChunkCoord (wpsCameraFacing wps)
                                   (wpsCameraX wps)
                                   (wpsCameraY wps)
          -- Claimed before generation, exactly as fresh world init does.
          centreClaims ← claimChunkGeneration worldState pid params
                                              [centerCoord]
          let (ct, cs, cterrain, cf, cice, cflora, cwt, cmagma) =
                  generateChunk registry catalog pid params centerCoord
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
          -- #1854 requirement 15: the restored centre is the FIRST
          -- resident chunk of the loaded session, so its pending legacy
          -- migration is drained and its designation mirrors hydrated
          -- before anything can read it — the same admission every
          -- streamed chunk takes.
          centerChunk ← admitChunkFlora worldState catalog logger $
              applyConstructSlopes cdesigs
                  (applyDigSlopes desigs (replayEdits edits centerChunkRaw))
          -- The restored centre is new residency (#2001), claimed and
          -- admitted exactly as a fresh world's centre is — under the
          -- CANONICAL key, which is where every reader looks for it.
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
          -- Register the restored box and append what still needs
          -- scheduling (#2001), through the same call fresh world init
          -- uses. This staged page is not published until
          -- World.Load.Publish, so nothing can race the queue here —
          -- sharing the one helper is what keeps the two seed paths from
          -- drifting, not a defence this path needs.
          -- Installs LoadPhase2 itself, as fresh world init does.
          _ ← seedInitialQueue pid worldState params remainingCoords

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
          pure (seeds, stamps, mCam, mZoomAtlasVal, mPreviewVal, mAtlasErr)

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
    -- #1844: the same boundary, for structure designations. The
    -- catalogue half already ran above, BEFORE any chunk existed; this
    -- is the terrain half, and it is the only pass that sees the chunks
    -- this page reconstructed synchronously — an arena rebuilds every
    -- chunk here and has an EMPTY init queue, and an ordinary page's
    -- centre chunk is excluded from the queue too, so neither would ever
    -- reach 'World.Thread.ChunkLoading''s publication sweep. Without
    -- this, a loaded designation whose surface has drifted, whose slot
    -- is now filled, or whose supporting floor is gone could survive
    -- indefinitely on exactly those chunks. Everything still unloaded
    -- resolves as unresolved-terrain and is retained, which is what the
    -- queue's own sweep then settles.
    _ ← revalidateStagedConstructDesignations refundDeps artCatalog logger
            worldState ConstructWholePage

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
          -- Either failure aborts the whole transaction; the construct
          -- reconciliation's is reported first because it names a
          -- concrete designation and a concrete missing definition,
          -- which is the more actionable of the two.
        , psrStageError      = case mConstructErr of
            Left (ConstructReconcileError t) → Just (StageError t)
            Right () → mStageErr
        }
