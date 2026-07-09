{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | The load path: reconstruct every saved world page from 'SaveData',
--   merge restored entities into the global managers, and restore
--   visibility. Split out of "World.Thread.Command.Save" (issue #561);
--   the per-page restoration itself lives in
--   "World.Thread.Command.Save.LoadPage" and restore-id assignment in
--   "World.Thread.Command.Save.RestoreIds".
module World.Thread.Command.Save.LoadWorld
    ( handleWorldLoadSaveCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
import Engine.Core.Log (logInfo, logError, logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Thread.Command.UI (handleWorldShowCommand, handleWorldHideCommand)
import World.ZoomMap.ColorPalette (buildColorPalette)
import Building.Types (BuildingManager(..), unBuildingId, biPage)
import Unit.Types (UnitManager(..), unUnitId, uiPage)
import Unit.Sim.Types (UnitThreadState(..))
import World.Thread.Helpers (sendGenLog, sendSaveLoaded, unWorldPageId)
import World.Thread.Command.Save.RestoreIds (dedupPages, assignRestoreIds)
import World.Thread.Command.Save.LoadPage (restoreSavedPage)

-- | Load: reconstruct WorldState from SaveData ──
--   This is essentially WorldInit but skipping the expensive
--   buildTimeline / computeOceanMap / initEarlyClimate steps
--   because those are already baked into sdGenParams.
handleWorldLoadSaveCommand ∷ EngineEnv → LoggerState → WorldPageId → SaveData → IO ()
handleWorldLoadSaveCommand env logger pageId saveData
  | (firstWps : _) ← sdWorlds saveData = do
    logInfo logger CatWorld $ "Loading save into world: "
        <> unWorldPageId pageId

    -- Live page ids in the CURRENT session, snapshotted before any registration
    -- below. 'priorLoad' is the set of pages a prior load of THIS SAME save
    -- (keyed by save name) registered. A synthetic collision-rename id must
    -- avoid UNRELATED live pages (so the load doesn't clobber them, #191) — but
    -- NOT this save's own prior pages: re-loading the same save reuses (and thus
    -- replaces) its previous synthetic ids instead of accumulating new ones. So
    -- synthetics avoid liveIds minus priorLoad. Scoping by save name keeps a
    -- DIFFERENT save's synthetic pages unrelated (preserved), not load-owned.
    let saveName = smName (sdMetadata saveData)
    liveIds   ← HS.fromList . map fst . wmWorlds <$> readIORef (worldManagerRef env)
    priorLoad ← HM.lookupDefault HS.empty saveName
                    <$> readIORef (loadProvenanceRef env)
    let effectiveLiveIds = liveIds `HS.difference` priorLoad

    -- #217/#218: restore EVERY saved page in sdWorlds, not just the active
    -- one. The active page (the one whose id matches 'sdActivePage') restores
    -- under the load-target id 'pageId' (always "main_world" — the documented
    -- convention Lua/headless code assumes); additional pages restore under
    -- their own saved ids. The pattern guard binds 'firstWps' as the fallback
    -- for a save whose recorded active id names no page.
    let activeWps   = fromMaybe firstWps (activeWorldPage saveData)
        activeWpsId = wpsPageId activeWps
        -- Restore-target id for each saved page, guaranteed collision-free:
        -- the active page → 'pageId' (main_world); every other page keeps its
        -- own id unless it would collide (a non-active page also named
        -- "main_world"), then a "#N" suffix. See 'assignRestoreIds'.
        restoreIds  = assignRestoreIds pageId activeWpsId effectiveLiveIds
                                       (sdWorlds saveData)
        restoreId w = HM.lookupDefault (wpsPageId w) (wpsPageId w) restoreIds
        -- The page ids the SAVE owns. A saved page that gets remapped (the
        -- active page → main_world, or a collision rename) leaves its ORIGINAL
        -- id behind: a live pre-load page under that id is now stale and must
        -- be torn down, and its entities must not survive as "off-page". Track
        -- both the saved originals and their restore targets.
        savedOriginalIds = HS.fromList (map wpsPageId (sdWorlds saveData))
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
    --    chunk + chunk queue (the historical single-page body, now
    --    'World.Thread.Command.Save.LoadPage.restoreSavedPage', called once
    --    per saved page). Each call returns this page's restored
    --    building/unit slices (stamped with the page's restore id) for the
    --    manager merge in step 2.
    perPage ← forM orderedPages $
        restoreSavedPage env logger registry palette catalog
                          currentBm currentUm restoreId activeWpsId

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
        -- The id-space the load OWNS: the restored pages, the saved originals
        -- (so a pre-load page under a remapped-away id — e.g. the saved active
        -- "other" now living as main_world — is not preserved), and the PRIOR
        -- load's pages (so re-loading replaces them instead of stranding their
        -- entities as off-page survivors). Genuinely-unrelated live pages are
        -- in none of these and are kept (#191).
        consumedIds  = restoredPageIds `HS.union` savedOriginalIds
                                       `HS.union` priorLoad
        -- Off-page = entities whose page is NOT owned by the load — genuinely
        -- unrelated live pages, which we keep (#191).
        offPageB     = HM.filter (\bi → not (HS.member (biPage bi) consumedIds))
                                 (bmInstances currentBm)
        offPageU     = HM.filter (\ui → not (HS.member (uiPage ui) consumedIds))
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

    -- 2b. Tear down STALE pages the load supersedes but didn't re-register:
    --     a saved page remapped away (active → main_world, or a collision
    --     rename) frees its original id, and any prior-load page this re-load
    --     didn't reuse (e.g. an old synthetic main_world#2 superseded by a
    --     freshly-reused id). Drop each from wmWorlds/wmVisible and discard its
    --     sim, so a within-session load leaves no ghost world behind. (Their
    --     entities were already excluded via 'consumedIds'.) Done before
    --     visibility restore so the hide/show below never touches a stale page.
    let staleIds = (savedOriginalIds `HS.union` priorLoad)
                       `HS.difference` restoredPageIds
    forM_ (HS.toList staleIds) $ \gid →
        Q.writeQueue (simQueue env) (SimDropWorld gid)
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        ( mgr { wmWorlds  = filter (\(p,_) → not (HS.member p staleIds))
                                   (wmWorlds mgr)
              , wmVisible = filter (\p → not (HS.member p staleIds))
                                   (wmVisible mgr) }, () )

    -- 3. Restore visibility (#217) through the proper show/hide handlers so
    --    their side effects fire — SimActivateWorld for shown pages,
    --    SimDeactivateWorld + cursor cleanup for hidden ones. A raw wmVisible
    --    write would skip those, leaving restored-visible pages un-simulated
    --    and (on a within-session load) now-hidden pages still simulating.
    --    Each visible page's saved id is remapped through 'restoreIds' (active
    --    → main_world, plus any collision rename). main_world is forced to the
    --    head so 'resolveActiveWorld' lands on it; handleWorldShowCommand
    --    prepends, so we show in reverse to leave main_world first.
    mgrNow ← readIORef (worldManagerRef env)
    let remapVis p   = HM.lookupDefault p p restoreIds
        wantVisible  = filter (\p → isJust (lookup p (wmWorlds mgrNow)))
                         (dedupPages
                            (pageId : map remapVis (sdVisiblePages saveData)))
    -- Hide whatever was visible before (clears stale within-session pages and
    -- gives showing a clean slate for deterministic head ordering).
    forM_ (wmVisible mgrNow) $ \p → handleWorldHideCommand env logger p
    forM_ (reverse wantVisible) $ \p → handleWorldShowCommand env logger p

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

    -- Record the pages THIS load registered under this save's name, so the next
    -- within-session load of the SAME save reuses (replaces) them rather than
    -- accumulating fresh synthetic ids — while a different save's pages stay
    -- preserved as unrelated (#214, #191).
    atomicModifyIORef' (loadProvenanceRef env) $ \m →
        (HM.insert saveName restoredPageIds m, ())

  | otherwise =
      -- Defense-in-depth: loadWorld already rejects an empty sdWorlds at
      -- decode time (Serialize.hs), so engine.loadSave fails cleanly before
      -- queueing this command and this branch is unreachable via the normal
      -- path. Kept so a future direct caller can't crash the world thread.
      logError logger CatWorld
          "Cannot load save: save contains no world pages"
