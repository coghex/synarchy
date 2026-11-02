{-# LANGUAGE Strict #-}
-- | The flora condition mutation seam (#2555, EFM-7): the world thread's
--   handlers for 'World.Command.Types.WorldSetFloraCondition' and
--   'World.Command.Types.WorldClearFloraCondition'.
--
--   These are the ONLY writers of a persistent flora condition — of the
--   identity-keyed 'wsFloraConditionsRef' record and of a groundcover
--   plot's embedded record. Lua only enqueues
--   ('Engine.Scripting.Lua.API.Forage.Condition'); the world thread owns
--   the state. What a set records is decided by the pure half
--   ("World.Flora.ConditionSeam"); this module resolves the occurrence,
--   runs the transition atomically against the live store, invalidates
--   the quad cache, and records an action outcome — accepted, no-op, or
--   a rejection that names its reason. A refusal changes NOTHING and
--   leaves the cache alone.
--
--   There is deliberately no hazard producer here. EFM-10 owns the same
--   world-thread boundary's page-day natural-death and expiry reconciliation.
module World.Thread.Command.Flora.Condition
    ( handleWorldSetFloraConditionCommand
    , handleWorldClearFloraConditionCommand
    , reconcileFloraDays
    , reconcileAdmittedFlora
    ) where

import UPrelude
import Gameplay.Tags.Page (CropPlotStore(..))
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Engine.Core.State (EngineEnv)
import World.Types
import World.Flora.Clock (growthCalendar)
import World.Flora.Condition
    ( ConditionRetention(..), FloraConditionRecordOf(..)
    , RetentionSuccessor(..), conditionAtClock, lookupFloraCondition )
import World.Flora.ConditionSeam
import World.Flora.CropPlot (CropPlotOf(..))
import World.Flora.CropPlot
    ( cropPlotElapsedDays, cropPlotInstance )
import World.Flora.Designation (forgetFloraInstances, replaceChunkForgettingFlora)
import World.Flora.Growth
    ( FloraGrowth(..), deadWindowDays, floraGrowth, floraTotalAgeAt
    , growthDeathDay, growthRate, instanceLifespan )
import World.Edit.Apply (applyEdit)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Generate.Coordinates (canonicalTile, globalToChunk)
import Gameplay.Tags.Page (removePlot)
import World.Generate.Coordinates (canonicalTileFrame)

-- | The kinds the action-outcome ring reports these under.
kindSet, kindClear ∷ Text
kindSet   = "flora.condition.set"
kindClear = "flora.condition.clear"

-- | One outcome record: @outcome@ is @accepted@, @noop@ or @rejected@.
recordOutcome ∷ EngineEnv → Text → Text → Int → Int → Maybe Text → IO ()
recordOutcome env kind outcome gx gy reason = do
    gt ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    pushActionOutcome (ucActionOutcomeRef (toUnitCombatCapability env))
        ActionOutcome
            { aoTs        = gt
            , aoKind      = kind
            , aoOutcome   = outcome
            , aoWhereX    = Just (fromIntegral gx)
            , aoWhereY    = Just (fromIntegral gy)
            , aoTarget    = Nothing
            , aoRequested = Nothing
            , aoApplied   = Nothing
            , aoDropped   = Nothing
            , aoReason    = reason
            , aoHandler   = Nothing
            }

recordRefusal ∷ EngineEnv → Text → Int → Int → SeamRefusal → IO ()
recordRefusal env kind gx gy r =
    recordOutcome env kind "rejected" gx gy (Just (renderSeamRefusal r))

-- | The page, or a "world page not found" rejection (the page can be
--   destroyed between enqueue and drain).
withPage ∷ EngineEnv → Text → WorldPageId → Int → Int
         → (WorldState → IO ()) → IO ()
withPage env kind pageId gx gy act = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Nothing → recordOutcome env kind "rejected" gx gy
            (Just ("world page not found: " <> unWorldPageId pageId))
        Just ws → act ws

-- | The resident occurrence @iid@ standing on the (alias) tile, with the
--   canonical tile it resolved to.
residentOccurrence ∷ WorldState → Int → Int → FloraInstanceId
                   → IO ((Int, Int), Maybe FloraInstance)
residentOccurrence ws rawGX rawGY iid = do
    worldSize ← pageWrapWorldSize ws
    let (coord, (lx, ly), (dgx, dgy)) = canonicalTileFrame worldSize rawGX rawGY
        tile = (rawGX + dgx, rawGY + dgy)
    td ← readIORef (wsTilesRef ws)
    pure ( tile
         , do lc ← lookupChunk coord td
              listToMaybe
                  [ i | i ← fcdInstances (lcFlora lc)
                      , fiInstanceId i ≡ iid
                      , fromIntegral (fiTileX i) ≡ lx
                      , fromIntegral (fiTileY i) ≡ ly ] )

handleWorldSetFloraConditionCommand
    ∷ EngineEnv → LoggerState → WorldPageId → Int → Int
    → Maybe FloraInstanceId → Text → IO ()
handleWorldSetFloraConditionCommand env logger pageId gx gy mIid causeTok =
    withPage env kindSet pageId gx gy $ \ws → case parseDeathCause causeTok of
        Nothing → recordRefusal env kindSet gx gy
                      (RefusedUnknownCause causeTok)
        Just cause → do
            cat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
            (daysPerYear, _, absDay) ← growthCalendar ws
            result ← case mIid of
                Just iid → do
                    (tile, mInst) ← residentOccurrence ws gx gy iid
                    case mInst of
                        Nothing → pure (Left RefusedUnknownOccurrence)
                        Just inst → do
                            conditions ← readIORef (wsFloraConditionsRef ws)
                            worldSize ← pageWrapWorldSize ws
                            anchored ← case HM.lookup iid conditions of
                                Just prior | fcrCondition prior ≡ ConditionReseeded →
                                    case lookupSpecies (fiSpecies inst) cat of
                                        Nothing → pure inst
                                        Just species →
                                            let growth = floraGrowth
                                                    (Just (conditionAtClock 0 prior))
                                                    species absDay inst
                                                current = anchorReseededState
                                                    species absDay growth inst
                                                (rawCoord, _) = globalToChunk
                                                    (fst tile) (snd tile)
                                                coord = wrapChunkCoordU worldSize rawCoord
                                            in persistFloraInstanceState ws worldSize
                                                coord inst current >> pure current
                                _ → pure inst
                            atomicModifyIORef' (wsFloraConditionsRef ws) $ \store →
                                case setOccurrenceCondition cat daysPerYear
                                         tile absDay anchored cause store of
                                    Left e → (store, Left e)
                                    Right store' → (store', Right ())
                Nothing → do
                    worldSize ← pageWrapWorldSize ws
                    let (_, _, (dgx, dgy)) = canonicalTileFrame worldSize gx gy
                        tile = (gx + dgx, gy + dgy)
                    -- The plot is read out of the SAME map the edit is
                    -- applied to, inside the atomic step.
                    atomicModifyIORef' (wsCropPlotsRef ws) $ \store →
                        case setPlotCondition cat daysPerYear tile absDay
                                 cause (cpsPlots store) of
                            Left e → (store, Left e)
                            Right ps → (store { cpsPlots = ps }, Right ())
            case result of
                Left e → recordRefusal env kindSet gx gy e
                Right () → do
                    bumpQuadCacheGen ws
                    logDebug logger CatWorld $
                        "Flora condition set at " <> tshow gx <> ","
                          <> tshow gy <> " cause=" <> causeTok
                    recordOutcome env kindSet "accepted" gx gy Nothing

handleWorldClearFloraConditionCommand
    ∷ EngineEnv → LoggerState → WorldPageId → Int → Int
    → Maybe FloraInstanceId → IO ()
handleWorldClearFloraConditionCommand env logger pageId gx gy mIid =
    withPage env kindClear pageId gx gy $ \ws → do
        cat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
        (_, _, absDay) ← growthCalendar ws
        result ← case mIid of
            Just iid → do
                (tile, mInst) ← residentOccurrence ws gx gy iid
                -- An occurrence is known if its chunk is resident OR the
                -- store holds a record whose provenance tile is this one
                -- (an evicted corpse must stay clearable).
                atomicModifyIORef' (wsFloraConditionsRef ws) $ \store →
                    let recorded = lookupFloraCondition iid store
                        known = isJust mInst
                              ∨ maybe False ((≡ tile) . fcrTile) recorded
                        underlyingNaturalDeath = case (recorded, mInst) of
                            (Just _, Just inst) → case lookupSpecies
                                    (fiSpecies inst) cat of
                                Just species → fgDead (floraGrowth Nothing
                                    species absDay inst)
                                Nothing → False
                            _ → False
                        protected = maybe False durableNaturalOrSuccessor recorded
                                  ∨ underlyingNaturalDeath
                    in if not known
                       then (store, Left RefusedUnknownOccurrence)
                       else if protected
                       then (store, Left RefusedAlreadyConditioned)
                       else ( clearOccurrenceCondition iid store
                            , Right (isJust recorded) )
            Nothing → do
                worldSize ← pageWrapWorldSize ws
                let (_, _, (dgx, dgy)) = canonicalTileFrame worldSize gx gy
                    tile = (gx + dgx, gy + dgy)
                atomicModifyIORef' (wsCropPlotsRef ws) $ \store →
                    let plots = cpsPlots store
                    in case HM.lookup tile plots of
                        Just plot | maybe False durableNaturalOrSuccessor
                                (cpCondition plot) →
                            (store, Left RefusedAlreadyConditioned)
                        Just plot | maybe False (\_ →
                                case lookupSpecies (cpSpecies plot) cat of
                                    Just species → fgDead (floraGrowth Nothing
                                        species (cropPlotElapsedDays absDay plot)
                                        (cropPlotInstance plot))
                                    Nothing → False) (cpCondition plot) →
                            (store, Left RefusedAlreadyConditioned)
                        _ → case clearPlotCondition tile plots of
                            Left e → (store, Left e)
                            Right ps →
                                ( store { cpsPlots = ps }
                                , Right (maybe False (isJust . cpCondition)
                                             (HM.lookup tile plots)) )
        case result of
            Left e → recordRefusal env kindClear gx gy e
            Right changed → do
                when changed (bumpQuadCacheGen ws)
                logDebug logger CatWorld $
                    "Flora condition clear at " <> tshow gx <> ","
                      <> tshow gy <> " changed=" <> tshow changed
                if changed
                    then recordOutcome env kindClear "accepted" gx gy Nothing
                    else recordOutcome env kindClear "noop" gx gy
                             (Just "the occurrence had no explicit condition")

durableNaturalOrSuccessor ∷ FloraConditionRecordOf s → Bool
durableNaturalOrSuccessor record =
    fcrCause record ≡ CauseNatural
      ∨ fcrCondition record `elem` [ConditionReseeded, ConditionAbsent]

-- | Reconcile one page's flora to its new absolute calendar day.
--   Each occurrence computes its next death/expiry boundary directly, so
--   long date jumps do work proportional to current flora and records,
--   not to the number of intervening days. The caller owns the page clock
--   write; another page's date is never borrowed.
reconcileFloraDays ∷ WorldSimCapability → WorldPageId → WorldState
                   → Int → Int → IO Bool
reconcileFloraDays _ _ _ fromDay toDay | toDay ≤ fromDay = pure False
reconcileFloraDays wsc _ ws _ toDay = do
    paramsM ← readIORef (wsGenParamsRef ws)
    let daysPerYear = maybe (calendarDaysPerYear defaultCalendarConfig)
            (calendarDaysPerYear ∘ wgpCalender) paramsM
    storedChanged ← reconcileStoredOutcomes ws toDay
    plotChanged ← reconcilePlots wsc ws daysPerYear toDay
    occurrenceChanged ← reconcileLoadedOccurrences wsc ws daysPerYear toDay Nothing
    let changed = storedChanged ∨ plotChanged ∨ occurrenceChanged
    when changed (bumpQuadCacheGen ws)
    pure changed

-- | Reconcile flora in newly admitted chunks against their owning page's
--   current calendar before any consumer sees them.
reconcileAdmittedFlora ∷ WorldSimCapability → WorldPageId → WorldState
                       → [ChunkCoord] → IO Bool
reconcileAdmittedFlora wsc _ ws coords = do
    (daysPerYear, _, day) ← growthCalendar ws
    td ← readIORef (wsTilesRef ws)
    let chunks = foldr (\coord acc → maybe acc (: acc) (lookupChunk coord td))
            [] coords
    changed ← reconcileLoadedOccurrences wsc ws daysPerYear day (Just chunks)
    when changed (bumpQuadCacheGen ws)
    pure changed

-- Stored occurrence outcomes survive chunk eviction. In particular, a
-- row's removal edit is written even if no chunk currently contains it.
reconcileStoredOutcomes ∷ WorldState → Int → IO Bool
reconcileStoredOutcomes ws day = do
    worldSize ← pageWrapWorldSize ws
    records ← readIORef (wsFloraConditionsRef ws)
    or <$> forM (HM.toList records) (\(iid, record) → case
        (fcrCondition record, fcrRetention record) of
            (ConditionDead, RetentionTransient expiry successor)
                | expiry ≤ day → case successor of
                    RetainReseed → updateMarker iid record ConditionReseeded
                    RetainAbsent → updateMarker iid record ConditionAbsent
                    RetainAwaitReplanting → do
                        removeRowOccurrence ws worldSize iid (fcrTile record)
                        atomicModifyIORef' (wsFloraConditionsRef ws) $ \store →
                            (HM.delete iid store, ())
                        pure True
            _ → pure False)
  where
    updateMarker iid record marker = do
        changed ← atomicModifyIORef' (wsFloraConditionsRef ws) $ \store →
            let current = HM.lookup iid store
            in case current of
                Just r | r ≡ record →
                    (HM.insert iid (record { fcrCondition = marker }) store, True)
                _ → (store, False)
        when (changed ∧ marker ≡ ConditionAbsent) $
            forgetFloraInstances ws [iid]
        pure changed

removeRowOccurrence ∷ WorldState → Int → FloraInstanceId → (Int, Int) → IO ()
removeRowOccurrence ws worldSize iid rawTile = do
    let (gx, gy) = canonicalTile worldSize (fst rawTile) (snd rawTile)
        (rawCoord, _) = globalToChunk gx gy
        coord = wrapChunkCoordU worldSize rawCoord
        edit = WeRemoveFlora gx gy iid
    atomicModifyIORef' (wsEditsRef ws) $ \edits →
        let exists = any (elem edit) (HM.elems edits)
        in (if exists then edits else appendEdit coord edit edits, ())
    tiles ← readIORef (wsTilesRef ws)
    case lookupChunk coord tiles of
        Nothing → pure ()
        Just old → replaceChunkForgettingFlora ws old (applyEdit edit old)
    forgetFloraInstances ws [iid]

-- | Materialize a reseed anchor into the planted instance's age baseline.
--   This makes removing a temporary explicit condition reveal the same
--   generation, age and next death boundary that the successor marker had.
anchorReseededState ∷ FloraSpecies → Int → FloraGrowth → FloraInstance
                    → FloraInstance
anchorReseededState species day growth fi = case instanceLifespan species fi of
    Nothing → fi
    Just lifespan →
        let cycleLen = lifespan + deadWindowDays
            age = if fgDead growth then lifespan else fgAge growth
            baseline = fromIntegral (fgGeneration growth) * cycleLen + age
                - fromIntegral day * growthRate (fiHealth fi)
        in fi { fiAge = baseline }

-- | Persist a changed placement-age baseline both in the page's edit log
--   and the resident chunk, keeping eviction/replay on the same anchor.
persistFloraInstanceState ∷ WorldState → Int → ChunkCoord → FloraInstance
                          → FloraInstance → IO ()
persistFloraInstanceState ws worldSize coord old current
    | fiAge old ≡ fiAge current = pure ()
    | otherwise = do
        let ChunkCoord cx cy = coord
            (gx, gy) = canonicalTile worldSize
                (cx * chunkSize + fromIntegral (fiTileX old))
                (cy * chunkSize + fromIntegral (fiTileY old))
            edit = WeSetFloraState gx gy (fiInstanceId old) (fiZ current)
                (fiOffU current) (fiOffV current) (fiVariant current)
                (fiHealth current) (fiAge current)
        atomicModifyIORef' (wsEditsRef ws) $ \edits →
            let compacted = HM.map (filter (not . sameState)) edits
                trimmed = HM.filter (not . null) compacted
                sameState (WeSetFloraState _ _ iid _ _ _ _ _ _) =
                    iid ≡ fiInstanceId old
                sameState _ = False
            in (appendEdit coord edit trimmed, ())
        atomicModifyIORef' (wsTilesRef ws) $ \tiles →
            case lookupChunk coord tiles of
                Nothing → (tiles, ())
                Just lc →
                    let set inst
                            | fiInstanceId inst ≡ fiInstanceId old = current
                            | otherwise = inst
                        flora' = (lcFlora lc)
                            { fcdInstances = map set (fcdInstances (lcFlora lc)) }
                        lc' = lc { lcFlora = flora' }
                    in (insertChunk lc' tiles, ())

-- | The first whole calendar day whose predicate is true. Growth uses
--   Float arithmetic, so binary search checks the actual production
--   expression after estimating no fixed day count.
firstCalendarDay ∷ (Int → Bool) → Int
firstCalendarDay reached
    | reached 0 = 0
    | otherwise = search 0 (until reached (* 2) 1)
  where
    search low high
        | low ≥ high = low
        | reached middle = search low middle
        | otherwise = search (middle + 1) high
      where middle = low + (high - low) `div` 2

reconcileLoadedOccurrences
    ∷ WorldSimCapability → WorldState → Int → Int → Maybe [LoadedChunk]
    → IO Bool
reconcileLoadedOccurrences wsc ws daysPerYear day mChunks = do
    catalog ← readIORef (wsFloraCatalogRef wsc)
    worldSize ← pageWrapWorldSize ws
    chunks ← case mChunks of
        Just selected → pure selected
        Nothing → HM.elems . wtdChunks <$> readIORef (wsTilesRef ws)
    or <$> forM
        [ (lcCoord lc, fi)
        | lc ← chunks
        , fi ← fcdInstances (lcFlora lc) ]
        (\(coord, fi) → reconcileOccurrence catalog worldSize coord fi)
  where
    reconcileOccurrence catalog worldSize coord fi = do
        let iid = fiInstanceId fi
        conditions ← readIORef (wsFloraConditionsRef ws)
        let mRecord = HM.lookup iid conditions
        case mRecord of
            Just record | fcrCondition record ≡ ConditionAbsent → pure False
            Just record | fcrCondition record ≡ ConditionDead →
                applyStoredDeath catalog worldSize (Just (coord, fi))
                    iid record False
            Just record | fcrCondition record ≡ ConditionReseeded →
                case lookupSpecies (fiSpecies fi) catalog of
                    Nothing → pure False
                    Just species → advanceReseeded catalog species worldSize
                        coord fi record
            _ → case lookupSpecies (fiSpecies fi) catalog of
                Nothing → pure False
                Just species → case initialDeathDay species fi of
                    Nothing → pure False
                    Just deathDay | deathDay > day → pure False
                    Just deathDay → do
                        let tile = maybe (tileOf worldSize coord fi) fcrTile mRecord
                            record = buildDeathRecord species daysPerYear tile
                                deathDay deathDay fi CauseNatural
                        installNaturalDeath catalog worldSize coord fi
                            mRecord record
    tileOf worldSize coord fi =
        let ChunkCoord cx cy = coord
        in canonicalTile worldSize
            (cx * chunkSize + fromIntegral (fiTileX fi))
            (cy * chunkSize + fromIntegral (fiTileY fi))

    initialDeathDay species fi = case instanceLifespan species fi of
        Nothing → Nothing
        Just lifespan →
            let atEpoch = floraGrowth Nothing species 0 fi
                threshold = fromIntegral (fgGeneration atEpoch)
                    * (lifespan + deadWindowDays) + lifespan
            in if fgDead atEpoch
                then growthDeathDay species fi atEpoch
                else Just (firstCalendarDay (\d → floraTotalAgeAt
                    (fiAge fi) (fiHealth fi) d ≥ threshold))

    reseededDeathDay species record fi = case fcrRetention record of
        RetentionTransient expiry RetainReseed → do
            lifespan ← instanceLifespan species fi
            let elapsed = firstCalendarDay (\d → floraTotalAgeAt 0
                    (fiHealth fi) d ≥ lifespan)
            pure (expiry + elapsed)
        _ → Nothing

    installNaturalDeath catalog worldSize coord fi expected record = do
        inserted ← atomicModifyIORef' (wsFloraConditionsRef ws) $ \store →
            if HM.lookup (fiInstanceId fi) store ≡ expected
                then (HM.insert (fiInstanceId fi) record store, True)
                else (store, False)
        if inserted
            then applyStoredDeath catalog worldSize (Just (coord, fi))
                (fiInstanceId fi) record True
            else pure False

    applyStoredDeath catalog worldSize mOccurrence iid record alreadyChanged =
        case fcrRetention record of
            RetentionPersistent → pure alreadyChanged
            RetentionTransient expiry successor
                | expiry > day → pure alreadyChanged
                | otherwise → case successor of
                    RetainReseed → do
                        changed ← updateMarker iid record ConditionReseeded
                        if changed
                            then do
                                conditions ← readIORef (wsFloraConditionsRef ws)
                                case HM.lookup iid conditions of
                                    Just marker → case lookupSpecies
                                            (fcrSpecies marker) catalog of
                                        Just species → do
                                            case mOccurrence of
                                                Just (coord, inst) → do
                                                    rest ← advanceReseeded catalog
                                                        species worldSize coord inst marker
                                                    pure (True ∨ rest)
                                                Nothing → pure True
                                        Nothing → pure True
                                    Nothing → pure True
                            else pure alreadyChanged
                    RetainAbsent → do
                        changed ← updateMarker iid record ConditionAbsent
                        when changed (forgetFloraInstances ws [iid])
                        pure (alreadyChanged ∨ changed)
                    RetainAwaitReplanting → do
                        removeRowOccurrence ws worldSize iid (fcrTile record)
                        atomicModifyIORef' (wsFloraConditionsRef ws) $ \store →
                            (HM.delete iid store, ())
                        pure True

    updateMarker iid record marker =
        atomicModifyIORef' (wsFloraConditionsRef ws) $ \store →
            case HM.lookup iid store of
                Just current | current ≡ record →
                    (HM.insert iid (current { fcrCondition = marker }) store, True)
                _ → (store, False)

    advanceReseeded catalog species worldSize coord fi previous =
        case reseededDeathDay species previous fi of
            Nothing → pure False
            Just deathDay | deathDay > day → pure False
            Just deathDay → do
                let growth = floraGrowth
                        (Just (conditionAtClock 0 previous)) species deathDay fi
                    current = anchorReseededDeath species deathDay growth fi
                    record = buildDeathRecord species daysPerYear
                        (fcrTile previous) deathDay deathDay current CauseNatural
                persistAnchor worldSize coord fi current
                installNaturalDeath catalog worldSize coord current
                    (Just previous) record

    anchorReseededDeath species deathDay growth fi =
        case instanceLifespan species fi of
            Nothing → fi
            Just lifespan →
                let cycleLen = lifespan + deadWindowDays
                    baseline = fromIntegral (fgGeneration growth) * cycleLen
                        + lifespan - fromIntegral deathDay * growthRate (fiHealth fi)
                in fi { fiAge = baseline }

    persistAnchor worldSize coord old current
        = persistFloraInstanceState ws worldSize coord old current

reconcilePlots ∷ WorldSimCapability → WorldState → Int → Int → IO Bool
reconcilePlots wsc ws daysPerYear day = do
    catalog ← readIORef (wsFloraCatalogRef wsc)
    store ← readIORef (wsCropPlotsRef ws)
    or <$> forM (HM.toList (cpsPlots store)) (\(tile, plot) → do
        let mRecord = cpCondition plot
            inst = cropPlotInstance plot
        case mRecord of
            Just record | fcrCondition record ≡ ConditionDead →
                case fcrRetention record of
                    RetentionTransient expiry _ | expiry ≤ day →
                        deleteIfSame plot tile
                    _ → pure False
            Just _ → pure False
            Nothing → case lookupSpecies (fiSpecies inst) catalog of
                Nothing → pure False
                Just species → do
                    case instanceLifespan species inst of
                        Nothing → pure False
                        Just lifespan → do
                            let deathElapsed = firstCalendarDay (\d →
                                    floraTotalAgeAt 0 (cpHealth plot) d ≥ lifespan)
                                deathDay = cpPlantedDay plot + deathElapsed
                            if deathDay > day
                                then pure False
                                else do
                                    let record = buildDeathRecord species daysPerYear
                                            tile deathDay deathElapsed inst CauseNatural
                                    case fcrRetention record of
                                        RetentionTransient expiry _ | expiry ≤ day →
                                            deleteIfSame plot tile
                                        _ → atomicModifyIORef' (wsCropPlotsRef ws) $ \current →
                                            case HM.lookup tile (cpsPlots current) of
                                                Just currentPlot | currentPlot ≡ plot
                                                    ∧ isNothing (cpCondition currentPlot) →
                                                        let marked = currentPlot
                                                                { cpCondition = Just record }
                                                        in (current { cpsPlots = HM.insert tile marked
                                                                (cpsPlots current) }, True)
                                                _ → (current, False))
  where
    deleteIfSame plot tile = atomicModifyIORef' (wsCropPlotsRef ws) $ \current →
        case HM.lookup tile (cpsPlots current) of
            Just currentPlot | currentPlot ≡ plot →
                (removePlot tile current, True)
            _ → (current, False)
