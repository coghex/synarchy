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
    , RetentionSuccessor(..), lookupFloraCondition )
import World.Flora.ConditionSeam
import World.Flora.CropPlot (CropPlotOf(..))
import World.Flora.CropPlot
    ( cropPlotElapsedDays, cropPlotGrowthCondition, cropPlotInstance )
import World.Flora.Designation (forgetFloraInstances, replaceChunkForgettingFlora)
import World.Flora.Growth
    ( FloraGrowth(..), deadWindowDays, floraGrowth, growthRate
    , instanceGrowthCondition, instanceLifespan )
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
                        Just inst →
                            atomicModifyIORef' (wsFloraConditionsRef ws) $ \store →
                                case setOccurrenceCondition cat daysPerYear
                                         tile absDay inst cause store of
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
                        protected = maybe False durableNaturalOrSuccessor recorded
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

-- | Reconcile one page's flora over every crossed absolute calendar day.
--   The caller owns the page clock write, so a date jump is replayed at
--   its day boundaries and never borrows another page's clock.
reconcileFloraDays ∷ WorldSimCapability → WorldPageId → WorldState
                   → Int → Int → IO Bool
reconcileFloraDays _ _ _ fromDay toDay | toDay ≤ fromDay = pure False
reconcileFloraDays wsc _ ws fromDay toDay = do
    paramsM ← readIORef (wsGenParamsRef ws)
    let daysPerYear = maybe (calendarDaysPerYear defaultCalendarConfig)
            (calendarDaysPerYear ∘ wgpCalender) paramsM
    or <$> forM [fromDay + 1 .. toDay] (reconcileDay wsc ws daysPerYear)

reconcileDay ∷ WorldSimCapability → WorldState → Int → Int → IO Bool
reconcileDay wsc ws daysPerYear day = do
    storedChanged ← reconcileStoredOutcomes ws day
    plotChanged ← reconcilePlots wsc ws daysPerYear day
    occurrenceChanged ← reconcileLoadedOccurrences wsc ws daysPerYear day
    let changed = storedChanged ∨ plotChanged ∨ occurrenceChanged
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

reconcileLoadedOccurrences
    ∷ WorldSimCapability → WorldState → Int → Int → IO Bool
reconcileLoadedOccurrences wsc ws daysPerYear day = do
    catalog ← readIORef (wsFloraCatalogRef wsc)
    worldSize ← pageWrapWorldSize ws
    td ← readIORef (wsTilesRef ws)
    or <$> forM
        [ (lcCoord lc, fi)
        | lc ← HM.elems (wtdChunks td)
        , fi ← fcdInstances (lcFlora lc) ]
        (\(coord, fi) → reconcileOccurrence catalog worldSize coord fi)
  where
    reconcileOccurrence catalog worldSize coord fi = do
        let iid = fiInstanceId fi
        conditions ← readIORef (wsFloraConditionsRef ws)
        let mRecord = HM.lookup iid conditions
        case mRecord of
            Just record | fcrCondition record `elem`
                [ConditionDead, ConditionAbsent] → pure False
            _ → case lookupSpecies (fiSpecies fi) catalog of
                Nothing → pure False
                Just species → do
                    let growth = floraGrowth
                            (instanceGrowthCondition conditions fi)
                            species day fi
                    if not (fgDead growth)
                       then pure False
                       else do
                            let (canonicalGX, canonicalGY) =
                                    maybe (tileOf worldSize coord fi) fcrTile mRecord
                                current = case mRecord of
                                    Just r | fcrCondition r ≡ ConditionReseeded →
                                        anchorReseededDeath species day growth fi
                                    _ → fi
                                record = buildDeathRecord species daysPerYear
                                    (canonicalGX, canonicalGY) day day current CauseNatural
                            persistAnchor worldSize coord fi current
                            atomicModifyIORef' (wsFloraConditionsRef ws) $ \store →
                                (HM.insert iid record store, ())
                            pure True
    tileOf worldSize coord fi =
        let ChunkCoord cx cy = coord
        in canonicalTile worldSize
            (cx * chunkSize + fromIntegral (fiTileX fi))
            (cy * chunkSize + fromIntegral (fiTileY fi))
    persistAnchor worldSize coord old current
        | fiAge old ≡ fiAge current = pure ()
        | otherwise = do
            let (gx, gy) = tileOf worldSize coord old
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

anchorReseededDeath ∷ FloraSpecies → Int → FloraGrowth → FloraInstance
                    → FloraInstance
anchorReseededDeath species day growth fi = case instanceLifespan species fi of
    Nothing → fi
    Just lifespan →
        let cycleLen = lifespan + deadWindowDays
            baseline = fromIntegral (fgGeneration growth) * cycleLen
                + lifespan - fromIntegral day * growthRate (fiHealth fi)
        in fi { fiAge = baseline }

reconcilePlots ∷ WorldSimCapability → WorldState → Int → Int → IO Bool
reconcilePlots wsc ws daysPerYear day = do
    catalog ← readIORef (wsFloraCatalogRef wsc)
    store ← readIORef (wsCropPlotsRef ws)
    or <$> (forM (HM.toList (cpsPlots store)) $ \(tile, plot) → do
        let mRecord = cpCondition plot
            elapsed = cropPlotElapsedDays day plot
            inst = cropPlotInstance plot
        case mRecord of
            Just record → case (fcrCondition record, fcrRetention record) of
                (ConditionDead, RetentionTransient expiry _)
                    | expiry ≤ day → atomicModifyIORef' (wsCropPlotsRef ws) $ \current →
                        case HM.lookup tile (cpsPlots current) of
                            Just currentPlot | currentPlot ≡ plot →
                                (removePlot tile current, True)
                            _ → (current, False)
                _ → pure False
            Nothing → case lookupSpecies (fiSpecies inst) catalog of
                Nothing → pure False
                Just species → do
                    let growth = floraGrowth (cropPlotGrowthCondition plot)
                            species elapsed inst
                    if not (fgDead growth)
                       then pure False
                       else do
                            let record = buildDeathRecord species daysPerYear
                                    tile day elapsed inst CauseNatural
                            atomicModifyIORef' (wsCropPlotsRef ws) $ \current →
                                case HM.lookup tile (cpsPlots current) of
                                    Just currentPlot | currentPlot ≡ plot
                                        ∧ isNothing (cpCondition currentPlot) →
                                            let marked = currentPlot
                                                    { cpCondition = Just record }
                                            in (current { cpsPlots = HM.insert tile marked
                                                    (cpsPlots current) }, True)
                                    _ → (current, False))
