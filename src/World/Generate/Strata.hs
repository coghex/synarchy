{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Strata
    ( StrataState(..)
    , EventDelta(..)
    , PeriodStrataCache(..)
    , StrataZState(..)
    , buildStrataCache
    , buildColumnStrata
    , writeDelta
    , applyCachedPeriod
    , applyEventDelta
    , applyPeriodStrata
    , applyEventStrata
    , applyDelta
    ) where

import UPrelude
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import World.Types
import World.Geology (applyGeoEvent, GeoModification(..))
import World.Geology.Erosion (applyErosion, lookupRegionalErosion)
import World.Geology.Timeline.Types (tileInBBoxWrapped)
import World.Scale (WorldScale(..))
import World.Material (MaterialId(..))

data StrataState = StrataState
    { ssElev      ∷ !Int
    , ssSurfMat   ∷ !MaterialId
    , ssUplift    ∷ !Int
    , ssZMat      ∷ !MaterialId
    , ssNeighbors ∷ !(Int, Int, Int, Int)
    }

data EventDelta = EventDelta
    { edDelta     ∷ !Int
    , edIntrusion ∷ !Int
    , edMat       ∷ !MaterialId
    }

data PeriodStrataCache = PeriodStrataCache
    { pscEvents          ∷ !(V.Vector EventDelta)
    , pscErosionDelta    ∷ !Int
    , pscErosionMat      ∷ !MaterialId
    , pscErosionIntrusion ∷ !Int
    }

data StrataZState = StrataZState
    { szElev    ∷ !Int
    , szSurfMat ∷ !MaterialId
    , szUplift  ∷ !Int
    , szZMat    ∷ !MaterialId
    }

-- | Build a per-period stratigraphy cache for one column.
--
--   Performance optimization: neighbor elevations are passed as their
--   FINAL post-timeline values (from finalElevVec), not base values.
--   This eliminates the expensive advanceNeighbor computation that
--   previously re-applied every geo event to 4 neighbors per period
--   (~200K applyGeoEvent calls per chunk).
--
--   Additionally, uses gpTaggedEvents with per-column bounding-box
--   filtering to skip events that can't affect this column. This
--   avoids the expensive distance/sqrt computation inside applyGeoEvent
--   for events on the other side of the world.
buildStrataCache ∷ GeoTimeline → Int → WorldScale → Int → Int
                 → (Int, MaterialId)
                 → (Int, Int, Int, Int)
                 → V.Vector PeriodStrataCache
buildStrataCache timeline worldSize wsc gx gy (baseElev, baseMat)
                 (nFinalN, nFinalS, nFinalE, nFinalW) =
    let initState = (baseElev, baseMat)
        caches = snd $ foldl' step (initState, []) (gtPeriods timeline)
    in V.fromList (reverse caches)
  where
    step ((elev, surfMat), acc) period =
        let -- Filter to events whose bbox contains this column.
            -- Most events are spatially local (volcanoes, craters, rivers),
            -- so for a typical column this cuts ~200 events down to ~0-5.
            relevantEvents = filter (\(_, bb) →
                tileInBBoxWrapped worldSize gx gy bb
                ) (gpTaggedEvents period)
            (eventDeltas, elev', surfMat') =
                foldl' (applyEvent elev surfMat) ([], elev, surfMat)
                       relevantEvents

            eventsVec = V.fromList (reverse eventDeltas)

            -- Use pre-computed final neighbor elevations directly.
            -- No advanceNeighbor calls needed — eliminates ~4 × events
            -- applyGeoEvent calls per period per column.
            regionalParams = lookupRegionalErosion
                (gpErosion period) (gpRegionalErosion period)
                worldSize gx gy
            erosionMod = applyErosion
                regionalParams
                worldSize
                (gpDuration period)
                (wsScale wsc)
                (unMaterialId surfMat')
                elev'
                (nFinalN, nFinalS, nFinalE, nFinalW)

            erosionDelta = gmElevDelta erosionMod
            erosionMat = case gmMaterialOverride erosionMod of
                Just m  → MaterialId m
                Nothing → surfMat'
            erosionIntrusion = gmIntrusionDepth erosionMod

            elev'' = elev' + erosionDelta
            st' = (elev'', erosionMat)

            cache = PeriodStrataCache
                { pscEvents           = eventsVec
                , pscErosionDelta     = erosionDelta
                , pscErosionMat       = erosionMat
                , pscErosionIntrusion = erosionIntrusion
                }
        in (st', cache : acc)

    -- Now receives tagged (event, bbox) pairs instead of raw events
    applyEvent elev surfMat (deltas, e, sm) (event, _bb) =
        let mod' = applyGeoEvent event worldSize gx gy e
            delta = gmElevDelta mod'
            intrusion = gmIntrusionDepth mod'
            eventMat = case gmMaterialOverride mod' of
                Just m  → MaterialId m
                Nothing → sm
            e' = e + delta
        in (EventDelta delta intrusion eventMat : deltas, e', eventMat)

buildColumnStrata ∷ V.Vector PeriodStrataCache
                  → (Int, MaterialId)
                  → Int → Int
                  → VU.Vector MaterialId
buildColumnStrata caches (baseElev, baseMat) startZ endZ =
    let depth = endZ - startZ + 1
    in if depth ≤ 0
       then VU.empty
       else runST $ do
            mats ← VUM.replicate depth baseMat

            let applyCache (!elev, !surfMat) cache = do
                    -- Apply each event's writes
                    (elev', _surfMat') ← V.foldM'
                        (\(!e, !sm) ed → do
                            writeDelta mats startZ depth e
                                       (edDelta ed) (edIntrusion ed) (edMat ed)
                            pure (e + edDelta ed, edMat ed)
                        ) (elev, surfMat) (pscEvents cache)

                    -- Apply erosion writes
                    writeDelta mats startZ depth
                        elev'
                        (pscErosionDelta cache)
                        (pscErosionIntrusion cache)
                        (pscErosionMat cache)

                    let elev'' = elev' + pscErosionDelta cache
                    pure (elev'', pscErosionMat cache)

            V.foldM'_ applyCache (baseElev, baseMat) caches

            VU.unsafeFreeze mats

-- | Write the material effects of a single delta (event or erosion)
--   into the mutable material vector. Only touches z-levels within
--   [startZ .. startZ + depth - 1].
writeDelta ∷ VUM.MVector s MaterialId
           → Int → Int
           → Int → Int → Int
           → MaterialId
           → ST s ()
writeDelta mats startZ depth elevBefore delta intrusion eventMat
    | delta > 0 =
        let clampedIntrusion = min intrusion delta
            upliftAmount = delta - clampedIntrusion
            intrusionBottom = elevBefore + upliftAmount + 1
            intrusionTop = elevBefore + delta
        in when (clampedIntrusion > 0) $
            forM_ [max intrusionBottom startZ .. min intrusionTop (startZ + depth - 1)] $ \z →
                VUM.write mats (z - startZ) eventMat

    | delta < 0 =
        let newSurfZ = elevBefore + delta
        in do
            when (newSurfZ ≥ startZ ∧ newSurfZ < startZ + depth) $
                VUM.write mats (newSurfZ - startZ) eventMat
            when (intrusion > 0) $
                forM_ [max newSurfZ startZ .. min (newSurfZ + intrusion - 1) (startZ + depth - 1)] $ \z →
                    VUM.write mats (z - startZ) eventMat

    | otherwise =
        when (elevBefore ≥ startZ ∧ elevBefore < startZ + depth) $
            VUM.write mats (elevBefore - startZ) eventMat

applyCachedPeriod ∷ Int → StrataZState → PeriodStrataCache → StrataZState
applyCachedPeriod queryZ state cache =
    let afterEvents = V.foldl' (applyEventDelta queryZ) state (pscEvents cache)

        (surfMat', uplift', zMat') =
            applyDelta queryZ
                       (szElev afterEvents)
                       (pscErosionDelta cache)
                       (pscErosionMat cache)
                       (pscErosionIntrusion cache)
                       (szSurfMat afterEvents)
                       (szUplift afterEvents)
                       (szZMat afterEvents)

        elev' = szElev afterEvents + pscErosionDelta cache
    in StrataZState
        { szElev    = elev'
        , szSurfMat = pscErosionMat cache
        , szUplift  = uplift'
        , szZMat    = zMat'
        }

applyEventDelta ∷ Int → StrataZState → EventDelta → StrataZState
applyEventDelta queryZ state ed =
    let (surfMat', uplift', zMat') =
            applyDelta queryZ
                       (szElev state)
                       (edDelta ed)
                       (edMat ed)
                       (edIntrusion ed)
                       (szSurfMat state)
                       (szUplift state)
                       (szZMat state)

        elev' = szElev state + edDelta ed
    in StrataZState
        { szElev    = elev'
        , szSurfMat = surfMat'
        , szUplift  = uplift'
        , szZMat    = zMat'
        }

applyPeriodStrata ∷ Int → WorldScale → Int → Int → Int
                  → StrataState → GeoPeriod → StrataState
applyPeriodStrata worldSize wsc gx gy queryZ state period =
    let afterEvents = foldl' (applyEventStrata worldSize gx gy queryZ)
                             state (gpEvents period)

        (nN, nS, nE, nW) = ssNeighbors afterEvents
        advanceNeighbor nElev ngx ngy =
            foldl' (\e ev → e + gmElevDelta (applyGeoEvent ev worldSize ngx ngy e))
                   nElev (gpEvents period)
        nN' = advanceNeighbor nN gx (gy - 1)
        nS' = advanceNeighbor nS gx (gy + 1)
        nE' = advanceNeighbor nE (gx + 1) gy
        nW' = advanceNeighbor nW (gx - 1) gy

        elev' = ssElev afterEvents
        surfMat' = ssSurfMat afterEvents
        erosionMod = applyErosion
            (gpErosion period)
            worldSize
            (gpDuration period)
            (wsScale wsc)
            (unMaterialId surfMat')
            elev'
            (nN', nS', nE', nW')
        erosionDelta = gmElevDelta erosionMod
        erosionMatId = case gmMaterialOverride erosionMod of
            Just m  → MaterialId m
            Nothing → surfMat'
        (surfMat'', uplift'', zMat'') =
            applyDelta queryZ elev' erosionDelta erosionMatId
                       (gmIntrusionDepth erosionMod)
                       surfMat' (ssUplift afterEvents) (ssZMat afterEvents)
        elev'' = elev' + erosionDelta
    in StrataState
        { ssElev     = elev''
        , ssSurfMat  = surfMat''
        , ssUplift   = uplift''
        , ssZMat     = zMat''
        , ssNeighbors = (nN', nS', nE', nW')
        }

applyEventStrata ∷ Int → Int → Int → Int → StrataState → GeoEvent → StrataState
applyEventStrata worldSize gx gy queryZ state event =
    let mod' = applyGeoEvent event worldSize gx gy (ssElev state)
        delta = gmElevDelta mod'
        intrusion = gmIntrusionDepth mod'
        eventMat = case gmMaterialOverride mod' of
            Just m  → MaterialId m
            Nothing → ssSurfMat state
        (surfMat', uplift', zMat') =
            applyDelta queryZ (ssElev state) delta eventMat intrusion
                       (ssSurfMat state) (ssUplift state) (ssZMat state)
        elev' = ssElev state + delta
    in state
        { ssElev    = elev'
        , ssSurfMat = surfMat'
        , ssUplift  = uplift'
        , ssZMat    = zMat'
        }

applyDelta ∷ Int → Int → Int → MaterialId → Int → MaterialId → Int → MaterialId
           → (MaterialId, Int, MaterialId)
applyDelta queryZ elevBefore delta eventMat intrusion surfMat uplift zMat
    | delta > 0 =
        let clampedIntrusion = min intrusion delta
            upliftAmount = delta - clampedIntrusion
            intrusionBottom = elevBefore + upliftAmount + 1
            intrusionTop = elevBefore + delta
            inIntrusion = clampedIntrusion > 0
                        ∧ queryZ ≥ intrusionBottom
                        ∧ queryZ ≤ intrusionTop
            inUplift = upliftAmount > 0
                     ∧ queryZ > elevBefore
                     ∧ queryZ ≤ elevBefore + upliftAmount
            newUplift = if inUplift then uplift + upliftAmount else uplift
            newZMat = if inIntrusion then eventMat else zMat
            newSurf = eventMat
        in (newSurf, newUplift, newZMat)
    | delta < 0 =
        let newSurfZ = elevBefore + delta
            newSurf = eventMat
            inFill = intrusion > 0
                   ∧ queryZ ≥ newSurfZ
                   ∧ queryZ < newSurfZ + intrusion
            atSurface = queryZ ≡ newSurfZ
            newZMat = if inFill ∨ atSurface
                      then eventMat
                      else zMat
        in (newSurf, uplift, newZMat)
    | otherwise =
        let newZMat = if queryZ ≡ elevBefore ∧ eventMat ≠ surfMat
                      then eventMat
                      else zMat
        in (eventMat, uplift, newZMat)
