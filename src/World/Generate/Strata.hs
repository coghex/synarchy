{-# LANGUAGE Strict #-}
module World.Generate.Strata
    ( StrataState(..)
    , EventDelta(..)
    , PeriodStrataCache(..)
    , StrataZState(..)
    , buildStrataCache
    , buildColumnStrata
    ) where

import UPrelude
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import World.Types
import World.Geology (applyGeoEvent)
import World.Geology.Erosion (applyErosion, lookupRegionalErosion)
import World.Scale (WorldScale(..))
import World.Material (MaterialId(..), MaterialRegistry
                      , getMaterialProps, MaterialProps(..))

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
                 → MaterialRegistry → (Int, MaterialId)
                 → (Int, Int, Int, Int)
                 → V.Vector PeriodStrataCache
buildStrataCache timeline worldSize wsc gx gy registry (baseElev, baseMat)
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
            hardness  = mpHardness (getMaterialProps registry surfMat')
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
                hardness
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
    applyEvent _elev _surfMat (deltas, e, sm) (event, _bb) =
        let h = mpHardness (getMaterialProps registry sm)
            mod' = applyGeoEvent event worldSize gx gy e h
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
                        (\(!e, !_sm) ed → do
                            writeDelta mats startZ depth e
                                       (edDelta ed) (edIntrusion ed)
                                       (edMat ed)
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
writeDelta ∷ VUM.MVector s MaterialId → Int → Int → Int → Int → Int → MaterialId → ST s ()
writeDelta mats startZ depth elevBefore delta intrusion eventMat
    -- Deposition: fill from elevBefore+1 up to elevBefore+delta
    | delta > 0 = do
        let clampedIntrusion = min intrusion delta
            intrusionBottom = elevBefore + delta - clampedIntrusion + 1
            intrusionTop    = elevBefore + delta
        forM_ [max intrusionBottom startZ .. min intrusionTop (startZ + depth - 1)] $ \z →
            VUM.write mats (z - startZ) eventMat

    -- Erosion with soil backfill: write soil into the top `intrusion` tiles
    -- below the new surface (elevBefore + delta)
    | delta < 0 ∧ intrusion > 0 = do
        let newSurf = elevBefore + delta  -- delta is negative
            soilTop = newSurf
            soilBot = newSurf - intrusion + 1
        forM_ [max soilBot startZ .. min soilTop (startZ + depth - 1)] $ \z →
            VUM.write mats (z - startZ) eventMat

    -- No elevation change but soil intrusion (last-age in-situ weathering)
    | delta ≡ 0 ∧ intrusion > 0 = do
        let soilTop = elevBefore
            soilBot = elevBefore - intrusion + 1
        forM_ [max soilBot startZ .. min soilTop (startZ + depth - 1)] $ \z →
            VUM.write mats (z - startZ) eventMat

    -- Just stamp the surface tile
    | otherwise =
        when (elevBefore ≥ startZ ∧ elevBefore < startZ + depth) $
            VUM.write mats (elevBefore - startZ) eventMat
