{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.River.Evolve
    ( evolveExistingRiver
    ) where
import UPrelude
import qualified Data.Vector as V
import World.Types
import World.Fluid.River (fixupSegmentContinuity)
import World.Geology.Hash
import World.Hydrology.River.Meander (meanderSegments)
import World.Hydrology.River.Tributary (buildTributarySegments)
import World.Geology.Timeline.Helpers (isActiveRiver, getRiverParamsFromPf)

-- * River evolution

evolveExistingRiver ∷ Word64 → Int → Int → Int
                    → PersistentFeature
                    → TimelineBuildState
                    → (PersistentFeature, [GeoEvent], TimelineBuildState)
evolveExistingRiver seed ageIdx periodIdx worldSize pf tbs =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        river = getRiverParamsFromPf pf
        h1 = hashGeo seed fidInt (900 + ageIdx)
        roll = hashToFloatGeo h1

        currentRiverCount = length $ filter (isActiveRiver . pfFeature) (tbsFeatures tbs)
        canBranch = currentRiverCount < 60
    in
    if roll < 0.25
    -- 25%: Meander — path changes in persistent state.
    --   Do NOT re-emit HydroEvent for the whole river.
    --   The valley already exists from when the river was first created.
    --   With target-based carving, re-emitting is harmless but wasteful.
    then let h2 = hashGeo seed fidInt (910 + ageIdx)
             meanderAmt = 0.15 + hashToFloatGeo h2 * 0.4
             newSegs = fixupSegmentContinuity $
                           meanderSegments seed fidInt meanderAmt (rpSegments river)
             newRiver = river { rpSegments = newSegs }
             evt = HydroModify fid (RiverMeander
                     (fromIntegral (hashGeo seed fidInt (911 + ageIdx)))
                     meanderAmt)
             updatedPf = pf
                 { pfFeature          = HydroShape $ RiverFeature newRiver
                 , pfLastActivePeriod = periodIdx
                 }
             tbs' = updateFeature fid (const updatedPf) tbs
         in (updatedPf, [evt], tbs')  -- FIXED: was [evt, HydroEvent (RiverFeature newRiver)]

    else if roll < 0.30 ∧ canBranch
    -- 5%: Branch — new tributary, emit HydroEvent only for the NEW tributary
    then let numSegs = V.length (rpSegments river)
         in if numSegs < 3
            then evolveDeepen seed ageIdx periodIdx pf river tbs
            else
            let (childId, tbs') = allocFeatureId tbs
                h2 = hashGeo seed fidInt (920 + ageIdx)
                h3 = hashGeo seed fidInt (921 + ageIdx)
                h4 = hashGeo seed fidInt (922 + ageIdx)
                h5 = hashGeo seed fidInt (923 + ageIdx)
                h6 = hashGeo seed fidInt (924 + ageIdx)

                branchSegIdx = hashToRangeGeo h2 1 (numSegs - 1)
                branchSeg = rpSegments river V.! min branchSegIdx (numSegs - 1)
                -- Wrap coordinates to canonical u-space. Parent river
                -- segments may have unwrapped coords from path tracing.
                GeoCoord bx0 by0 = rsStart branchSeg
                GeoCoord bex0 bey0 = rsEnd branchSeg
                (bx, by) = wrapCoord worldSize bx0 by0
                (bex, bey) = wrapCoord worldSize bex0 bey0

                segDX = fromIntegral (bex - bx) ∷ Float
                segDY = fromIntegral (bey - by) ∷ Float
                segAngle = atan2 segDY segDX
                side = if hashToFloatGeo h3 < 0.5 then 1.0 else -1.0
                branchAngle = segAngle + side * (0.7 + hashToFloatGeo h4 * 0.7)
                branchLen = hashToRangeGeo h5 15 50
                srcX = bx - round (fromIntegral branchLen * cos branchAngle)
                srcY = by - round (fromIntegral branchLen * sin branchAngle)

                numTribSegs = hashToRangeGeo h6 2 4
                branchElev = rsStartElev branchSeg
                rawTribSegs = buildTributarySegments seed fidInt
                                  srcX srcY bx by numTribSegs branchElev

                -- Force the trib's LAST segment to terminate at an
                -- elevation that makes its channel-floor match the main
                -- at the branch point. Without this, a shallow trib
                -- (depth ≈ 2-3) joining a deep main (depth ≈ 9) leaves
                -- a vertical water cliff of (mainDepth - tribDepth)
                -- tiles at the confluence — visible as a multi-tile
                -- waterfall the renderer draws as a straight drop.
                --
                --   target = branchElev − mainDepth + tribLastDepth
                --     ↳ trib cf at end = target − tribLastDepth
                --                       = branchElev − mainDepth
                --                       = main cf at branch
                --
                -- Clamped to ≤ rsStartElev of the last seg so we never
                -- ask the trib to flow uphill (audit #12 sub-bug B's
                -- monotonic-descent invariant). When the clamp bites,
                -- the residual mismatch is unavoidable without
                -- truncating the trib further upstream.
                tribSegs =
                    if V.null rawTribSegs then rawTribSegs
                    else let lastIdx = V.length rawTribSegs - 1
                             lastSeg = rawTribSegs V.! lastIdx
                             mainDepth = rsDepth branchSeg
                             target = branchElev - mainDepth + rsDepth lastSeg
                             clamped = min target (rsStartElev lastSeg)
                         in rawTribSegs V.// [(lastIdx, lastSeg { rsEndElev = clamped })]

                tributaryParams = RiverParams
                    { rpSourceRegion = GeoCoord srcX srcY
                    , rpMouthRegion  = GeoCoord bx by
                    , rpSegments     = tribSegs
                    , rpFlowRate     = rsFlowRate branchSeg * 0.4
                    }

                childPf = PersistentFeature
                    { pfId               = childId
                    , pfFeature          = HydroShape $ RiverFeature tributaryParams
                    , pfActivity         = FActive
                    , pfFormationPeriod   = periodIdx
                    , pfLastActivePeriod  = periodIdx
                    , pfEruptionCount     = 1
                    , pfParentId          = Just fid
                    }

                evt = HydroModify fid (RiverBranch
                        (GeoCoord bx by) branchAngle branchLen childId)

                newRiver = river
                    { rpFlowRate = rpFlowRate river + rpFlowRate tributaryParams * 0.5
                    }
                updatedPf = pf
                    { pfFeature          = HydroShape $ RiverFeature newRiver
                    , pfLastActivePeriod = periodIdx
                    , pfEruptionCount    = pfEruptionCount pf + 1
                    }

                tbs'' = registerFeature childPf tbs'
                tbs''' = updateFeature fid (const updatedPf) tbs''

            in ( updatedPf
               , [ evt
                 , HydroEvent (RiverFeature tributaryParams)
                 ]
               , tbs''' )

    else if roll < 0.50
    -- 20%: Deepen — NO HydroEvent, only state update
    then evolveDeepen seed ageIdx periodIdx pf river tbs

    else if roll < 0.60
    -- 10%: Widen — NO HydroEvent, only state update
    then let newSegs = V.map (\seg → seg
                 { rsWidth = min 16 (rsWidth seg + 1)
                 , rsValleyWidth = rsValleyWidth seg + 3
                 }) (rpSegments river)
             newRiver = river { rpSegments = newSegs }
             updatedPf = pf
                 { pfFeature          = HydroShape $ RiverFeature newRiver
                 , pfLastActivePeriod = periodIdx
                 }
             tbs' = updateFeature fid (const updatedPf) tbs
         in (updatedPf, [], tbs')

    else
    -- 40%: Continue unchanged
    (pf, [], tbs)

-- | Deepen — updates persistent state but emits NO events.
--   The deeper channel will be reflected next time the river
--   is emitted (meander or new creation).
--
--   Channel deepens toward a per-segment target derived from the
--   segment's flow rate (audit #14). Previously a flat cap of 20
--   meant old rivers monotonically reached 20 regardless of flow,
--   while late-formed rivers stayed shallow. Now small rivers
--   converge to shallow targets, big rivers to deep ones.
evolveDeepen ∷ Word64 → Int → Int
             → PersistentFeature → RiverParams
             → TimelineBuildState
             → (PersistentFeature, [GeoEvent], TimelineBuildState)
evolveDeepen seed ageIdx periodIdx pf river tbs =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        h2 = hashGeo seed fidInt (930 + ageIdx)
        deepenAmt = hashToRangeGeo h2 1 3
        -- Flow-driven target depth. Flow rates come from
        -- Simulation.hs as `accum * 0.005 + 0.1`: a small river
        -- (flow ≈ 0.5) targets depth 3; a large river (flow ≈ 5)
        -- targets 20. After merges (audit #12), downstream segments
        -- inherit the combined flow and can deepen further.
        targetDepth seg = min 20
                        $ max 3
                        $ floor (rsFlowRate seg * 4.0 ∷ Float)
        newSegs = fixupSegmentContinuity $ V.map (\seg →
            let cap = targetDepth seg
            in seg { rsDepth = min cap (rsDepth seg + deepenAmt)
                   , rsValleyWidth = rsValleyWidth seg + 1
                   }) (rpSegments river)
        newRiver = river { rpSegments = newSegs }
        updatedPf = pf
            { pfFeature          = HydroShape $ RiverFeature newRiver
            , pfLastActivePeriod = periodIdx
            , pfEruptionCount    = pfEruptionCount pf + 1
            }
        tbs' = updateFeature fid (const updatedPf) tbs
    in (updatedPf, [], tbs')

-- | Wrap (gx, gy) to canonical u-space.
wrapCoord ∷ Int → Int → Int → (Int, Int)
wrapCoord ws gx gy =
    let w = ws * chunkSize
        halfW = w `div` 2
        u = gx - gy
        v = gx + gy
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
    in ((wrappedU + v) `div` 2, (v - wrappedU) `div` 2)
