{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Simulation.Flow
    ( simulateHydrology
    ) where

import UPrelude
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VA
import Control.Monad.ST (runST)
import World.Types
import World.Weather.Types (ClimateState(..))
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))
import World.Hydrology.Simulation.Types
    (ElevGrid(..), FlowResult(..), effRiverThreshold, minLakeDepth, minExtendWorld)
import World.Hydrology.Simulation.PriorityFlood (fillDepressions)
import World.Hydrology.Simulation.LakeDedup (dedupLakes)

-- * Flow Simulation

simulateHydrology ∷ Word64 → Int → Int → ElevGrid → ClimateState → FlowResult
simulateHydrology _seed worldSize _ageIdx grid climate =
    let gridW   = egGridW grid
        totalSamples = gridW * gridW
        origElev = egElev grid
        landVec  = egLand grid
        gxVec    = egGX grid
        gyVec    = egGY grid

        ---------------------------------------------------
        -- Step 1: Fill depressions + drainage directions
        ---------------------------------------------------
        -- fillDepressions now returns both the filled surface
        -- AND the drainage parent for each cell, recorded during
        -- the priority-flood. This correctly handles flat areas
        -- (filled depressions) where steepest-descent would fail.
        (filledElev, flowDirVec) = fillDepressions grid

        ---------------------------------------------------
        -- Step 2: Flow accumulation
        ---------------------------------------------------
        -- Sort indices by descending filled elevation using in-place
        -- vector sort. Much faster than list sort on 262K elements
        -- because it avoids list node allocation and GC pressure.
        sortedByElev ∷ VU.Vector Int
        sortedByElev = runST $ do
            mv ← VUM.generate totalSamples id
            VA.sortBy (\a b → compare (filledElev VU.! b) (filledElev VU.! a)) mv
            VU.unsafeFreeze mv

        -- Precipitation-weighted flow accumulation.
        -- Each cell contributes water based on local rainfall + snowmelt
        -- instead of a flat 1 unit. This ensures rivers form from wet
        -- regions and mountain snowmelt, not just drainage area.
        accumVec ∷ VU.Vector Int
        accumVec = runST $ do
            mv ← VUM.new totalSamples
            -- Initialize each cell's water contribution from climate
            forM_ [0 .. totalSamples - 1] $ \idx →
                if not (landVec VU.! idx)
                then VUM.write mv idx 0
                else do
                    let gx = gxVec VU.! idx
                        gy = gyVec VU.! idx
                        LocalClimate{lcTemp=temp, lcPrecip=precip
                                    , lcSnow=snowFrac} =
                            lookupLocalClimate climate worldSize gx gy
                        -- Direct rainfall (non-snow precipitation)
                        rainfall = precip * (1.0 - snowFrac)
                        -- Snowmelt: snow that thaws. Ramps from 0 at
                        -- -5°C (permanently frozen) to full melt at +5°C.
                        -- This places river sources at the snowmelt line
                        -- below glacial peaks, not on the frozen summits.
                        meltFactor = max 0.0 (min 1.0 ((temp + 5.0) / 10.0))
                        snowmelt = snowFrac * precip * meltFactor
                        -- Total water entering the system at this cell.
                        -- Scale ×10 to keep integer precision (1 old unit = 10 new).
                        -- Minimum 1 so that even dry cells contribute a trickle.
                        waterUnits = max 1 (round ((rainfall + snowmelt) * 10.0) ∷ Int)
                    VUM.write mv idx waterUnits
            -- Accumulate downstream (topological order)
            VU.forM_ sortedByElev $ \idx → do
                myAccum ← VUM.read mv idx
                let downstream = flowDirVec VU.! idx
                when (downstream ≥ 0) $
                    VUM.modify mv (+ myAccum) downstream
            VU.unsafeFreeze mv

        ---------------------------------------------------
        -- Step 3: Lakes
        ---------------------------------------------------
        -- Raw lake candidates — sorted deepest-first so large basins
        -- get priority during dedup (they "claim" more territory).
        lakes ∷ [LakeParams]
        lakes = sortBy (comparing (Down . lkDepth)) $ catMaybes
            [ let origE  = origElev VU.! idx
                  fillE  = filledElev VU.! idx
                  depth  = fillE - origE
                  gx     = gxVec VU.! idx
                  gy     = gyVec VU.! idx
                  -- Radius scales with sqrt(depth) — deep basins get
                  -- proportionally larger lakes (power-law distribution).
                  -- Cap at 50 to keep spillway checks within land.
                  r = min 50 (max 4 (round (sqrt (fromIntegral depth ∷ Float)
                           * (4.0 ∷ Float))))
              in if landVec VU.! idx ∧ depth ≥ minLakeDepth
                 then Just LakeParams
                    { lkCenter  = GeoCoord gx gy
                    , lkRadius  = r
                    , lkSurface = fillE
                    , lkDepth   = depth
                    , lkSource  = TectonicBasin
                    }
                 else Nothing
            | idx ← [0 .. totalSamples - 1]
            ]

        dedupedLakes = dedupLakes worldSize lakes

        ---------------------------------------------------
        -- Step 4: River sources
        -- A cell is a headwater if:
        --   1. It's land
        --   2. It has accumulation >= riverThreshold
        --   3. No upstream neighbor has accumulation >= riverThreshold
        --      (i.e., this is where significant flow begins)
        --
        -- riverThreshold scales with the world's avg precipitation so
        -- the drainage-area requirement (in cells) stays consistent
        -- across arid / balanced / wet climates (audit #15).
        ---------------------------------------------------

        riverThreshold = effRiverThreshold climate

        -- Pre-compute which cells have a qualified upstream contributor.
        -- For each cell with sufficient flow, mark its downstream target
        -- as "has qualified upstream". This is O(n) instead of O(n×8).
        hasQualifiedUpstream ∷ VU.Vector Bool
        hasQualifiedUpstream = runST $ do
            mv ← VUM.replicate totalSamples False
            forM_ [0 .. totalSamples - 1] $ \idx →
                when (accumVec VU.! idx ≥ riverThreshold) $ do
                    let downstream = flowDirVec VU.! idx
                    when (downstream ≥ 0 ∧ downstream < totalSamples) $
                        VUM.write mv downstream True
            VU.unsafeFreeze mv

        headwaters = filter (\idx →
            landVec VU.! idx
            ∧ accumVec VU.! idx ≥ riverThreshold
            ∧ not (hasQualifiedUpstream VU.! idx)
            ) [0 .. totalSamples - 1]

        -- Sort by accumulation descending — biggest rivers first
        sortedSources = sortBy (comparing (Down . (accumVec VU.!))) headwaters

        -- For each cell, the upstream neighbour that contributes the
        -- most flow (its "main-stem" parent). Built by inverting the
        -- flow grid: every land cell names one downstream target, so we
        -- record, per target, the contributor with the largest
        -- accumulation. O(n).
        bestUpstream ∷ VU.Vector Int
        bestUpstream = runST $ do
            parent  ← VUM.replicate totalSamples (-1)
            bestAcc ← VUM.replicate totalSamples (minBound ∷ Int)
            forM_ [0 .. totalSamples - 1] $ \idx → do
                let d = flowDirVec VU.! idx
                when (d ≥ 0 ∧ d < totalSamples) $ do
                    let a = accumVec VU.! idx
                    cur ← VUM.read bestAcc d
                    when (a > cur) $ do
                        VUM.write bestAcc d a
                        VUM.write parent  d idx
            VU.unsafeFreeze parent

        -- Walk from a headwater up the main-stem parent chain toward the
        -- catchment divide (issue #221). A "headwater" is only the cell
        -- where flow first crossed the river threshold — typically LOW,
        -- near the wet coastal mountains where precipitation
        -- concentrates, which is why rivers used to start near the coast
        -- and stay short. Following the largest upstream contributor
        -- moves the SOURCE toward the true drainage head, so the river
        -- originates inland and grows as it descends. This is additive:
        -- every river is preserved and merely lengthened up its own
        -- catchment, so small coastal catchments stay short while large
        -- interior catchments become long — sea-draining rivers reach
        -- the sea, closed-basin rivers still terminate inland.
        --
        -- The walk STOPS on three conditions:
        --
        --  1. The next cell upstream is a LAKE / basin cell — its
        --     depression-filled surface stands `minLakeDepth` or more
        --     above its raw terrain, i.e. it sits under a lake. Stopping
        --     here keeps the source BELOW the lake, at the basin's
        --     outflow, so the river flows *into* the lake rather than
        --     tracing through and carving it out. This preserves the
        --     interior lakes (closed-basin drainages stay lakes, as
        --     intended) and, because those basins are no longer drained,
        --     stops both the lava that sat beneath them from being
        --     exposed and the basin-floor valley from being lava-flooded
        --     (issue #221 — the lake-collapse / lava blow-up that
        --     extending sources had caused on small volcanic worlds).
        --
        --  2. The climb exceeds `maxClimb` above the headwater. This
        --     keeps the source in the foothills rather than on the
        --     volcanic peaks, whose fresh headwater valleys the
        --     lava-pool pour (Magma.Pool floods local depressions to
        --     their rim) would otherwise bury.
        --
        --  3. The usual ridge / off-land / step bounds.
        --
        -- The upstream graph is a forest (flow is acyclic) so it cannot
        -- loop; maxWalk also bounds the cost.
        maxClimb ∷ Int
        maxClimb = 40

        -- A grid cell is "in a lake" if it is a flooded basin cell —
        -- its depression-filled surface stands above its raw terrain.
        -- This matches the actual world-lake pipeline, which makes EVERY
        -- flooded basin a rendered lake/pond (basin tile = filled >
        -- terrain, kept by area ≥ minBasinTiles = 1 — depth is NOT a
        -- factor; World.Fluid.Lake.Identify). An earlier per-cell
        -- `depth ≥ minLakeDepth` test, and even a whole-component test
        -- gated on the component's deepest cell, both missed shallow
        -- ponds (max depth 1..8) — which ARE lakes — and let a river be
        -- sourced inside one and traced/carved out through it (issue
        -- #221 review). Any flooded land cell is therefore a lake cell;
        -- `walkToDivide` stops below it and `dropToOutflow` descends a
        -- flooded headwater to the basin's spill point, so rivers start
        -- on well-drained ground and flow INTO ponds, never out of them.
        isLakeCell ∷ Int → Bool
        isLakeCell i = landVec VU.! i ∧ filledElev VU.! i > origElev VU.! i
        walkToDivide ∷ Int → Int
        walkToDivide start = go start (0 ∷ Int)
          where
            maxWalk = 4 * gridW
            startElev = origElev VU.! start
            go idx steps
                | steps ≥ maxWalk = idx
                | otherwise =
                    let up = bestUpstream VU.! idx
                    in if up < 0 ∨ not (landVec VU.! up)
                          ∨ isLakeCell up
                          ∨ origElev VU.! up - startElev > maxClimb
                       then idx
                       else go up (steps + 1)

        -- A headwater (first threshold-crossing cell) can itself fall
        -- INSIDE a filled lake basin — lake flats are land, so they pass
        -- the headwater test. Sourcing a river there (or, after
        -- `walkToDivide`, just above it) would carve the basin out and
        -- drain the lake. So if the chosen source is a lake cell, descend
        -- the flow chain to the basin's OUTFLOW (the first non-lake cell)
        -- and source there, so the river starts at the lake's edge and
        -- flows AWAY from it — the lake is preserved (issue #221). Bounded
        -- by maxWalk; falls back to the last cell if the chain hits the
        -- ocean while still flooded (degenerate lake-to-sea case).
        dropToOutflow ∷ Int → Int
        dropToOutflow start = go start (0 ∷ Int)
          where
            maxWalk = 4 * gridW
            go idx steps
                | steps ≥ maxWalk      = idx
                | not (isLakeCell idx) = idx
                | otherwise =
                    let d = flowDirVec VU.! idx
                    in if d ≥ 0 ∧ d < totalSamples
                       then go d (steps + 1)
                       else idx

        -- Inland-origin source extension is applied only on real-scale
        -- worlds (issue #221). On tiny worlds (size 32/64 — the
        -- regression-gate sizes) it is both pointless (a 512–1024-tile
        -- world can't host a long river) and actively harmful: their
        -- volcanism is packed densely enough that an extended river
        -- routes through a caldera, breaches its rim and lets the
        -- lava-pool pour flood the low terrain — observed as 100s of
        -- new lava tiles on ground dropped ~160 z (seed 13579 w32),
        -- which the per-source lake-stop and climb cap can't prevent
        -- because the breach is mid-course, not at the source. Gating
        -- to worldSize ≥ `minExtendWorld` confines the feature to the
        -- worlds it targets (≥128 verified lava-neutral) and leaves the
        -- small worlds — and the magma system — exactly as they were.
        extendSources = worldSize ≥ minExtendWorld

        riverSources ∷ [(Int, Int, Int, Float)]
        riverSources = map (\idx →
            let srcIdx
                    | not extendSources = idx              -- small worlds: unchanged
                    | isLakeCell idx    = dropToOutflow idx -- headwater in a lake
                    | otherwise         = walkToDivide idx  -- extend up to divide
                gx = gxVec VU.! srcIdx
                gy = gyVec VU.! srcIdx
                elev = origElev VU.! srcIdx
                -- Flow magnitude characterises the whole river, so it is
                -- taken from the headwater (where flow first
                -- concentrated), not the thin divide cell. Accumulation
                -- is precipitation-weighted (×10 scale); the same flow
                -- formula as before.
                flow = fromIntegral (accumVec VU.! idx) * 0.005 + 0.1
            in (gx, gy, elev, flow)
            ) sortedSources

    in FlowResult { frRiverSources = riverSources
                   , frLakes = dedupedLakes
                   , frFilledElev = filledElev
                   , frFlowDir = flowDirVec
                   }
