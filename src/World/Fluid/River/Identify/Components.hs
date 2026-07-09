{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | River-component tracing for the river-identification pipeline:
--   extends primary (above-threshold) flow into full chains, clamps
--   the water surface so no downstream step exceeds the waterfall
--   quantum, expands each chain to its flow-derived width, labels and
--   culls connected components, and builds the per-river bookkeeping
--   ('World.Fluid.River.Types.River') consumed by
--   'World.Fluid.River.Identify.traceRivers'. See that module's header
--   comment for the full pipeline overview.
module World.Fluid.River.Identify.Components
    ( extendRiverChains
    , clampCentreSurfaces
    , clampLateralSurfaces
    , expandWidth
    , depthFromRadius
    , cullByLength
    , labelRiverComponents
    , buildRivers
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Chunk.Types (chunkSize)
import World.Constants (seaLevel)
import World.Fluid.River.Identify.Common
    ( dirNorth, dirEast, dirSouth, dirWest, dirNone, stepDir, sortDescOn )
import World.Fluid.River.Types (River(..))

-- | Reference flow level that maps to one tile of perpendicular width.
--   See 'widthRadiusFromFlow'. With @widthScale = 500@: flow 500 →
--   radius 0 (1 wide), flow 1000 → radius 1 (3 wide), flow 2000 → 2
--   (5 wide), flow 4000+ → 3 (7 wide).
widthScale ∷ Int
widthScale = 500

-- | Maximum half-width. Caps the widest river at 7 tiles end-to-end
--   (centre + 3 perpendicular on each side).
maxWidthRadius ∷ Int
maxWidthRadius = 3

-- | Per-tile width radius from accumulated flow. Logarithmic ramp so
--   small rivers stay narrow but big ones still hit the cap.
widthRadiusFromFlow ∷ Int → Int
widthRadiusFromFlow f =
    let ratio = fromIntegral (max 1 f) / fromIntegral widthScale ∷ Float
    in max 0 (min maxWidthRadius
                  (floor (logBase 2.0 (max 1.0 ratio)) ∷ Int))

-- | Extend each primary-river chain downstream along the D4 dir until
--   it reaches a terminus (lake / ocean tile / world boundary / sink).
--   Tiles below threshold are still marked river so an arid stretch
--   mid-chain doesn't fragment the river — width will end up at
--   radius 0 for those tiles because flow there is small.
--
--   Pass in descending-z order so by the time we visit a tile, any
--   upstream chain that reaches it has already marked it.
extendRiverChains
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Bool       -- ^ isPrimary (flow ≥ threshold + land tile)
    → VU.Vector Int        -- ^ ascending z order
    → VU.Vector Bool
extendRiverChains worldTiles terrain lakeIdAt dir isPrimary ascOrder =
    let nTiles = worldTiles * worldTiles
        nOrd   = VU.length ascOrder
    in VU.create $ do
        isR ← VUM.replicate nTiles False
        forM_ [0 .. nTiles - 1] $ \i →
            when (isPrimary VU.! i) (VUM.write isR i True)
        forM_ [nOrd - 1, nOrd - 2 .. 0] $ \k → do
            let i = ascOrder VU.! k
            mine ← VUM.read isR i
            when mine $
                case stepDir worldTiles i (dir VU.! i) of
                    Nothing → pure ()
                    Just dn → do
                        let dnT       = terrain  VU.! dn
                            dnLake    = lakeIdAt VU.! dn ≥ 0
                            dnOcean   = dnT ≠ minBound ∧ dnT ≤ seaLevel
                                      ∧ not dnLake
                            dnBeyond  = dnT ≡ minBound
                        -- Stop the river at any terminus tile (lake,
                        -- ocean, void). Otherwise propagate the marker.
                        when (not dnLake ∧ not dnOcean ∧ not dnBeyond) $
                            VUM.write isR dn True
        pure isR

-- | Waterfall clamp: per-centre water surface, computed in ascending
--   terrain order so each tile's downstream neighbour is finalised
--   first ('World.Fluid.River.Identify.Flow.computeDescentDirs' only
--   ever picks a strictly lower neighbour, so a downstream tile
--   always sits in an earlier bucket of the ascending sort):
--
--       surf[u] = min(terrain[u], surf[downstream u] + waterfallQuantum)
--
--   Monotonicity (non-increasing downstream) falls out automatically:
--   terrain[u] > terrain[dn] ≥ surf[dn], so surf[u] ≥ surf[dn], and
--   the min bounds every downstream step at 'waterfallQuantum'. Where
--   terrain drops faster than the quantum (cliffs), the clamped
--   surface dips below local terrain and 'World.Fluid.River.Identify.BedDepth.computeCarveDelta'
--   deepens the channel — the river cuts a stepped gorge through the
--   scarp instead of rendering one tall water wall. On reaches with
--   slope ≤ quantum the min picks terrain and behaviour is unchanged.
--
--   Non-centre tiles hold minBound.
clampCentreSurfaces
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Bool       -- ^ isRiverCentre (after extension)
    → VU.Vector Int        -- ^ ascending z order
    → Int                  -- ^ waterfall quantum (max downstream step)
    → VU.Vector Int
clampCentreSurfaces worldTiles terrain dir isRiverCentre ascOrder waterfallQuantum =
    let nTiles = worldTiles * worldTiles
        nOrd   = VU.length ascOrder
    in VU.create $ do
        surf ← VUM.replicate nTiles (minBound ∷ Int)
        forM_ [0 .. nOrd - 1] $ \k → do
            let i = ascOrder VU.! k
            when (isRiverCentre VU.! i) $ do
                let t = terrain VU.! i
                s ← case stepDir worldTiles i (dir VU.! i) of
                    Nothing → pure t
                    Just dn → do
                        dnS ← VUM.read surf dn
                        -- dn not a river centre (lake/ocean terminus):
                        -- the chain ends here at its own terrain. The
                        -- strict-descent property guarantees a centre
                        -- dn is already finalised.
                        pure $ if dnS ≡ minBound
                               then t
                               else min t (dnS + waterfallQuantum)
                VUM.write surf i s
        pure surf

-- | Lateral waterfall clamp: relax the surface field until EVERY pair
--   of 4-adjacent river tiles differs by at most 'waterfallQuantum'.
--   The chain clamp only constrains flow edges; this pass also covers
--   meander necks, parallel reaches, wing-tile adjacencies and
--   breakthrough paths. Worklist relaxation: lowering a tile can only
--   force its neighbours lower, surfaces are bounded below, so the
--   fixpoint (the q-Lipschitz lower envelope of the input surfaces
--   over the river adjacency graph) is reached in finite steps —
--   in practice one or two visits per affected tile.
clampLateralSurfaces
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Bool       -- ^ isRiverTile (post-breakthrough)
    → VU.Vector Int        -- ^ surface z (post-breakthrough)
    → Int                  -- ^ waterfall quantum (max lateral step)
    → VU.Vector Int
clampLateralSurfaces worldTiles isRiverTile surfZ0 waterfallQuantum =
    let nTiles = VU.length surfZ0
        seeds  = [ i | i ← [0 .. nTiles - 1], isRiverTile VU.! i ]
    in VU.create $ do
        surf ← VU.thaw surfZ0
        queueRef ← newSTRef seeds
        let push i = modifySTRef' queueRef (i:)
            relaxNeighbour sI d i =
                case stepDir worldTiles i d of
                    Nothing → pure ()
                    Just n → when (isRiverTile VU.! n) $ do
                        sN ← VUM.read surf n
                        when (sN > sI + waterfallQuantum) $ do
                            VUM.write surf n (sI + waterfallQuantum)
                            push n
            loop = do
                q ← readSTRef queueRef
                case q of
                    [] → pure ()
                    (i:rest) → do
                        writeSTRef queueRef rest
                        sI ← VUM.read surf i
                        relaxNeighbour sI dirNorth i
                        relaxNeighbour sI dirEast  i
                        relaxNeighbour sI dirSouth i
                        relaxNeighbour sI dirWest  i
                        loop
        loop
        pure surf

-- | Per-tile width radius (0..maxWidthRadius), expanded perpendicular
--   to flow direction. Returns the widened river-tile mask, per-tile
--   width radius (-1 for non-river), and per-tile water surface z.
--
--   The surface z at a widened tile is inherited from the centre that
--   widened it. If multiple centres' wings overlap on a tile, the
--   minimum (= most-downstream) surface wins so the water plane
--   stays flat across the cross-section.
expandWidth
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Int        -- ^ flow
    → VU.Vector Bool       -- ^ isRiverCentre (after extension)
    → VU.Vector Int        -- ^ waterfall-clamped per-centre surface z
    → (VU.Vector Bool, VU.Vector Int, VU.Vector Int, VU.Vector Int)
expandWidth worldTiles terrain dir flow isRiverCentre centreSurf =
    let nTiles = worldTiles * worldTiles
        -- Don't widen into terrain that rises too far above the river's
        -- water surface. Caps the carve depth a wing tile can demand;
        -- without this, a river running past a cliff base would try to
        -- widen INTO the cliff and demand a 100+ z carve to keep the
        -- water plane flush. 'depthFromRadius r + bankSlack' gives the
        -- max pre-carve elevation a wing tile can have above the
        -- centre's surface.
        bankSlack = 3
    in runST $ do
        isR    ← VUM.replicate nTiles False
        width  ← VUM.replicate nTiles (-1   ∷ Int)
        surfZ  ← VUM.replicate nTiles minBound
        -- Perpendicular distance from the channel centreline (0 =
        -- centre tile, k = k-th wing tile out). Drives the thalweg
        -- cross-section in 'World.Fluid.River.Identify.BedDepth.computeBedDepth',
        -- min wins when a tile is claimed by several centres.
        perp   ← VUM.replicate nTiles (maxBound ∷ Int)
        let updateTile tIdx r s p = do
                VUM.write isR tIdx True
                curR ← VUM.read width tIdx
                when (r > curR) (VUM.write width tIdx r)
                curS ← VUM.read surfZ tIdx
                when (curS ≡ minBound ∨ s < curS) (VUM.write surfZ tIdx s)
                curP ← VUM.read perp tIdx
                when (p < curP) (VUM.write perp tIdx p)
            walkWing centreIdx r perpD = do
                centreS ← VUM.read surfZ centreIdx
                let maxBank = centreS + depthFromRadius r + bankSlack
                    -- Also cap downhill — a wing tile far below the
                    -- centre's surface would create a many-tile water
                    -- column trying to fill the natural depression
                    -- (e.g., a river running near a deep valley).
                    -- Limit to a modest drop so rivers don't pool
                    -- into adjacent low ground.
                    minBank = centreS - 6
                    loop k cur
                        | k > r = pure ()
                        | otherwise =
                            case stepDir worldTiles cur perpD of
                                Nothing → pure ()
                                Just nxt → do
                                    let nT = terrain VU.! nxt
                                    when (nT ≠ minBound
                                          ∧ nT ≤ maxBank
                                          ∧ nT ≥ minBank) $ do
                                        updateTile nxt r centreS k
                                        loop (k + 1) nxt
                loop 1 centreIdx
        forM_ [0 .. nTiles - 1] $ \i → when (isRiverCentre VU.! i) $ do
            let surf = centreSurf VU.! i
                r    = widthRadiusFromFlow (flow VU.! i)
            updateTile i r surf 0
            when (r > 0) $
                forM_ (perpDirs (dir VU.! i)) (walkWing i r)
        isRf   ← VU.unsafeFreeze isR
        widthF ← VU.unsafeFreeze width
        surfZf ← VU.unsafeFreeze surfZ
        perpF  ← VU.unsafeFreeze perp
        pure (isRf, widthF, surfZf, perpF)

-- | Carve depth in z for a given width radius. v2 user-approved:
--   narrow rivers (radius 0/1) carve 1 z; wider rivers carve deeper
--   so high-flow rivers cut visible canyons.
depthFromRadius ∷ Int → Int
depthFromRadius r
    | r ≥ 3     = 3
    | r ≥ 2     = 2
    | otherwise = 1

-- | Cardinal directions perpendicular to a given D4 direction. For
--   sinks ('dirNone'), expand in all four cardinal directions so a
--   ponding terminus still picks up width.
perpDirs ∷ Word8 → [Word8]
perpDirs d
    | d ≡ dirNorth ∨ d ≡ dirSouth = [dirEast,  dirWest]
    | d ≡ dirEast  ∨ d ≡ dirWest  = [dirNorth, dirSouth]
    | otherwise                   = [dirNorth, dirSouth, dirEast, dirWest]

-- | Cap on the number of rivers kept, scaled by world area. Tuned so
--   worldSize=32 → ~20 rivers, worldSize=64 → ~80, worldSize=128 → ~320.
--   Earlier counts at /200 and /100 were findable in dumps but the
--   user couldn't see them in the GUI: 1-tile-wide rivers are visually
--   subtle, so density needs to be high enough that a player crossing
--   the world reliably stumbles into one. Rivers are ranked by
--   per-component tile count so the kept chains are the longest
--   visible drainages — peak-flow ranking picked single-tile mouth
--   confluences instead, which render as isolated columns.
targetRiverCount ∷ Int → Int
targetRiverCount worldSize =
    max 5 (worldSize * worldSize `div` 50)

-- | Keep only the top @targetRiverCount worldSize@ river components
--   ranked by per-component tile count. Surviving components are
--   renumbered densely from 0; tiles in dropped components revert
--   to non-river.
--
--   Returns @(newIsRiverTile, newCompId, newNComps)@.
cullByLength
    ∷ Int                  -- ^ worldSize
    → VU.Vector Int        -- ^ raw compId (-1 = non-river)
    → Int                  -- ^ raw nComps
    → VU.Vector Bool       -- ^ raw isRiverTile
    → (VU.Vector Bool, VU.Vector Int, Int)
cullByLength worldSize rawCompId rawNComps rawIsRiverTile =
    let counts ∷ VU.Vector Int
        counts = VU.create $ do
            v ← VUM.replicate rawNComps 0
            VU.iforM_ rawCompId $ \_ cid →
                when (cid ≥ 0) (VUM.modify v (+ 1) cid)
            pure v
        keepN      = min rawNComps (targetRiverCount worldSize)
        ordered    = sortDescOn (counts VU.!) [0 .. rawNComps - 1]
        keptSet    = VU.create $ do
            v ← VUM.replicate rawNComps False
            forM_ (take keepN ordered) (\cid → VUM.write v cid True)
            pure v
        idMap ∷ VU.Vector Int
        idMap = runST $ do
            v ← VUM.replicate rawNComps (-1 ∷ Int)
            nextId ← newSTRef (0 ∷ Int)
            forM_ [0 .. rawNComps - 1] $ \oldId →
                when (keptSet VU.! oldId) $ do
                    newId ← readSTRef nextId
                    writeSTRef nextId (newId + 1)
                    VUM.write v oldId newId
            VU.unsafeFreeze v
        newNComps   = min rawNComps keepN
        newCompId   = VU.map (\oldId →
            if oldId < 0 then -1 else idMap VU.! oldId) rawCompId
        newIsRiver  = VU.zipWith (\rt cid → rt ∧ cid ≥ 0)
                                 rawIsRiverTile newCompId
    in (newIsRiver, newCompId, newNComps)

-- | BFS label connected components of the river-tile mask. Edges are
--   plain 4-adjacency over river tiles (E/W wrapped to mirror
--   'stepDir's torus).
--
--   The previous rule connected i ↔ j only when @dir[i]@ pointed at j
--   or @dir[j]@ at i. That detached width-expansion wing tiles whose
--   own steepest-descent dir points AWAY from their channel (downhill
--   -side wings especially — 'expandWidth' admits terrain down to
--   @centreS − 6@): they formed singleton or strip components, which
--   'cullByLength' then either stripped (rivers silently lost bank
--   tiles) or kept as bogus standalone "rivers" consuming
--   'targetRiverCount' slots. Adjacent river tiles are one connected
--   water body — label them together.
labelRiverComponents
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Bool       -- ^ isRiverTile
    → (VU.Vector Int, Int)
       -- ^ (per-tile component id, component count)
labelRiverComponents worldTiles isRiverTile =
    let nTiles = worldTiles * worldTiles
    in runST $ do
        ids ← VUM.replicate nTiles (-1 ∷ Int)
        nextId ← newSTRef (0 ∷ Int)
        forM_ [0 .. nTiles - 1] $ \start → do
            cur ← VUM.read ids start
            when (cur < 0 ∧ isRiverTile VU.! start) $ do
                lbl ← readSTRef nextId
                writeSTRef nextId (lbl + 1)
                VUM.write ids start lbl
                queue ← newSTRef [start]
                let loop = do
                        q ← readSTRef queue
                        case q of
                            []       → pure ()
                            (i : rs) → do
                                writeSTRef queue rs
                                let bx = i `mod` worldTiles
                                    by = i `div` worldTiles
                                    tryEdge ok nIdx = when ok $ do
                                        seen ← VUM.read ids nIdx
                                        when (seen < 0
                                              ∧ isRiverTile VU.! nIdx) $ do
                                            VUM.write ids nIdx lbl
                                            modifySTRef' queue (nIdx :)
                                -- E/W wrap to mirror 'stepDir's torus.
                                let west  = if bx > 0
                                            then i - 1
                                            else i - 1 + worldTiles
                                    east  = if bx < worldTiles - 1
                                            then i + 1
                                            else i + 1 - worldTiles
                                tryEdge True west
                                tryEdge True east
                                tryEdge (by > 0)              (i - worldTiles)
                                tryEdge (by < worldTiles - 1) (i + worldTiles)
                                loop
                loop
        n     ← readSTRef nextId
        idsF  ← VU.unsafeFreeze ids
        pure (idsF, n)

-- | Build the 'River' vector. For each component compute bbox, peak
--   flow, source-lake (if its start tile is a spillway), sink-lake
--   (if a downstream tile of the chain enters a lake).
buildRivers
    ∷ Int                  -- ^ worldSize
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → VU.Vector Int        -- ^ isSpillwayOf
    → VU.Vector Int        -- ^ spillwayOf
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Int        -- ^ flow
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Int        -- ^ compId
    → Int                  -- ^ nComps
    → V.Vector River
buildRivers worldSize _terrain lakeIdAt isSpillwayOf _spillwayOf dir flow
            isRiverTile compId nComps =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        half       = worldTiles `div` 2
    in runST $ do
        bbMinX  ← VUM.replicate nComps (maxBound ∷ Int)
        bbMinY  ← VUM.replicate nComps (maxBound ∷ Int)
        bbMaxX  ← VUM.replicate nComps (minBound ∷ Int)
        bbMaxY  ← VUM.replicate nComps (minBound ∷ Int)
        peak    ← VUM.replicate nComps (0        ∷ Int)
        srcLake ← VUM.replicate nComps (-1       ∷ Int)
        snkLake ← VUM.replicate nComps (-1       ∷ Int)
        forM_ [0 .. nTiles - 1] $ \i → when (isRiverTile VU.! i) $ do
            let cid = compId VU.! i
                gx  = (i `mod` worldTiles) - half
                gy  = (i `div` worldTiles) - half
                fl  = flow VU.! i
            VUM.modify bbMinX (min gx) cid
            VUM.modify bbMinY (min gy) cid
            VUM.modify bbMaxX (max gx) cid
            VUM.modify bbMaxY (max gy) cid
            VUM.modify peak   (max fl) cid
            -- Source lake: this tile is a spillway → record.
            let sp = isSpillwayOf VU.! i
            when (sp ≥ 0) (VUM.write srcLake cid sp)
            -- Sink lake: this tile's D4 step lands in a lake.
            case stepDir worldTiles i (dir VU.! i) of
                Just dn → do
                    let nlk = lakeIdAt VU.! dn
                    when (nlk ≥ 0) (VUM.write snkLake cid nlk)
                Nothing → pure ()
        bMinXF ← VU.unsafeFreeze bbMinX
        bMinYF ← VU.unsafeFreeze bbMinY
        bMaxXF ← VU.unsafeFreeze bbMaxX
        bMaxYF ← VU.unsafeFreeze bbMaxY
        peakF  ← VU.unsafeFreeze peak
        srcF   ← VU.unsafeFreeze srcLake
        snkF   ← VU.unsafeFreeze snkLake
        pure $ V.generate nComps $ \cid →
            River
                { rivFlowRate   = peakF VU.! cid
                , rivSourceLake = let s = srcF VU.! cid
                                  in if s ≥ 0 then Just s else Nothing
                , rivSinkLake   = let s = snkF VU.! cid
                                  in if s ≥ 0 then Just s else Nothing
                , rivBBoxMinX   = bMinXF VU.! cid
                , rivBBoxMinY   = bMinYF VU.! cid
                , rivBBoxMaxX   = bMaxXF VU.! cid
                , rivBBoxMaxY   = bMaxYF VU.! cid
                }
