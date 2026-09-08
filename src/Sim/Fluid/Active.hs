{-# LANGUAGE Strict #-}
module Sim.Fluid.Active
    ( simulateActiveTick
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Chunk.Types (ChunkCoord, chunkSize)
import World.Fluid.Types (FluidCell(..))
import World.SideFace.Base (SideDecoType(..), sideDecoBase)
import Sim.State.Types (SimWorldState(..), SimChunkState(..))
import Sim.Fluid.Types (ActiveFluidCell(..), volumePerLevel, volumeToSurface)
import Sim.Fluid.Reaction
    (CellSite(..), SolidificationEvent, TransferOutcome(..)
    , applyTransfer, dedupeEvents)
import Sim.Topology (SimTopology, simSeamNeighbor)

-- | Ticks at equilibrium before a chunk is deactivated.
equilThreshold ∷ Int
equilThreshold = 200

-- | Run one tick of volume-conserving simulation for all active chunks
--   of ONE world. The engine-level pause guard is the caller's job.
--
--   Unlike-fluid contact (#2481) is resolved by every transfer branch
--   below through 'Sim.Fluid.Reaction.applyTransfer', which also bounds
--   ordinary transfers by the LIVE source volume and the destination's
--   remaining 'Word16' capacity. A tick's solidification events are
--   deduplicated to at most one per canonical coordinate and appended to
--   this world's 'swsSolidEvents'; nothing here drains that collection.
simulateActiveTick ∷ SimWorldState → SimWorldState
simulateActiveTick sws =
        let chunks = swsChunks sws
            activeChunks = HM.filter scsActive chunks
        in if HM.null activeChunks
           -- An inactive world still carries whatever it has already
           -- emitted: the event history is output, not scratch.
           then sws
           else let ticked = HM.mapWithKey (simulateActiveChunk chunks) activeChunks
                    phased = HM.map (\(scs, ch, _) → (scs, ch)) ticked
                    -- Chunk-key order, so a tick's events (and therefore
                    -- the dedupe below) do not depend on hash iteration.
                    chunkEvents = concatMap (\(_, _, evs) → evs)
                        (map snd (sortOn fst (HM.toList ticked)))
                    (results, seamEvents) = reconcileSeams (swsTopology sws) phased
                    dirty = HM.foldlWithKey' (\acc cc (_, changed) →
                        if changed then HS.insert cc acc else acc
                        ) (swsDirtyChunks sws) results
                    -- Merge updated active chunks back, handle deactivation
                    newChunks = HM.foldlWithKey' (\acc cc (scs, changed) →
                        let scs' = if changed
                                   then scs { scsEquilTicks = 0 }
                                   else scs { scsEquilTicks = scsEquilTicks scs + 1 }
                            -- Deactivate if at equilibrium long enough
                            scs'' = if scsEquilTicks scs' ≥ equilThreshold
                                    then deactivateInPlace scs'
                                    else scs'
                        in HM.insert cc scs'' acc
                        ) chunks results
                    -- At most ONE event per canonical coordinate per tick.
                    -- A deactivating tick bakes its grid to passive fluid,
                    -- but the events it already produced are kept.
                    fresh = dedupeEvents (chunkEvents <> seamEvents)
                in sws { swsChunks = newChunks
                       , swsDirtyChunks = dirty
                       , swsSolidEvents =
                           swsSolidEvents sws <> Seq.fromList fresh
                       }

-- | Deactivate a chunk: bake active volumes back to passive fluid.
deactivateInPlace ∷ SimChunkState → SimChunkState
deactivateInPlace scs =
    let terrV = scsTerrain scs
        bakedFluid = V.imap (\idx mafc →
            case mafc of
                Nothing  → Nothing
                Just afc → case volumeToSurface (terrV VU.! idx) (afcVolume afc) of
                    s | afcVolume afc ≡ 0 → Nothing
                      | otherwise → Just (FluidCell (afcType afc) s)
            ) (scsActiveFluid scs)
        sz = V.length bakedFluid
    in scs { scsActive      = False
           , scsActiveFluid = V.replicate sz Nothing
           , scsFluid       = bakedFluid
           , scsEquilTicks  = 0
           }

-- | One side of a contact inside a single chunk's grid.
siteIn ∷ ChunkCoord → VU.Vector Int → Int → CellSite
siteIn cc terrainV idx = CellSite { csChunk   = cc
                                  , csIndex   = idx
                                  , csTerrain = terrainV VU.! idx
                                  }
{-# INLINE siteIn #-}

-- | Simulate one tick for a single active chunk. The chunk's own stored
--   key names any solidification event its phases emit (#2481).
simulateActiveChunk ∷ HM.HashMap ChunkCoord SimChunkState
                    → ChunkCoord → SimChunkState
                    → (SimChunkState, Bool, [SolidificationEvent])
simulateActiveChunk _allChunks coord scs =
    let terrainV = scsTerrain scs
        (newActive, newDeco, changed, events) = runST $ do
            mv ← V.thaw (scsActiveFluid scs)
            decoMv ← VU.thaw (scsSideDeco scs)
            changedRef ← newSTRef False
            eventsRef ← newSTRef ([] ∷ [SolidificationEvent])

            -- Phase A: Gravity (downhill flow)
            phaseGravity coord mv terrainV changedRef eventsRef

            -- Phase B: Lateral pressure equalization
            phaseLateral coord mv terrainV changedRef eventsRef

            -- Phase C: Waterfall detection + downward transfer
            phaseWaterfall coord mv decoMv terrainV changedRef eventsRef

            -- Phase D: Dry-out (remove zero-volume cells)
            phaseDryOut mv changedRef

            result ← V.freeze mv
            decoResult ← VU.freeze decoMv
            ch ← readSTRef changedRef
            evs ← readSTRef eventsRef
            pure (result, decoResult, ch, reverse evs)

        -- Also derive passive FluidMap for writeback
        newFluid = deriveFluidMap terrainV newActive

    in (scs { scsActiveFluid = newActive
            , scsFluid       = newFluid
            , scsSideDeco    = newDeco
            }, changed, events)

-- | Record one applied request's outcome: any event it produced, and
--   whether it changed the grid at all (a reaction counts).
noteOutcome ∷ STRef s Bool → STRef s [SolidificationEvent] → TransferOutcome
            → ST s ()
noteOutcome changedRef eventsRef outcome = do
    when (toMoved outcome > 0 ∨ toConsumed outcome > 0) $
        writeSTRef changedRef True
    case toEvent outcome of
        Nothing → pure ()
        Just ev → modifySTRef' eventsRef (ev :)
{-# INLINE noteOutcome #-}

-- | Derive the passive fluid map (surfaces) from an active-volume grid.
deriveFluidMap ∷ VU.Vector Int → V.Vector (Maybe ActiveFluidCell)
               → V.Vector (Maybe FluidCell)
deriveFluidMap terrainV active =
    V.imap (\idx mafc →
        case mafc of
            Nothing  → Nothing
            Just afc | afcVolume afc ≡ 0 → Nothing
                     | otherwise →
                Just (FluidCell (afcType afc)
                        (volumeToSurface (terrainV VU.! idx) (afcVolume afc)))
        ) active

-- * Seam exchange: cross-chunk fluid transfer
--
-- The per-chunk phases above only move fluid WITHIN a chunk — an edge
-- cell has no in-chunk neighbour past the boundary, so dammed water
-- piles into a 1-tile lip at chunk seams. This pass transfers fluid
-- across the shared edge of every pair of adjacent ACTIVE chunks.

-- | Net seam flow from cell A to cell B (negative = B → A), using the
--   SAME rules as the in-chunk phases so seam physics matches the
--   interior: lateral volume-equalisation at equal terrain (with the
--   >1 guard that stops 1-unit oscillation), gravity by surface drop at
--   unequal terrain. Dry (Nothing) cells count as volume 0 at terrain.
seamFlow ∷ Int → Maybe ActiveFluidCell → Int → Maybe ActiveFluidCell → Int
seamFlow terrA mca terrB mcb =
    let volA = maybe 0 (fromIntegral . afcVolume) mca ∷ Int
        volB = maybe 0 (fromIntegral . afcVolume) mcb ∷ Int
    in if terrA ≡ terrB
       then let diff = volA - volB                       -- lateral (phaseLateral)
            in if diff > 1 ∧ volA > 0      then max 1 (diff `div` 4)
               else if diff < (-1) ∧ volB > 0 then negate (max 1 ((negate diff) `div` 4))
               else 0
       else if terrB < terrA ∧ volA > 0                  -- gravity A → lower B
       then let surfDiff = volumeToSurface terrA (fromIntegral volA)
                         - volumeToSurface terrB (fromIntegral volB)
            in if surfDiff > 0
               then max 1 (min volA (surfDiff * volumePerLevel `div` 4)) else 0
       else if terrA < terrB ∧ volB > 0                  -- gravity B → lower A
       then let surfDiff = volumeToSurface terrB (fromIntegral volB)
                         - volumeToSurface terrA (fromIntegral volA)
            in if surfDiff > 0
               then negate (max 1 (min volB (surfDiff * volumePerLevel `div` 4))) else 0
       else 0

-- | Apply a signed seam flow (positive = A → B) live through the shared
--   applier, so an unlike pair across the seam reacts exactly like an
--   unlike pair inside a chunk. Bounded by the source's CURRENT volume,
--   so two seams meeting at a corner cell can't over-drain it.
moveSeam ∷ MV.MVector s (Maybe ActiveFluidCell) → CellSite
         → MV.MVector s (Maybe ActiveFluidCell) → CellSite → Int
         → ST s TransferOutcome
moveSeam mA siteA mB siteB flow
    | flow > 0  = applyTransfer mA siteA mB siteB flow
    | flow < 0  = applyTransfer mB siteB mA siteA (negate flow)
    | otherwise = pure (TransferOutcome 0 0 Nothing)

-- | (selfIdx, neighbourIdx) along the +X seam: this chunk's right column
--   (lx = chunkSize-1) facing the East neighbour's left column (lx = 0).
eastEdgePairs ∷ [(Int, Int)]
eastEdgePairs =
    [ (ly * chunkSize + (chunkSize - 1), ly * chunkSize) | ly ← [0 .. chunkSize - 1] ]

-- | (selfIdx, neighbourIdx) along the +Y seam: this chunk's bottom row
--   (ly = chunkSize-1) facing the South neighbour's top row (ly = 0).
southEdgePairs ∷ [(Int, Int)]
southEdgePairs =
    [ ((chunkSize - 1) * chunkSize + lx, lx) | lx ← [0 .. chunkSize - 1] ]

-- | Exchange fluid across the seams of adjacent active chunks. Each
--   shared edge is processed once (via the +X / +Y neighbour), live in
--   ST so corner cells stay conserved; both chunks in any transfer are
--   re-derived and flagged changed (so they stay active + re-render).
--
--   The neighbour is the physically adjacent chunk's STORED key, not the
--   raw @(cx+1, cy)@ / @(cx, cy+1)@ one: on a cylindrical page the coord
--   across the u seam has both components changed, so the raw lookup
--   missed and fluid piled up against an artificial wall (#2044). The
--   canonicalisation is a bijection on stored keys, so every shared edge
--   still belongs to exactly one @(chunk, direction)@ probe and is still
--   processed exactly once. Identity on a flat page and away from the
--   seam.
--
--   Those stored keys are also what a seam solidification event is named
--   by (#2481): the event belongs to the exhausted LAVA cell, whichever
--   side of the seam it is on, under the key the sim already holds that
--   chunk under.
reconcileSeams ∷ SimTopology
               → HM.HashMap ChunkCoord (SimChunkState, Bool)
               → ( HM.HashMap ChunkCoord (SimChunkState, Bool)
                 , [SolidificationEvent] )
reconcileSeams topo results
    | HM.size results < 2 = (results, [])
    | otherwise =
        let (grids', touched, events) = runST $ do
                mgrids ← traverse (\(scs, _) → V.thaw (scsActiveFluid scs)) results
                touchedRef ← newSTRef HS.empty
                eventsRef ← newSTRef ([] ∷ [SolidificationEvent])
                -- Chunk-key order: the seam pass mutates live grids, so
                -- its outcome must not depend on hash iteration order.
                forM_ (sortOn fst (HM.toList results)) $ \(coord, (scsA, _)) →
                    forM_ [ (simSeamNeighbor topo 1 0 coord, eastEdgePairs)
                          , (simSeamNeighbor topo 0 1 coord, southEdgePairs) ] $ \(nbr, pairs) →
                        case HM.lookup nbr results of
                            Nothing → pure ()
                            Just (scsB, _) → do
                                let mA = mgrids HM.! coord
                                    mB = mgrids HM.! nbr
                                    terrA = scsTerrain scsA
                                    terrB = scsTerrain scsB
                                anyRef ← newSTRef False
                                forM_ pairs $ \(ia, ib) → do
                                    ca ← MV.read mA ia
                                    cb ← MV.read mB ib
                                    let flow = seamFlow (terrA VU.! ia) ca
                                                        (terrB VU.! ib) cb
                                    when (flow ≢ 0) $ do
                                        outcome ← moveSeam mA (siteIn coord terrA ia)
                                                           mB (siteIn nbr terrB ib) flow
                                        when (toMoved outcome > 0
                                              ∨ toConsumed outcome > 0) $
                                            writeSTRef anyRef True
                                        case toEvent outcome of
                                            Nothing → pure ()
                                            Just ev → modifySTRef' eventsRef (ev :)
                                didMove ← readSTRef anyRef
                                when didMove $ modifySTRef' touchedRef
                                    (HS.insert coord . HS.insert nbr)
                frozen ← traverse V.freeze mgrids
                t ← readSTRef touchedRef
                evs ← readSTRef eventsRef
                pure (frozen, t, reverse evs)
        in ( HM.mapWithKey (\coord (scs, changed) →
                if HS.member coord touched
                then let active' = grids' HM.! coord
                     in (scs { scsActiveFluid = active'
                             , scsFluid = deriveFluidMap (scsTerrain scs) active' }, True)
                else (scs, changed)
                ) results
           , events )

-- * Phase A: Gravity — downhill flow

phaseGravity ∷ ChunkCoord
             → MV.MVector s (Maybe ActiveFluidCell)
             → VU.Vector Int
             → STRef s Bool
             → STRef s [SolidificationEvent]
             → ST s ()
phaseGravity coord mv terrainV changedRef eventsRef = do
    snap ← V.freeze mv
    let sz = chunkSize * chunkSize
    forM_ [0 .. sz - 1] $ \idx → do
        let cell = snap V.! idx
        case cell of
            Nothing → pure ()
            Just afc | afcVolume afc ≡ 0 → pure ()
            Just afc → do
                let terrZ = terrainV VU.! idx
                    lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    nbrs = cardinalNeighbors lx ly
                    transfers = mapMaybe (\(nx, ny) →
                        if nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize
                        then let nIdx = ny * chunkSize + nx
                                 nTerrZ = terrainV VU.! nIdx
                             in if nTerrZ < terrZ
                                then let srcSurf = volumeToSurface terrZ (afcVolume afc)
                                         nbrSurf = case snap V.! nIdx of
                                             Nothing  → nTerrZ
                                             Just nfc → volumeToSurface nTerrZ (afcVolume nfc)
                                         surfDiff = srcSurf - nbrSurf
                                     in if surfDiff > 0
                                        then let outflow = min (fromIntegral (afcVolume afc))
                                                               (surfDiff * volumePerLevel `div` 4)
                                             in Just (nIdx, max 1 outflow)
                                        else Nothing
                                else Nothing
                        else Nothing
                        ) nbrs
                    totalRequested = sum (map snd transfers)
                    avail = fromIntegral (afcVolume afc)
                    scale = if totalRequested > avail ∧ totalRequested > 0
                            then (avail ∷ Int) * 256 `div` totalRequested
                            else 256
                when (not (null transfers) ∧ avail > 0) $ do
                    totalRef ← newSTRef (0 ∷ Int)
                    forM_ transfers $ \(nIdx, amt) → do
                        soFar ← readSTRef totalRef
                        let scaled = if scale < 256
                                     then max 1 (amt * scale `div` 256)
                                     else amt
                            actual = min scaled (avail - soFar)
                        when (actual > 0) $ do
                            -- Planned from the snapshot, paid from the LIVE
                            -- cells: a reaction can consume more than this
                            -- request, so the applier re-reads both sides
                            -- and this loop advances only by what moved.
                            outcome ← applyTransfer
                                mv (siteIn coord terrainV idx)
                                mv (siteIn coord terrainV nIdx)
                                actual
                            writeSTRef totalRef (soFar + toMoved outcome)
                            noteOutcome changedRef eventsRef outcome

-- * Phase B: Lateral pressure equalization

phaseLateral ∷ ChunkCoord
             → MV.MVector s (Maybe ActiveFluidCell)
             → VU.Vector Int
             → STRef s Bool
             → STRef s [SolidificationEvent]
             → ST s ()
phaseLateral coord mv terrainV changedRef eventsRef = do
    snap ← V.freeze mv
    let sz = chunkSize * chunkSize
    forM_ [0 .. sz - 1] $ \idx → do
        let cell = snap V.! idx
        case cell of
            Nothing → pure ()
            Just afc | afcVolume afc ≡ 0 → pure ()
            Just afc → do
                let terrZ = terrainV VU.! idx
                    srcVol = fromIntegral (afcVolume afc) ∷ Int
                    lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    nbrs = cardinalNeighbors lx ly
                -- Every request below is sized from the FROZEN snapshot but
                -- paid out of the LIVE grid, so the cumulative spend has to
                -- be tracked and each payment capped by what the source has
                -- left — the same shape 'phaseGravity' and 'phaseWaterfall'
                -- already use. Without the cap a low-volume cell with
                -- several thirstier neighbours pays out more than it holds
                -- and its 'Word16' volume wraps to ~65535, manufacturing
                -- fluid (#2042). The rate ('diff div 4') and the
                -- minimum-one-unit progress rule are untouched for every
                -- transfer the source can actually afford. Since #2481 the
                -- live cap is enforced inside 'applyTransfer' as well, which
                -- is what makes it hold when an unlike-fluid reaction — not
                -- this loop's own spending — is what emptied the source.
                spentRef ← newSTRef (0 ∷ Int)
                forM_ nbrs $ \(nx, ny) →
                    when (nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize) $ do
                        let nIdx = ny * chunkSize + nx
                            nTerrZ = terrainV VU.! nIdx
                        when (nTerrZ ≡ terrZ) $ do
                            let nbrCell = snap V.! nIdx
                            case nbrCell of
                                Just nfc → do
                                    let dstVol = fromIntegral (afcVolume nfc) ∷ Int
                                        diff   = srcVol - dstVol
                                    -- Only transfer from higher side to avoid double-counting
                                    when (diff > 1) $ do
                                        spent ← readSTRef spentRef
                                        let transfer = min (max 1 (diff `div` 4))
                                                           (srcVol - spent)
                                        when (transfer > 0) $ do
                                            outcome ← applyTransfer
                                                mv (siteIn coord terrainV idx)
                                                mv (siteIn coord terrainV nIdx)
                                                transfer
                                            writeSTRef spentRef
                                                (spent + toMoved outcome)
                                            noteOutcome changedRef eventsRef outcome
                                Nothing | srcVol > volumePerLevel → do
                                    spent ← readSTRef spentRef
                                    let transfer = min (max 1 (srcVol `div` 4))
                                                       (srcVol - spent)
                                    when (transfer > 0) $ do
                                        -- The destination was empty in the
                                        -- SNAPSHOT, but an earlier source this
                                        -- same phase may already have spilled
                                        -- into it — possibly with an unlike
                                        -- fluid. 'applyTransfer' reads the LIVE
                                        -- cell, so this branch reacts, adds, or
                                        -- creates exactly as the live state
                                        -- warrants instead of overwriting what
                                        -- is there (#2042, #2481).
                                        outcome ← applyTransfer
                                            mv (siteIn coord terrainV idx)
                                            mv (siteIn coord terrainV nIdx)
                                            transfer
                                        writeSTRef spentRef
                                            (spent + toMoved outcome)
                                        noteOutcome changedRef eventsRef outcome
                                _ → pure ()

-- * Phase C: Waterfall detection

phaseWaterfall ∷ ChunkCoord
               → MV.MVector s (Maybe ActiveFluidCell)
               → MVU.MVector s Word8
               → VU.Vector Int
               → STRef s Bool
               → STRef s [SolidificationEvent]
               → ST s ()
phaseWaterfall coord mv decoMv terrainV changedRef eventsRef = do
    snap ← V.freeze mv
    let sz = chunkSize * chunkSize
    forM_ [0 .. sz - 1] $ \idx → do
        let cell = snap V.! idx
        case cell of
            Nothing → pure ()
            Just afc | afcVolume afc ≡ 0 → pure ()
            Just afc → do
                let terrZ = terrainV VU.! idx
                    lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    nbrs = cardinalNeighbors lx ly
                    avail = fromIntegral (afcVolume afc) ∷ Int
                    falls = mapMaybe (\(dirBit, (nx, ny)) →
                        if nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize
                        then let nIdx = ny * chunkSize + nx
                                 nTerrZ = terrainV VU.! nIdx
                                 drop' = terrZ - nTerrZ
                             in if drop' > 1
                                then Just (nIdx, dirBit, min avail (volumePerLevel `div` 2))
                                else Nothing
                        else Nothing
                        ) (zip [0∷Int ..] nbrs)
                    totalRequested = sum (map (\(_, _, t) → t) falls)
                    scale = if totalRequested > avail ∧ totalRequested > 0
                            then avail * 256 `div` totalRequested
                            else 256
                flowDirRef ← newSTRef (afcFlowDir afc)
                when (not (null falls) ∧ avail > 0) $ do
                    totalRef ← newSTRef (0 ∷ Int)
                    forM_ falls $ \(nIdx, dirBit, amt) → do
                        soFar ← readSTRef totalRef
                        let scaled = if scale < 256
                                     then max 1 (amt * scale `div` 256)
                                     else amt
                            actual = min scaled (avail - soFar)
                        when (actual > 0) $ do
                            outcome ← applyTransfer
                                mv (siteIn coord terrainV idx)
                                mv (siteIn coord terrainV nIdx)
                                actual
                            writeSTRef totalRef (soFar + toMoved outcome)
                            noteOutcome changedRef eventsRef outcome
                            -- A reaction moves no fluid, so nothing fell
                            -- here: the waterfall marker and flow direction
                            -- describe transfers only.
                            when (toMoved outcome > 0) $ do
                                fd ← readSTRef flowDirRef
                                writeSTRef flowDirRef
                                    (fd ⌄ ((1 ∷ Word8) `shiftL` dirBit))
                                MVU.write decoMv idx
                                    (sideDecoBase DecoWaterfall
                                        + fromIntegral (dirBit `mod` 4))
                newFD ← readSTRef flowDirRef
                when (newFD ≢ afcFlowDir afc) $ do
                    cur ← MV.read mv idx
                    case cur of
                        Just c → MV.write mv idx (Just c { afcFlowDir = newFD })
                        Nothing → pure ()

-- * Phase D: Dry-out

phaseDryOut ∷ MV.MVector s (Maybe ActiveFluidCell)
            → STRef s Bool
            → ST s ()
phaseDryOut mv changedRef = do
    let sz = chunkSize * chunkSize
    forM_ [0 .. sz - 1] $ \idx → do
        cell ← MV.read mv idx
        case cell of
            Just afc | afcVolume afc ≡ 0 → do
                MV.write mv idx Nothing
                writeSTRef changedRef True
            _ → pure ()

cardinalNeighbors ∷ Int → Int → [(Int, Int)]
cardinalNeighbors lx ly =
    [(lx, ly - 1), (lx + 1, ly), (lx, ly + 1), (lx - 1, ly)]
{-# INLINE cardinalNeighbors #-}
