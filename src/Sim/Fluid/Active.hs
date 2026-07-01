{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.Fluid.Active
    ( simulateActiveTick
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Maybe (mapMaybe)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidCell(..))
import Sim.State.Types (SimWorldState(..), SimChunkState(..))
import Sim.Fluid.Types (ActiveFluidCell(..), volumePerLevel, volumeToSurface)

-- | Ticks at equilibrium before a chunk is deactivated.
equilThreshold ∷ Int
equilThreshold = 200

-- | Run one tick of volume-conserving simulation for all active chunks
--   of ONE world. The engine-level pause guard is the caller's job.
simulateActiveTick ∷ SimWorldState → SimWorldState
simulateActiveTick sws =
        let chunks = swsChunks sws
            activeChunks = HM.filter scsActive chunks
        in if HM.null activeChunks
           then sws
           else let results = reconcileSeams
                                 (HM.mapWithKey (simulateActiveChunk chunks) activeChunks)
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
                in sws { swsChunks = newChunks
                       , swsDirtyChunks = dirty
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
           , scsGenFluid    = bakedFluid
           , scsEquilTicks  = 0
           }

-- | Simulate one tick for a single active chunk.
simulateActiveChunk ∷ HM.HashMap ChunkCoord SimChunkState
                    → ChunkCoord → SimChunkState → (SimChunkState, Bool)
simulateActiveChunk _allChunks _coord scs =
    let terrainV = scsTerrain scs
        _sz = chunkSize * chunkSize
        (newActive, newDeco, changed) = runST $ do
            mv ← V.thaw (scsActiveFluid scs)
            decoMv ← VU.thaw (scsSideDeco scs)
            changedRef ← newSTRef False

            -- Phase A: Gravity (downhill flow)
            phaseGravity mv terrainV changedRef

            -- Phase B: Lateral pressure equalization
            phaseLateral mv terrainV changedRef

            -- Phase C: Waterfall detection + downward transfer
            phaseWaterfall mv decoMv terrainV changedRef

            -- Phase D: Dry-out (remove zero-volume cells)
            phaseDryOut mv changedRef

            result ← V.freeze mv
            decoResult ← VU.freeze decoMv
            ch ← readSTRef changedRef
            pure (result, decoResult, ch)

        -- Also derive passive FluidMap for writeback
        newFluid = deriveFluidMap terrainV newActive

    in (scs { scsActiveFluid = newActive
            , scsFluid       = newFluid
            , scsSideDeco    = newDeco
            }, changed)

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

-- | Apply a signed seam flow (positive = mA[ia] → mB[ib]) live, bounded
--   by the source's CURRENT volume — so two seams meeting at a corner
--   cell can't over-drain it. Volume-conserving.
moveSeam ∷ MV.MVector s (Maybe ActiveFluidCell) → Int
         → MV.MVector s (Maybe ActiveFluidCell) → Int → Int → ST s ()
moveSeam mA ia mB ib flow
    | flow > 0  = transferCell mA ia mB ib flow
    | flow < 0  = transferCell mB ib mA ia (negate flow)
    | otherwise = pure ()

transferCell ∷ MV.MVector s (Maybe ActiveFluidCell) → Int
             → MV.MVector s (Maybe ActiveFluidCell) → Int → Int → ST s ()
transferCell mSrc iSrc mDst iDst amt = do
    msrc ← MV.read mSrc iSrc
    case msrc of
        Nothing → pure ()
        Just s → do
            let actual = min amt (fromIntegral (afcVolume s)) ∷ Int
            when (actual > 0) $ do
                MV.write mSrc iSrc (Just s
                    { afcVolume = afcVolume s - fromIntegral actual })
                mdst ← MV.read mDst iDst
                case mdst of
                    Nothing → MV.write mDst iDst (Just ActiveFluidCell
                        { afcType = afcType s
                        , afcVolume = fromIntegral actual
                        , afcFlowDir = 0 })
                    Just d → MV.write mDst iDst (Just d
                        { afcVolume = afcVolume d + fromIntegral actual })

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
reconcileSeams ∷ HM.HashMap ChunkCoord (SimChunkState, Bool)
               → HM.HashMap ChunkCoord (SimChunkState, Bool)
reconcileSeams results
    | HM.size results < 2 = results
    | otherwise =
        let (grids', touched) = runST $ do
                mgrids ← traverse (\(scs, _) → V.thaw (scsActiveFluid scs)) results
                touchedRef ← newSTRef HS.empty
                forM_ (HM.toList results) $ \(coord, (scsA, _)) → do
                    let ChunkCoord cx cy = coord
                    forM_ [ (ChunkCoord (cx + 1) cy, eastEdgePairs)
                          , (ChunkCoord cx (cy + 1), southEdgePairs) ] $ \(nbr, pairs) →
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
                                    when (flow ≠ 0) $ do
                                        moveSeam mA ia mB ib flow
                                        writeSTRef anyRef True
                                didMove ← readSTRef anyRef
                                when didMove $ modifySTRef' touchedRef
                                    (HS.insert coord . HS.insert nbr)
                frozen ← traverse V.freeze mgrids
                t ← readSTRef touchedRef
                pure (frozen, t)
        in HM.mapWithKey (\coord (scs, changed) →
            if HS.member coord touched
            then let active' = grids' HM.! coord
                 in (scs { scsActiveFluid = active'
                         , scsFluid = deriveFluidMap (scsTerrain scs) active' }, True)
            else (scs, changed)
            ) results

-- * Phase A: Gravity — downhill flow

phaseGravity ∷ MV.MVector s (Maybe ActiveFluidCell)
             → VU.Vector Int
             → STRef s Bool
             → ST s ()
phaseGravity mv terrainV changedRef = do
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
                            writeSTRef totalRef (soFar + actual)
                            srcCell ← MV.read mv idx
                            case srcCell of
                                Just src → MV.write mv idx
                                    (Just src { afcVolume = afcVolume src - fromIntegral actual })
                                Nothing → pure ()
                            dst ← MV.read mv nIdx
                            case dst of
                                Nothing →
                                    MV.write mv nIdx (Just ActiveFluidCell
                                        { afcType = afcType afc
                                        , afcVolume = fromIntegral actual
                                        , afcFlowDir = 0
                                        })
                                Just d →
                                    MV.write mv nIdx (Just d
                                        { afcVolume = afcVolume d + fromIntegral actual })
                            writeSTRef changedRef True

-- * Phase B: Lateral pressure equalization

phaseLateral ∷ MV.MVector s (Maybe ActiveFluidCell)
             → VU.Vector Int
             → STRef s Bool
             → ST s ()
phaseLateral mv terrainV changedRef = do
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
                                        let transfer = max 1 (diff `div` 4)
                                        curSrc ← MV.read mv idx
                                        curDst ← MV.read mv nIdx
                                        case (curSrc, curDst) of
                                            (Just s, Just d) → do
                                                MV.write mv idx (Just s
                                                    { afcVolume = afcVolume s - fromIntegral transfer })
                                                MV.write mv nIdx (Just d
                                                    { afcVolume = afcVolume d + fromIntegral transfer })
                                                writeSTRef changedRef True
                                            _ → pure ()
                                Nothing | srcVol > volumePerLevel → do
                                    let transfer = max 1 (srcVol `div` 4)
                                    curSrc ← MV.read mv idx
                                    case curSrc of
                                        Just s → do
                                            MV.write mv idx (Just s
                                                { afcVolume = afcVolume s - fromIntegral transfer })
                                            MV.write mv nIdx (Just ActiveFluidCell
                                                { afcType = afcType afc
                                                , afcVolume = fromIntegral transfer
                                                , afcFlowDir = 0
                                                })
                                            writeSTRef changedRef True
                                        Nothing → pure ()
                                _ → pure ()

-- * Phase C: Waterfall detection

phaseWaterfall ∷ MV.MVector s (Maybe ActiveFluidCell)
               → MVU.MVector s Word8
               → VU.Vector Int
               → STRef s Bool
               → ST s ()
phaseWaterfall mv decoMv terrainV changedRef = do
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
                            writeSTRef totalRef (soFar + actual)
                            srcCell ← MV.read mv idx
                            case srcCell of
                                Just src → MV.write mv idx
                                    (Just src { afcVolume = afcVolume src - fromIntegral actual })
                                Nothing → pure ()
                            dst ← MV.read mv nIdx
                            case dst of
                                Nothing →
                                    MV.write mv nIdx (Just ActiveFluidCell
                                        { afcType = afcType afc
                                        , afcVolume = fromIntegral actual
                                        , afcFlowDir = 0
                                        })
                                Just d →
                                    MV.write mv nIdx (Just d
                                        { afcVolume = afcVolume d + fromIntegral actual })
                            fd ← readSTRef flowDirRef
                            writeSTRef flowDirRef (fd .|. ((1 ∷ Word8) `shiftL` dirBit))
                            MVU.write decoMv idx (17 + fromIntegral (dirBit `mod` 4))
                            writeSTRef changedRef True
                newFD ← readSTRef flowDirRef
                when (newFD ≠ afcFlowDir afc) $ do
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

