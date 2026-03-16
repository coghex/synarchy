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
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Bits ((.&.), (.|.), shiftL)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidType(..), FluidCell(..))
import Sim.State.Types (SimState(..), SimChunkState(..))
import Sim.Fluid.Types (ActiveFluidCell(..), volumePerLevel, volumeToSurface)

-- | Equilibrium tick threshold for deactivation.
equilThreshold ∷ Int
equilThreshold = 200

-- | Run one tick of volume-conserving simulation for all active chunks.
simulateActiveTick ∷ SimState → SimState
simulateActiveTick ss
    | ssPaused ss = ss
    | otherwise =
        let chunks = ssChunks ss
            activeChunks = HM.filter scsActive chunks
        in if HM.null activeChunks
           then ss
           else let results = HM.mapWithKey (simulateActiveChunk chunks) activeChunks
                    dirty = HM.foldlWithKey' (\acc cc (_, changed) →
                        if changed then HS.insert cc acc else acc
                        ) (ssDirtyChunks ss) results
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
                in ss { ssChunks = newChunks
                      , ssDirtyChunks = dirty
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
simulateActiveChunk allChunks coord scs =
    let terrainV = scsTerrain scs
        sz = chunkSize * chunkSize
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
        newFluid = V.imap (\idx mafc →
            case mafc of
                Nothing  → Nothing
                Just afc | afcVolume afc ≡ 0 → Nothing
                         | otherwise →
                    let terrZ = terrainV VU.! idx
                    in Just (FluidCell (afcType afc)
                                (volumeToSurface terrZ (afcVolume afc)))
            ) newActive

    in (scs { scsActiveFluid = newActive
            , scsFluid       = newFluid
            , scsSideDeco    = newDeco
            }, changed)

-----------------------------------------------------------
-- Phase A: Gravity — downhill flow
-----------------------------------------------------------

phaseGravity ∷ MV.MVector s (Maybe ActiveFluidCell)
             → VU.Vector Int
             → STRef s Bool
             → ST s ()
phaseGravity mv terrainV changedRef = do
    -- Snapshot: read all decisions from frozen state, write transfers to mv
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
                    -- Compute all transfers from snapshot, then apply
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
                    -- Cap total outflow to available volume
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
                            -- Subtract from source
                            srcCell ← MV.read mv idx
                            case srcCell of
                                Just src → MV.write mv idx
                                    (Just src { afcVolume = afcVolume src - fromIntegral actual })
                                Nothing → pure ()
                            -- Add to destination
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

-----------------------------------------------------------
-- Phase B: Lateral pressure equalization
-----------------------------------------------------------

phaseLateral ∷ MV.MVector s (Maybe ActiveFluidCell)
             → VU.Vector Int
             → STRef s Bool
             → ST s ()
phaseLateral mv terrainV changedRef = do
    -- Snapshot: read all decisions from frozen state, write transfers to mv
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
                        -- Same terrain height: equalize volumes
                        when (nTerrZ ≡ terrZ) $ do
                            let nbrCell = snap V.! nIdx
                            case nbrCell of
                                Just nfc → do
                                    let dstVol = fromIntegral (afcVolume nfc) ∷ Int
                                        diff   = srcVol - dstVol
                                    -- Only transfer if we are the higher side (src > dst)
                                    -- to avoid both sides transferring to each other
                                    when (diff > 1) $ do
                                        let transfer = max 1 (diff `div` 4)
                                        -- Apply to live state
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
                                    -- Source has excess: share with empty neighbor at same height
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

-----------------------------------------------------------
-- Phase C: Waterfall detection
-----------------------------------------------------------

phaseWaterfall ∷ MV.MVector s (Maybe ActiveFluidCell)
               → MVU.MVector s Word8
               → VU.Vector Int
               → STRef s Bool
               → ST s ()
phaseWaterfall mv decoMv terrainV changedRef = do
    -- Snapshot: read all decisions from frozen state, write transfers to mv
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
                    -- Compute all waterfall transfers from snapshot
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
                    -- Cap total outflow to available volume
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
                            -- Subtract from source
                            srcCell ← MV.read mv idx
                            case srcCell of
                                Just src → MV.write mv idx
                                    (Just src { afcVolume = afcVolume src - fromIntegral actual })
                                Nothing → pure ()
                            -- Add volume at bottom
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
                            -- Mark flow direction
                            fd ← readSTRef flowDirRef
                            writeSTRef flowDirRef (fd .|. ((1 ∷ Word8) `shiftL` dirBit))
                            -- Write waterfall deco at the cliff edge
                            MVU.write decoMv idx (17 + fromIntegral (dirBit `mod` 4))
                            writeSTRef changedRef True
                -- Update flow direction
                newFD ← readSTRef flowDirRef
                when (newFD ≠ afcFlowDir afc) $ do
                    cur ← MV.read mv idx
                    case cur of
                        Just c → MV.write mv idx (Just c { afcFlowDir = newFD })
                        Nothing → pure ()

-----------------------------------------------------------
-- Phase D: Dry-out
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

cardinalNeighbors ∷ Int → Int → [(Int, Int)]
cardinalNeighbors lx ly =
    [(lx, ly - 1), (lx + 1, ly), (lx, ly + 1), (lx - 1, ly)]
{-# INLINE cardinalNeighbors #-}

