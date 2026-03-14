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
    let sz = chunkSize * chunkSize
    forM_ [0 .. sz - 1] $ \idx → do
        cell ← MV.read mv idx
        case cell of
            Nothing → pure ()
            Just afc | afcVolume afc ≡ 0 → pure ()
            Just afc → do
                let terrZ = terrainV VU.! idx
                    mySurf = volumeToSurface terrZ (afcVolume afc)
                    lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    nbrs = cardinalNeighbors lx ly
                forM_ nbrs $ \(nx, ny) →
                    when (nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize) $ do
                        let nIdx = ny * chunkSize + nx
                            nTerrZ = terrainV VU.! nIdx
                        -- Flow downhill: neighbor terrain is lower and water surface lower
                        when (nTerrZ < terrZ) $ do
                            srcCell ← MV.read mv idx
                            case srcCell of
                                Nothing → pure ()
                                Just src | afcVolume src ≡ 0 → pure ()
                                Just src → do
                                    let srcSurf = volumeToSurface terrZ (afcVolume src)
                                    nbrCell ← MV.read mv nIdx
                                    let nbrSurf = case nbrCell of
                                            Nothing  → nTerrZ
                                            Just nfc → volumeToSurface nTerrZ (afcVolume nfc)
                                    when (srcSurf > nbrSurf) $ do
                                        let surfDiff = srcSurf - nbrSurf
                                            avail = fromIntegral (afcVolume src)
                                            outflow = min avail
                                                        (surfDiff * volumePerLevel `div` 4)
                                            transfer = max 1 outflow
                                        when (transfer > 0 ∧ avail > 0) $ do
                                            let actualTransfer = min transfer avail
                                                newSrcVol = afcVolume src - fromIntegral actualTransfer
                                            MV.write mv idx (Just src { afcVolume = newSrcVol })
                                            -- Add to destination
                                            dst ← MV.read mv nIdx
                                            case dst of
                                                Nothing →
                                                    MV.write mv nIdx (Just ActiveFluidCell
                                                        { afcType = afcType src
                                                        , afcVolume = fromIntegral actualTransfer
                                                        , afcFlowDir = 0
                                                        })
                                                Just d →
                                                    MV.write mv nIdx (Just d
                                                        { afcVolume = afcVolume d + fromIntegral actualTransfer })
                                            writeSTRef changedRef True

-----------------------------------------------------------
-- Phase B: Lateral pressure equalization
-----------------------------------------------------------

phaseLateral ∷ MV.MVector s (Maybe ActiveFluidCell)
             → VU.Vector Int
             → STRef s Bool
             → ST s ()
phaseLateral mv terrainV changedRef = do
    let sz = chunkSize * chunkSize
    forM_ [0 .. sz - 1] $ \idx → do
        cell ← MV.read mv idx
        case cell of
            Nothing → pure ()
            Just afc | afcVolume afc ≡ 0 → pure ()
            Just afc → do
                let terrZ = terrainV VU.! idx
                    lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    nbrs = cardinalNeighbors lx ly
                forM_ nbrs $ \(nx, ny) →
                    when (nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize) $ do
                        let nIdx = ny * chunkSize + nx
                            nTerrZ = terrainV VU.! nIdx
                        -- Same terrain height: equalize volumes
                        when (nTerrZ ≡ terrZ) $ do
                            srcCell ← MV.read mv idx
                            dstCell ← MV.read mv nIdx
                            case (srcCell, dstCell) of
                                (Just src, Just dst) → do
                                    let srcVol = fromIntegral (afcVolume src) ∷ Int
                                        dstVol = fromIntegral (afcVolume dst) ∷ Int
                                        diff   = srcVol - dstVol
                                    when (diff > 1) $ do
                                        let transfer = min 1 (diff `div` 2)
                                        MV.write mv idx (Just src
                                            { afcVolume = fromIntegral (srcVol - transfer) })
                                        MV.write mv nIdx (Just dst
                                            { afcVolume = fromIntegral (dstVol + transfer) })
                                        writeSTRef changedRef True
                                (Just src, Nothing) | afcVolume src > fromIntegral volumePerLevel → do
                                    -- Source has excess: share with empty neighbor at same height
                                    let srcVol = fromIntegral (afcVolume src) ∷ Int
                                        transfer = min 1 (srcVol `div` 2)
                                    when (transfer > 0) $ do
                                        MV.write mv idx (Just src
                                            { afcVolume = fromIntegral (srcVol - transfer) })
                                        MV.write mv nIdx (Just ActiveFluidCell
                                            { afcType = afcType src
                                            , afcVolume = fromIntegral transfer
                                            , afcFlowDir = 0
                                            })
                                        writeSTRef changedRef True
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
    let sz = chunkSize * chunkSize
    forM_ [0 .. sz - 1] $ \idx → do
        cell ← MV.read mv idx
        case cell of
            Nothing → pure ()
            Just afc | afcVolume afc ≡ 0 → pure ()
            Just afc → do
                let terrZ = terrainV VU.! idx
                    lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    nbrs = cardinalNeighbors lx ly
                flowDirRef ← newSTRef (afcFlowDir afc)
                forM_ (zip [0∷Int ..] nbrs) $ \(dirBit, (nx, ny)) →
                    when (nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize) $ do
                        let nIdx = ny * chunkSize + nx
                            nTerrZ = terrainV VU.! nIdx
                            drop' = terrZ - nTerrZ
                        -- Waterfall: terrain drops > 1
                        when (drop' > 1) $ do
                            srcCell ← MV.read mv idx
                            case srcCell of
                                Nothing → pure ()
                                Just src | afcVolume src ≡ 0 → pure ()
                                Just src → do
                                    let avail = fromIntegral (afcVolume src) ∷ Int
                                        transfer = min avail (volumePerLevel `div` 2)
                                    when (transfer > 0) $ do
                                        let newSrcVol = fromIntegral (avail - transfer)
                                        MV.write mv idx (Just src { afcVolume = newSrcVol })
                                        -- Add volume at bottom
                                        dst ← MV.read mv nIdx
                                        case dst of
                                            Nothing →
                                                MV.write mv nIdx (Just ActiveFluidCell
                                                    { afcType = afcType src
                                                    , afcVolume = fromIntegral transfer
                                                    , afcFlowDir = 0
                                                    })
                                            Just d →
                                                MV.write mv nIdx (Just d
                                                    { afcVolume = afcVolume d + fromIntegral transfer })
                                        -- Mark flow direction
                                        fd ← readSTRef flowDirRef
                                        writeSTRef flowDirRef (fd .|. ((1 ∷ Word8) `shiftL` dirBit))
                                        -- Write waterfall deco at the cliff edge
                                        -- Waterfall deco IDs: 17-20 (17 + variant)
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

