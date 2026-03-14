{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.Fluid.Tick
    ( simulateFluidTick
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Control.Monad (forM_, when, unless)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal (FluidMap)
import World.Constants (seaLevel)
import Sim.State.Types (SimState(..), SimChunkState(..))

simulateFluidTick ∷ SimState → SimState
simulateFluidTick ss
    | ssPaused ss = ss
    | HM.null (ssChunks ss) = ss
    | otherwise = simulateOnce ss

simulateOnce ∷ SimState → SimState
simulateOnce ss =
    let chunks = ssChunks ss
        results = HM.mapWithKey (simulateChunk chunks) chunks
        dirty = HM.foldlWithKey' (\acc cc (_, changed) →
            if changed then HS.insert cc acc else acc
            ) (ssDirtyChunks ss) results
        newChunks = HM.map fst results
    in ss { ssChunks = newChunks
          , ssDirtyChunks = dirty
          }

-- | Simulate fluid for a single chunk.
--
--   Algorithm:
--     1. Smooth (20 sub-iterations): Each water tile lowers toward
--        min(water neighbor surfaces) + 1. Also raises by 1 per sub-iter
--        toward max(water neighbor surfaces) - 1 when current surface is
--        lower. Tiles that drop to terrain get removed.
--     1.5. Fill gaps (up to 20 iterations): Dry tiles with ≥2 cardinal
--        water neighbors whose surface > terrain get filled at min of
--        those water surfaces. Floods terrain bumps inside rivers.
--     2. Drain: Isolated tiles (no water neighbors) lower by 1 per tick.
--     3. Replenish: Generated valley tiles that are dry get restored
--        at their water neighbor's level (not terrain+1).
--     4. Flow: Water spreads to lowest dry neighbor.
--     5. Bank strip (iterating): Remove water on slopes — any tile whose
--        terrain is above a neighbor's terrain gets removed. Iterates
--        up to 20 times to cascade along valley walls.
simulateChunk ∷ HM.HashMap ChunkCoord SimChunkState
              → ChunkCoord → SimChunkState → (SimChunkState, Bool)
simulateChunk allChunks coord scs =
    let terrainV = scsTerrain scs
        fluidV   = scsFluid scs
        genFluid = scsGenFluid scs
        sz       = chunkSize * chunkSize
        (newFluid, changed) = runST $ do
            mv ← V.thaw fluidV
            changedRef ← newSTRef False

            -- Phase 1: Smooth — hybrid bidirectional propagation, 20 sub-iters.
            -- Lower: fast (jump) when diff > 3 (drains walls quickly),
            --         slow (by 1) when diff ≤ 3 (prevents shore depressions).
            -- Raise: by 1 per sub-iter toward max(water neighbors) - 1.
            -- 20 iterations propagates smoothing up to 20 tiles deep per tick.
            forM_ [(1∷Int)..20] $ \_ → do
                forM_ [0 .. sz - 1] $ \idx → do
                    cell ← MV.read mv idx
                    case cell of
                        Nothing → pure ()
                        Just fc → do
                            let terrZ  = terrainV VU.! idx
                                mySurf = fcSurface fc
                            when (fcType fc /= Ocean ∧ mySurf > terrZ) $ do
                                let lx = idx `mod` chunkSize
                                    ly = idx `div` chunkSize
                                    nbrs = [(lx-1,ly),(lx+1,ly)
                                           ,(lx,ly-1),(lx,ly+1)]
                                nbrInfo ← getNeighborInfo mv terrainV
                                              allChunks coord nbrs
                                let waterSurfs = [ s | (s, _, True) ← nbrInfo ]
                                unless (null waterSurfs) $ do
                                    let minW = minimum waterSurfs
                                        maxW = maximum waterSurfs
                                        target = minW + 1
                                        diff   = mySurf - target
                                    if diff > 0
                                        then do
                                            -- Fast lower for walls (diff>3),
                                            -- slow lower for small diffs
                                            let newSurf = if diff > 3
                                                          then target
                                                          else mySurf - 1
                                            if newSurf ≤ terrZ
                                                then MV.write mv idx Nothing
                                                else MV.write mv idx
                                                        (Just fc { fcSurface = newSurf })
                                            writeSTRef changedRef True
                                        -- Raise by 1 toward max - 1
                                        else when (mySurf < maxW - 1) $ do
                                            let raised = mySurf + 1
                                            MV.write mv idx
                                                (Just fc { fcSurface = raised })
                                            writeSTRef changedRef True

            -- Phase 1.5: Fill gaps — flood dry tiles surrounded by water.
            -- Two modes:
            --   a) Generated water tile with 1+ water neighbor: always fill
            --      (water belongs here per world gen).
            --   b) Non-generated with 2+ water neighbors ABOVE terrain:
            --      fill at min of those surfaces.
            -- Non-generated tiles where terrain is above water level are
            -- NOT filled — they are legitimate terrain features (islands,
            -- ridges) and filling them creates visible bumps.
            -- Surface = max(terrZ+1, min water neighbor surface).
            -- Iterates to handle multi-tile gaps.
            let fillGaps = do
                    filledAny ← newSTRef False
                    forM_ [0 .. sz - 1] $ \idx → do
                        cell ← MV.read mv idx
                        case cell of
                            Just _ → pure ()
                            Nothing → do
                                let terrZ = terrainV VU.! idx
                                    lx    = idx `mod` chunkSize
                                    ly    = idx `div` chunkSize
                                    nbrs  = [(lx-1,ly),(lx+1,ly)
                                            ,(lx,ly-1),(lx,ly+1)]
                                    wasGen = case genFluid V.! idx of
                                                 Just gf → fcType gf /= Ocean
                                                 Nothing → False
                                nbrInfo ← getNeighborInfo mv terrainV
                                              allChunks coord nbrs
                                let waterNbrs  = [ s | (s, _, True) ← nbrInfo ]
                                    waterAbove = [ s | s ← waterNbrs, s > terrZ ]
                                    nWater     = length waterNbrs
                                    doFill = wasGen ∧ nWater ≥ 1
                                           ∨ length waterAbove ≥ 2
                                when doFill $ do
                                    let minW = if null waterNbrs
                                               then terrZ + 1
                                               else minimum waterNbrs
                                        fillSurf = max (terrZ + 1) minW
                                    MV.write mv idx
                                        (Just (FluidCell River fillSurf))
                                    writeSTRef filledAny True
                                    writeSTRef changedRef True
                    readSTRef filledAny
                fillGapsLoop 0 = pure ()
                fillGapsLoop n = do
                    again ← fillGaps
                    when again $ fillGapsLoop (n - 1)
            fillGapsLoop 20

            -- Phase 2: Drain — isolated tiles (no water neighbors) lower.
            forM_ [0 .. sz - 1] $ \idx → do
                cell ← MV.read mv idx
                case cell of
                    Nothing → pure ()
                    Just fc → do
                        let terrZ  = terrainV VU.! idx
                            mySurf = fcSurface fc
                        when (fcType fc /= Ocean ∧ mySurf > terrZ) $ do
                            let lx = idx `mod` chunkSize
                                ly = idx `div` chunkSize
                                nbrs = [(lx-1,ly),(lx+1,ly)
                                       ,(lx,ly-1),(lx,ly+1)]
                            nbrInfo ← getNeighborInfo mv terrainV
                                          allChunks coord nbrs
                            let hasWaterNbr = any (\(_, _, isW) → isW) nbrInfo
                            when (not hasWaterNbr) $ do
                                let newSurf = mySurf - 1
                                if newSurf ≤ terrZ
                                    then MV.write mv idx Nothing
                                    else MV.write mv idx
                                            (Just fc { fcSurface = newSurf })
                                writeSTRef changedRef True

            -- Phase 3: Replenish — restore generated water tiles that are dry.
            -- If a generated (non-ocean) tile is dry and has a water neighbor,
            -- restore it. Surface = max(terrZ+1, min water neighbor surface).
            -- This handles bumps inside lakes/rivers that got drained.
            forM_ [0 .. sz - 1] $ \idx → do
                case genFluid V.! idx of
                    Nothing → pure ()
                    Just genFc → do
                        when (fcType genFc /= Ocean) $ do
                            cell ← MV.read mv idx
                            case cell of
                                Just _ → pure ()  -- already has water
                                Nothing → do
                                    let terrZ = terrainV VU.! idx
                                        lx    = idx `mod` chunkSize
                                        ly    = idx `div` chunkSize
                                        nbrCoords = filter (\(nx,ny) →
                                            nx ≥ 0 ∧ nx < chunkSize ∧
                                            ny ≥ 0 ∧ ny < chunkSize)
                                            [(lx-1,ly),(lx+1,ly)
                                            ,(lx,ly-1),(lx,ly+1)]
                                    -- Find min water neighbor surface
                                    waterLevelRef ← newSTRef (Nothing ∷ Maybe Int)
                                    forM_ nbrCoords $ \(nx,ny) → do
                                        let nIdx = ny * chunkSize + nx
                                        nCell ← MV.read mv nIdx
                                        case nCell of
                                            Just nfc | fcType nfc /= Ocean → do
                                                let ns = fcSurface nfc
                                                cur ← readSTRef waterLevelRef
                                                case cur of
                                                    Nothing →
                                                        writeSTRef waterLevelRef (Just ns)
                                                    Just cs →
                                                        when (ns < cs) $
                                                            writeSTRef waterLevelRef (Just ns)
                                            _ → pure ()
                                    mWater ← readSTRef waterLevelRef
                                    case mWater of
                                        Nothing → pure ()
                                        Just waterLevel → do
                                            let fillSurf = max (terrZ + 1) waterLevel
                                            MV.write mv idx
                                                (Just (FluidCell River fillSurf))
                                            writeSTRef changedRef True

            -- Phase 4: Flow — spread water to lowest dry neighbor.
            forM_ [0 .. sz - 1] $ \idx → do
                cell ← MV.read mv idx
                case cell of
                    Nothing → pure ()
                    Just fc → do
                        let terrZ  = terrainV VU.! idx
                            mySurf = fcSurface fc
                        when (fcType fc /= Ocean ∧ mySurf > terrZ) $ do
                            let lx = idx `mod` chunkSize
                                ly = idx `div` chunkSize
                                cardinals = [(lx-1,ly),(lx+1,ly)
                                            ,(lx,ly-1),(lx,ly+1)]
                            bestRef ← newSTRef (Nothing ∷ Maybe (Int, Int))
                            forM_ cardinals $ \(nlx, nly) → do
                                let inBounds = nlx ≥ 0 ∧ nlx < chunkSize
                                             ∧ nly ≥ 0 ∧ nly < chunkSize
                                when inBounds $ do
                                    let nIdx   = nly * chunkSize + nlx
                                        nTerrZ = terrainV VU.! nIdx
                                    nCell ← MV.read mv nIdx
                                    case nCell of
                                        Nothing →
                                            when (nTerrZ < mySurf ∧ nTerrZ ≤ terrZ) $ do
                                                cur ← readSTRef bestRef
                                                case cur of
                                                    Nothing →
                                                        writeSTRef bestRef
                                                            (Just (nIdx, nTerrZ))
                                                    Just (_, bestT) →
                                                        when (nTerrZ < bestT) $
                                                            writeSTRef bestRef
                                                                (Just (nIdx, nTerrZ))
                                        _ → pure ()
                            best ← readSTRef bestRef
                            case best of
                                Nothing → pure ()
                                Just (tgtIdx, tgtTerrZ) → do
                                    let newTgtSurf = tgtTerrZ + 1
                                    MV.write mv tgtIdx
                                        (Just (FluidCell River newTgtSurf))
                                    writeSTRef changedRef True

            -- Phase 5: Strip bank water — remove water on steep valley walls.
            -- Only strip tiles that were NOT originally generated as water
            -- AND whose terrain is well above the river floor (>3 above
            -- min water neighbor terrain). Protects shorelines/banks.
            -- Iterates to cascade up the walls.
            let bankStrip = do
                    strippedAny ← newSTRef False
                    forM_ [0 .. sz - 1] $ \idx → do
                        cell ← MV.read mv idx
                        case cell of
                            Nothing → pure ()
                            Just fc → do
                                let terrZ  = terrainV VU.! idx
                                    wasGen = case genFluid V.! idx of
                                                 Just _  → True
                                                 Nothing → False
                                when (fcType fc /= Ocean ∧ not wasGen) $ do
                                    let lx = idx `mod` chunkSize
                                        ly = idx `div` chunkSize
                                        cardinals = [(lx-1,ly),(lx+1,ly)
                                                    ,(lx,ly-1),(lx,ly+1)]
                                    -- Find min terrain among water neighbors
                                    minWaterTerrRef ← newSTRef (Nothing ∷ Maybe Int)
                                    forM_ cardinals $ \(nlx, nly) → do
                                        let inBounds = nlx ≥ 0 ∧ nlx < chunkSize
                                                     ∧ nly ≥ 0 ∧ nly < chunkSize
                                        when inBounds $ do
                                            let nIdx   = nly * chunkSize + nlx
                                                nTerrZ = terrainV VU.! nIdx
                                            nCell ← MV.read mv nIdx
                                            case nCell of
                                                Just nfc | fcType nfc /= Ocean → do
                                                    cur ← readSTRef minWaterTerrRef
                                                    case cur of
                                                        Nothing →
                                                            writeSTRef minWaterTerrRef (Just nTerrZ)
                                                        Just ct →
                                                            when (nTerrZ < ct) $
                                                                writeSTRef minWaterTerrRef (Just nTerrZ)
                                                _ → pure ()
                                    mMinWT ← readSTRef minWaterTerrRef
                                    case mMinWT of
                                        Nothing → pure ()
                                        Just minWT →
                                            -- Strip if terrain is >3 above
                                            -- lowest water neighbor terrain
                                            when (terrZ > minWT + 3) $ do
                                                MV.write mv idx Nothing
                                                writeSTRef strippedAny True
                                                writeSTRef changedRef True
                    readSTRef strippedAny
                bankStripLoop 0 = pure ()
                bankStripLoop n = do
                    again ← bankStrip
                    when again $ bankStripLoop (n - 1)
            bankStripLoop 20

            result ← V.freeze mv
            ch ← readSTRef changedRef
            pure (result, ch)
    in (scs { scsFluid = newFluid }, changed)

-- | Get info about cardinal neighbors.
--   Returns [(surface, terrainZ, isWater)]
getNeighborInfo ∷ MV.MVector s (Maybe FluidCell)
                → VU.Vector Int
                → HM.HashMap ChunkCoord SimChunkState
                → ChunkCoord
                → [(Int, Int)]
                → ST s [(Int, Int, Bool)]
getNeighborInfo mv terrainV allChunks myCoord nbrs = do
    results ← newSTRef []
    forM_ nbrs $ \(nlx, nly) → do
        let inBounds = nlx ≥ 0 ∧ nlx < chunkSize ∧ nly ≥ 0 ∧ nly < chunkSize
        if inBounds
            then do
                let nIdx   = nly * chunkSize + nlx
                    nTerrZ = terrainV VU.! nIdx
                nCell ← MV.read mv nIdx
                case nCell of
                    Just nfc | fcType nfc /= Ocean →
                        modifySTRef' results ((fcSurface nfc, nTerrZ, True) :)
                    Just nfc →
                        modifySTRef' results ((fcSurface nfc, nTerrZ, False) :)
                    Nothing →
                        modifySTRef' results ((nTerrZ, nTerrZ, False) :)
            else do
                let (nCoord, cnlx, cnly) = resolveNeighborCoord myCoord nlx nly
                case HM.lookup nCoord allChunks of
                    Nothing → pure ()
                    Just ncs → do
                        let nIdx   = cnly * chunkSize + cnlx
                            nTerrZ = scsTerrain ncs VU.! nIdx
                            nCell  = scsFluid ncs V.! nIdx
                        case nCell of
                            Just nfc | fcType nfc /= Ocean →
                                modifySTRef' results
                                    ((fcSurface nfc, nTerrZ, True) :)
                            Just nfc →
                                modifySTRef' results
                                    ((fcSurface nfc, nTerrZ, False) :)
                            Nothing →
                                modifySTRef' results
                                    ((nTerrZ, nTerrZ, False) :)
    readSTRef results

resolveNeighborCoord ∷ ChunkCoord → Int → Int → (ChunkCoord, Int, Int)
resolveNeighborCoord (ChunkCoord cx cy) lx ly =
    let (dcx, nlx) | lx < 0         = (-1, lx + chunkSize)
                   | lx ≥ chunkSize = (1,  lx - chunkSize)
                   | otherwise       = (0,  lx)
        (dcy, nly) | ly < 0         = (-1, ly + chunkSize)
                   | ly ≥ chunkSize = (1,  ly - chunkSize)
                   | otherwise       = (0,  ly)
    in (ChunkCoord (cx + dcx) (cy + dcy), nlx, nly)
