{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.Internal
    ( FluidMap
    , emptyFluidMap
    , withFluidMap
    , forEachSurface
    , wrappedDeltaUVFluid
    , wrapChunkCoordU
    , floorDiv'
    , unionFluidMap
    , equilibrateFluidMap
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import World.Base
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Chunk.Types (ChunkCoord(..), chunkSize)

type FluidMap = V.Vector (Maybe FluidCell)

-----------------------------------------------------------
-- Helpers (Vector builders)
-----------------------------------------------------------

emptyFluidMap ∷ FluidMap
emptyFluidMap = V.replicate (chunkSize * chunkSize) Nothing

withFluidMap ∷ (forall s. MV.MVector s (Maybe FluidCell) → ST s ()) → FluidMap
withFluidMap action = runST $ do
    mv ← MV.replicate (chunkSize * chunkSize) Nothing
    action mv
    V.freeze mv

forEachSurface ∷ VU.Vector Int → (Int → Int → Int → Int → ST s ()) → ST s ()
forEachSurface surfaceMap f =
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        let lx = idx `mod` chunkSize
            ly = idx `div` chunkSize
            surfZ = surfaceMap VU.! idx
        f idx lx ly surfZ

-----------------------------------------------------------
-- Wrapped distance in u-space
-----------------------------------------------------------

-- | Wrapped distance in u-space for fluid computations.
--   Returns (dx, dy) accounting for cylindrical wrapping along u-axis.
{-# INLINE wrappedDeltaUVFluid #-}
wrappedDeltaUVFluid ∷ Int → Int → Int → Int → Int → (Int, Int)
wrappedDeltaUVFluid worldSize gx1 gy1 gx2 gy2 =
    let w = worldSize * chunkSize
        halfW = w `div` 2
        du = (gx1 - gy1) - (gx2 - gy2)
        dv = (gx1 + gy1) - (gx2 + gy2)
        wrappedDU = ((du + halfW) `mod` w + w) `mod` w - halfW
        dx = (wrappedDU + dv) `div` 2
        dy = (dv - wrappedDU) `div` 2
    in (dx, dy)

-----------------------------------------------------------
-- Chunk coord wrapping
-----------------------------------------------------------

wrapChunkCoordU ∷ Int → ChunkCoord → ChunkCoord
wrapChunkCoordU worldSize (ChunkCoord cx cy) =
    let halfSize = worldSize `div` 2
        w = halfSize * 2
        u = cx - cy
        v = cx + cy
        halfW = w `div` 2
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
    in ChunkCoord ((wrappedU + v) `div` 2) ((v - wrappedU) `div` 2)

-----------------------------------------------------------
-- Misc helpers
-----------------------------------------------------------

floorDiv' ∷ Int → Int → Int
floorDiv' a b = floor (fromIntegral a / fromIntegral b ∷ Double)

-- | joins all fluid maps, overwrite priority occurs here
unionFluidMap ∷ FluidMap → FluidMap → FluidMap
unionFluidMap = V.zipWith (\a b → case a of
    Just _  → a
    Nothing → b
  )

-----------------------------------------------------------
-- Fluid Equilibration
-----------------------------------------------------------

-- | Post-processing pass with two phases per iteration:
--   1. LEVEL: lower existing water surfaces to the minimum
--      neighbor water surface. This spreads the lowest water
--      level across connected water bodies, implementing the
--      spillway rule: water settles to the lowest overflow point.
--   2. FILL: propagate water to adjacent empty tiles whose
--      terrain is at or below the minimum neighbor water surface.
--   Iterates until stable (max 20 passes).
equilibrateFluidMap ∷ VU.Vector Int → FluidMap → FluidMap
equilibrateFluidMap surfaceMap fluidMap = runST $ do
    mv ← V.thaw fluidMap
    let area = chunkSize * chunkSize

        pass = do
            changed ← newSTRef False

            -- Phase 1: Level — lower water to min neighbor surface.
            -- Water seeks its lowest level. If a neighbor's water is
            -- lower, our water should flow there (lowering ours).
            -- Skip Ocean, Lava, and River:
            --   Ocean/Lava: levels are fixed by definition.
            --   River: water surface is interpolated along segments and
            --          intentionally varies. Equilibration would flatten
            --          the gradient and cause chunk-boundary artifacts.
            -- Only level Lake fluid.
            forM_ [0 .. area - 1] $ \idx → do
                val ← MV.read mv idx
                case val of
                    Just fc
                      | fcType fc ≡ Ocean → pure ()
                      | fcType fc ≡ Lava  → pure ()
                      | fcType fc ≡ River → pure ()
                      | otherwise → do
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                            surfZ = surfaceMap VU.! idx
                        minNS ← minNeighborSurfaceOfType mv (fcType fc) lx ly
                        case minNS of
                            Just minS
                              | minS < fcSurface fc → do
                                let newSurf = max surfZ minS
                                if newSurf < fcSurface fc
                                    then do
                                        MV.write mv idx (Just (fc { fcSurface = newSurf }))
                                        writeSTRef changed True
                                    else pure ()
                            _ → pure ()
                    _ → pure ()

            -- Phase 2: Fill — propagate water to empty neighbors.
            forM_ [0 .. area - 1] $ \idx → do
                val ← MV.read mv idx
                when (isNothing val) $ do
                    let lx = idx `mod` chunkSize
                        ly = idx `div` chunkSize
                        surfZ = surfaceMap VU.! idx
                    when (surfZ > minBound) $ do
                        result ← containedFill mv lx ly surfZ
                        case result of
                            Nothing → pure ()
                            Just fc → do
                                MV.write mv idx (Just fc)
                                writeSTRef changed True
            readSTRef changed

        loop 0 = pure ()
        loop n = do
            didChange ← pass
            when didChange $ loop (n - 1)

    loop (20 ∷ Int)

    -- Phase 3: Cross-fluid boundary smoothing.
    -- When different fluid types meet (e.g., river-lake), ensure
    -- adjacent water surfaces differ by at most 1 level.  The
    -- renderer only draws smooth slopes for exactly 1-level drops;
    -- anything larger creates a hard cliff.
    -- Raise the lower tile toward the higher one (water fills to
    -- the overflow point).  Iterate to propagate.
    let crossFluidPass = do
            cRef ← newSTRef False
            forM_ [0 .. area - 1] $ \idx → do
                val ← MV.read mv idx
                case val of
                    Just fc
                      | fcType fc ≢ Ocean ∧ fcType fc ≢ Lava → do
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                        maxNbr ← maxNeighborWaterSurface mv lx ly
                        case maxNbr of
                            Just nMax | nMax > fcSurface fc + 1 → do
                                let newSurf = nMax - 1
                                MV.write mv idx (Just (fc { fcSurface = newSurf }))
                                writeSTRef cRef True
                            _ → pure ()
                    _ → pure ()
            readSTRef cRef
        crossLoop 0 = pure ()
        crossLoop n = do
            didChange ← crossFluidPass
            when didChange $ crossLoop (n - 1)
    crossLoop (10 ∷ Int)

    V.freeze mv

-- | Get minimum water surface among 4-connected same-type neighbors.
--   Only considers neighbors with the same FluidType to prevent
--   ocean dragging river levels down (or vice versa).
minNeighborSurfaceOfType ∷ MV.MVector s (Maybe FluidCell) → FluidType
                         → Int → Int → ST s (Maybe Int)
minNeighborSurfaceOfType mv ftype lx ly = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure Nothing
          | otherwise = do
              val ← MV.read mv (y * chunkSize + x)
              pure $ case val of
                  Just fc | fcType fc ≡ ftype → Just (fcSurface fc)
                  _                           → Nothing
    vN ← check lx (ly - 1)
    vS ← check lx (ly + 1)
    vE ← check (lx + 1) ly
    vW ← check (lx - 1) ly
    let surfaces = catMaybes [vN, vS, vE, vW]
    pure $ case surfaces of
        []     → Nothing
        (s:ss) → Just (foldl' min s ss)

-- | Get maximum water surface among 4-connected neighbors (any fluid type
--   except ocean/lava). Used for cross-fluid boundary smoothing.
maxNeighborWaterSurface ∷ MV.MVector s (Maybe FluidCell) → Int → Int
                        → ST s (Maybe Int)
maxNeighborWaterSurface mv lx ly = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure Nothing
          | otherwise = do
              val ← MV.read mv (y * chunkSize + x)
              pure $ case val of
                  Just fc | fcType fc ≢ Ocean ∧ fcType fc ≢ Lava
                      → Just (fcSurface fc)
                  _ → Nothing
    vN ← check lx (ly - 1)
    vS ← check lx (ly + 1)
    vE ← check (lx + 1) ly
    vW ← check (lx - 1) ly
    let surfaces = catMaybes [vN, vS, vE, vW]
    pure $ case surfaces of
        []     → Nothing
        (s:ss) → Just (foldl' max s ss)

-- | Check if a tile should be filled by equilibration.
--   Uses the minimum adjacent water surface (water seeks its level).
--   Only propagates Lake fluid — River has its own segment-based fill
--   that covers the full valley width, and propagation would create
--   lifted water blocks outside the valley.
containedFill ∷ MV.MVector s (Maybe FluidCell)
              → Int → Int → Int → ST s (Maybe FluidCell)
containedFill mv lx ly surfZ = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure Nothing
          | otherwise = MV.read mv (y * chunkSize + x)
    vN ← check lx (ly - 1)
    vS ← check lx (ly + 1)
    vE ← check (lx + 1) ly
    vW ← check (lx - 1) ly
    -- Filter to only Lake neighbors for propagation
    let waterCells = filter (\fc → fcType fc ≡ Lake)
                            (catMaybes [vN, vS, vE, vW])
    pure $ case waterCells of
        [] → Nothing
        (w:ws) → let minSurf = foldl' (\a fc → min a (fcSurface fc))
                                       (fcSurface w) ws
                 in if surfZ ≤ minSurf
                    then Just (FluidCell Lake minSurf)
                    else Nothing

-- | Mutable ST reference (avoids IORef in ST)
newSTRef ∷ a → ST s (MV.MVector s a)
newSTRef v = do
    ref ← MV.new 1
    MV.write ref 0 v
    pure ref

readSTRef ∷ MV.MVector s a → ST s a
readSTRef ref = MV.read ref 0

writeSTRef ∷ MV.MVector s a → a → ST s ()
writeSTRef ref v = MV.write ref 0 v
