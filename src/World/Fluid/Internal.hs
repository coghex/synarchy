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

-- | Post-processing pass: propagate fluid to adjacent empty tiles
--   whose terrain is at or below the minimum neighboring fluid surface.
--   Uses minimum (not max) water surface — water seeks its lowest
--   level. Iterates until stable (max 8 passes).
equilibrateFluidMap ∷ VU.Vector Int → FluidMap → FluidMap
equilibrateFluidMap surfaceMap fluidMap = runST $ do
    mv ← V.thaw fluidMap
    let area = chunkSize * chunkSize

        pass = do
            changed ← newSTRef False
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

    loop (8 ∷ Int)
    V.freeze mv

-- | Check if a tile should be filled by equilibration.
--   Uses the minimum adjacent water surface (water seeks its level).
--   Only called for interior tiles (not chunk edges).
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
    let waterCells = catMaybes [vN, vS, vE, vW]
    pure $ case waterCells of
        [] → Nothing
        (w:ws) → let minSurf = foldl' (\a fc → min a (fcSurface fc))
                                       (fcSurface w) ws
                     ftype   = fcType w
                 in if surfZ ≤ minSurf
                    then Just (FluidCell ftype minSurf)
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
