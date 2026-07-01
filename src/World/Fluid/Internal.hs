{-# LANGUAGE Strict, UnicodeSyntax  #-}
module World.Fluid.Internal
    ( FluidMap
    , emptyFluidMap
    , withFluidMap
    , forEachSurface
    , wrappedDeltaUVFluid
    , wrapChunkCoordU
    , floorDiv'
    , preferFluidMap
    , lavaOverrides
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Control.Monad.ST (ST, runST)
import World.Fluid.Types (FluidCell(..))
import World.Chunk.Types (chunkSize, wrapChunkCoordU)

type FluidMap = V.Vector (Maybe FluidCell)

-- * Helpers (Vector builders)

emptyFluidMap ∷ FluidMap
emptyFluidMap = V.replicate (chunkSize * chunkSize) Nothing

withFluidMap ∷ (∀ s. MV.MVector s (Maybe FluidCell) → ST s ()) → FluidMap
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

-- * Wrapped distance in u-space

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

-- * Chunk coord wrapping
--
-- 'wrapChunkCoordU' is the canonical seam wrap, defined in
-- "World.Chunk.Types" and re-exported here so the fluid / ocean / seabed /
-- lake / magma / zoommap paths share one source of truth with the slope /
-- chunk-loading path (see "World.Slope"). See issue #316.

-- * Misc helpers

floorDiv' ∷ Int → Int → Int
floorDiv' a b = floor (fromIntegral a / fromIntegral b ∷ Double)

-- | Element-wise merge of two fluid maps. The /first/ argument's cell wins
--   when both are @Just@; @Nothing@ in the first falls back to the second.
--
--   This is asymmetric: call as @preferFluidMap preferred fallback@.
--   Chained left-associatively, the leftmost layer has highest priority:
--
--   > river `preferFluidMap` lake `preferFluidMap` ocean
--   >   -- river wins over lake, which wins over ocean
preferFluidMap ∷ FluidMap → FluidMap → FluidMap
preferFluidMap = V.zipWith (\preferred fallback → case preferred of
    Just _  → preferred
    Nothing → fallback
  )

-- | Layer lava on top of any water-class fluid. Lava cells overwrite
--   river / lake / ocean cells at the same tile; water cells fill in
--   wherever lava is absent. This is the user-stated invariant for
--   the world's fluid system — wrapping it in a named function so
--   the call site reads as the intent, and so flipping the argument
--   order requires an explicit rename instead of silently inverting
--   the priority.
lavaOverrides ∷ FluidMap  -- ^ lava cells
              → FluidMap  -- ^ water (river / lake / ocean) cells
              → FluidMap
lavaOverrides = preferFluidMap

-- * Fluid Equilibration

