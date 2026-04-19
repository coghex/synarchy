{-# LANGUAGE Strict, UnicodeSyntax  #-}
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
    , fillCoastalGaps
    , stripLakeRiverCliffs
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import World.Base
import World.Constants (seaLevel)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Chunk.Types (ChunkCoord(..), chunkSize)

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

wrapChunkCoordU ∷ Int → ChunkCoord → ChunkCoord
wrapChunkCoordU worldSize (ChunkCoord cx cy) =
    let halfSize = worldSize `div` 2
        w = halfSize * 2
        u = cx - cy
        v = cx + cy
        halfW = w `div` 2
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
    in ChunkCoord ((wrappedU + v) `div` 2) ((v - wrappedU) `div` 2)

-- * Misc helpers

floorDiv' ∷ Int → Int → Int
floorDiv' a b = floor (fromIntegral a / fromIntegral b ∷ Double)

-- | joins all fluid maps, overwrite priority occurs here
unionFluidMap ∷ FluidMap → FluidMap → FluidMap
unionFluidMap = V.zipWith (\a b → case a of
    Just _  → a
    Nothing → b
  )

-- * Fluid Equilibration

-- | Propagate lake water to adjacent empty tiles whose terrain is
--   at or below the neighboring lake surface. Iterates until stable
--   (max 20 passes).
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

    loop (20 ∷ Int)
    V.freeze mv

-- | Check if a tile should be filled by equilibration.
--
--   Two modes:
--     Lake: flat propagation at minSurf (water seeks its level).
--     River: terrain-following at terrZ+1 (preserves slope).
--
--   Lake takes priority when both types are adjacent. River
--   propagation fills tiles below the highest adjacent river
--   surface, placing water at terrZ+1 instead of at the
--   neighbor's surface. This prevents flat pools in sloped
--   river channels while still filling overflow gaps.
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
    let allNbrs   = catMaybes [vN, vS, vE, vW]
        lakeCells = filter (\fc → fcType fc ≡ Lake) allNbrs
        riverCells = filter (\fc → fcType fc ≡ River) allNbrs
    pure $ case lakeCells of
        -- Lake neighbor: flat pool at min surface
        (w:ws) →
            let minSurf = foldl' (\a fc → min a (fcSurface fc))
                                 (fcSurface w) ws
            in if surfZ ≤ minSurf
               then Just (FluidCell Lake minSurf)
               else Nothing
        -- River neighbor only: terrain-following fill
        [] → case riverCells of
            [] → Nothing
            _  →
                let maxRiverSurf = foldl' (\a fc → max a (fcSurface fc))
                                          minBound riverCells
                -- Fill if terrain is strictly below the highest
                -- adjacent river surface (genuine overflow).
                in if surfZ < maxRiverSurf
                   then Just (FluidCell River (surfZ + 1))
                   else Nothing

-- | Fill coastal gaps: tiles without fluid that are at or below sea level
--   and adjacent to any fluid (river, ocean, lake) should get ocean water.
--   River carving can create below-sea-level terrain that neither the
--   ocean BFS (which uses pre-carving elevation) nor the river fill
--   (which stops where terrain ≥ water surface) covers.
--
--   Phase 1: fills tiles at or below seaLevel adjacent to any fluid.
--   Phase 2: fills tiles at seaLevel+1 adjacent to river water whose
--   surface is above the tile. This prevents "dry gaps" between the
--   river channel and surrounding terrain at river mouths.
fillCoastalGaps ∷ VU.Vector Int → FluidMap → FluidMap
fillCoastalGaps surfaceMap fluidMap = runST $ do
    mv ← V.thaw fluidMap
    let area = chunkSize * chunkSize

        -- Phase 1: fill at-or-below-sea-level tiles adjacent to any fluid
        gapPass = do
            changed ← newSTRef False
            forM_ [0 .. area - 1] $ \idx → do
                val ← MV.read mv idx
                when (isNothing val) $ do
                    let surfZ = surfaceMap VU.! idx
                    when (surfZ ≤ seaLevel ∧ surfZ > minBound) $ do
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                        hasAdj ← anyAdjacentFluid mv lx ly
                        when hasAdj $ do
                            MV.write mv idx (Just (FluidCell Ocean seaLevel))
                            writeSTRef changed True
            readSTRef changed

        loop1 0 = pure ()
        loop1 n = do
            didChange ← gapPass
            when didChange $ loop1 (n - 1)

    loop1 (10 ∷ Int)

    -- Phase 2: fill tiles just above sea level that are adjacent to
    -- river water with surface above their terrain. These are the
    -- "black hole" tiles at river mouths — carved coastline that is
    -- between the river channel and the ocean. Without this, they
    -- render as dry terrain patches surrounded by water.
    let riverGapPass = do
            changed ← newSTRef False
            forM_ [0 .. area - 1] $ \idx → do
                val ← MV.read mv idx
                when (isNothing val) $ do
                    let surfZ = surfaceMap VU.! idx
                    -- Only fill tiles slightly above sea level (1-2 above)
                    -- that are adjacent to river/ocean water high enough to
                    -- cover them.
                    when (surfZ > seaLevel ∧ surfZ ≤ seaLevel + 2
                          ∧ surfZ > minBound) $ do
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                        adjWater ← maxAdjacentWaterSurface mv lx ly
                        case adjWater of
                            Just ws | ws > surfZ → do
                                -- Cap water depth to prevent deep water on
                                -- what is essentially flat terrain near the coast
                                let cappedWs = min ws (surfZ + 3)
                                MV.write mv idx (Just (FluidCell River cappedWs))
                                writeSTRef changed True
                            _ → pure ()
            readSTRef changed

        loop2 0 = pure ()
        loop2 n = do
            didChange ← riverGapPass
            when didChange $ loop2 (n - 1)

    loop2 (5 ∷ Int)

    -- Phase 3: Smooth river surfaces toward sea level at mouths.
    -- River tiles adjacent to ocean or to already-lowered coastal
    -- water get their surface pulled down. The seaLevel + 6
    -- threshold allows propagation upstream through tiles that
    -- were lowered in previous iterations without flattening
    -- interior river sections.
    let mouthSmooth = do
            changed ← newSTRef False
            forM_ [0 .. area - 1] $ \idx → do
                val ← MV.read mv idx
                case val of
                    Just fc | fcType fc ≡ River → do
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                            surfZ = surfaceMap VU.! idx
                        hasOceanNbr ← anyAdjacentOcean mv lx ly
                        minWater ← minAdjacentWaterSurface mv lx ly
                        let nearCoast = hasOceanNbr ∨ case minWater of
                                Just ms → ms ≤ seaLevel + 6
                                Nothing → False
                        when (nearCoast ∧ fcSurface fc > seaLevel + 2) $ do
                            -- Target = max of terrain+1 and the
                            -- lowest neighbor water surface.  No +1
                            -- offset: the terrain gradient provides
                            -- the natural slope, and removing the
                            -- offset lets the lowering propagate
                            -- past the computeChunkRivers smooth
                            -- floor within 12 iterations.
                            let target = case minWater of
                                    Just ms → max (surfZ + 1) ms
                                    Nothing → max (surfZ + 1) (seaLevel + 1)
                            when (target < fcSurface fc) $ do
                                MV.write mv idx (Just (fc { fcSurface = target }))
                                writeSTRef changed True
                    _ → pure ()
            readSTRef changed

        mouthLoop 0 = pure ()
        mouthLoop n = do
            didChange ← mouthSmooth
            when didChange $ mouthLoop (n - 1)

    mouthLoop (12 ∷ Int)

    -- Phase 4: Convert river tiles near sea level to ocean.
    -- River water at or just above seaLevel is effectively part of
    -- the ocean — keeping it as River causes a visible step at the
    -- coast. Convert any river tile whose surface is within 1 tile
    -- of seaLevel AND whose terrain is at/below seaLevel+1.
    forM_ [0 .. area - 1] $ \idx → do
        val ← MV.read mv idx
        case val of
            Just fc | fcType fc ≡ River ∧ fcSurface fc ≤ seaLevel + 1 → do
                let surfZ = surfaceMap VU.! idx
                when (surfZ ≤ seaLevel + 1) $
                    MV.write mv idx (Just (FluidCell Ocean seaLevel))
            _ → pure ()

    V.freeze mv

anyAdjacentFluid ∷ MV.MVector s (Maybe FluidCell) → Int → Int
                 → ST s Bool
anyAdjacentFluid mv lx ly = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure False
          | otherwise = isJust ⊚ MV.read mv (y * chunkSize + x)
    n ← check lx (ly - 1)
    s ← check lx (ly + 1)
    e ← check (lx + 1) ly
    w ← check (lx - 1) ly
    pure (n ∨ s ∨ e ∨ w)

-- | Check if any cardinal neighbor is ocean fluid.
anyAdjacentOcean ∷ MV.MVector s (Maybe FluidCell) → Int → Int
                 → ST s Bool
anyAdjacentOcean mv lx ly = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure False
          | otherwise = do
              val ← MV.read mv (y * chunkSize + x)
              pure $ case val of
                  Just fc → fcType fc ≡ Ocean
                  Nothing → False
    n ← check lx (ly - 1)
    s ← check lx (ly + 1)
    e ← check (lx + 1) ly
    w ← check (lx - 1) ly
    pure (n ∨ s ∨ e ∨ w)

-- | Get the minimum water surface among cardinal neighbors.
minAdjacentWaterSurface ∷ MV.MVector s (Maybe FluidCell) → Int → Int
                        → ST s (Maybe Int)
minAdjacentWaterSurface mv lx ly = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure Nothing
          | otherwise = do
              val ← MV.read mv (y * chunkSize + x)
              pure $ case val of
                  Just fc → Just (fcSurface fc)
                  Nothing → Nothing
    vN ← check lx (ly - 1)
    vS ← check lx (ly + 1)
    vE ← check (lx + 1) ly
    vW ← check (lx - 1) ly
    let surfaces = catMaybes [vN, vS, vE, vW]
    pure $ case surfaces of
        []     → Nothing
        (s:ss) → Just (foldl' min s ss)

-- | Get the maximum water surface among cardinal neighbors.
--   Used by the river-gap filling phase to determine if adjacent
--   water is high enough to cover this tile.
maxAdjacentWaterSurface ∷ MV.MVector s (Maybe FluidCell) → Int → Int
                        → ST s (Maybe Int)
maxAdjacentWaterSurface mv lx ly = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure Nothing
          | otherwise = do
              val ← MV.read mv (y * chunkSize + x)
              pure $ case val of
                  Just fc → Just (fcSurface fc)
                  Nothing → Nothing
    vN ← check lx (ly - 1)
    vS ← check lx (ly + 1)
    vE ← check (lx + 1) ly
    vW ← check (lx - 1) ly
    let surfaces = catMaybes [vN, vS, vE, vW]
    pure $ case surfaces of
        []     → Nothing
        (s:ss) → Just (foldl' max s ss)

-- * Strip Lake/River Cliff Artifacts

-- | Remove lake tiles that are adjacent to river tiles with a much
--   lower surface. These create massive water cliffs at river banks
--   where the lake fill covers the carved river valley. Iterates to
--   peel back multiple layers of lake tiles from the river bank.
stripLakeRiverCliffs ∷ VU.Vector Int → FluidMap → FluidMap
stripLakeRiverCliffs surfaceMap fluidMap = runST $ do
    mv ← V.thaw fluidMap
    let area = chunkSize * chunkSize

        stripPass = do
            changed ← newSTRef False
            forM_ [0 .. area - 1] $ \idx → do
                val ← MV.read mv idx
                case val of
                    Just fc | fcType fc ≡ Lake → do
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                        hasCliff ← checkAdjacentRiverCliff mv fc surfaceMap lx ly
                        when hasCliff $ do
                            MV.write mv idx Nothing
                            writeSTRef changed True
                    _ → pure ()
            readSTRef changed

        stripLoop 0 = pure ()
        stripLoop n = do
            didChange ← stripPass
            when didChange $ stripLoop (n - 1)

    stripLoop (20 ∷ Int)
    V.freeze mv

-- | Check if a lake tile has a cardinal neighbor that is a river (or
--   dry gap) with a surface more than 2 levels below the lake surface.
checkAdjacentRiverCliff ∷ MV.MVector s (Maybe FluidCell) → FluidCell
                        → VU.Vector Int → Int → Int → ST s Bool
checkAdjacentRiverCliff mv lakeCell surfMap lx ly = do
    let lakeSurf = fcSurface lakeCell
        check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure False
          | otherwise = do
              let nIdx = y * chunkSize + x
                  nTerrZ = surfMap VU.! nIdx
              val ← MV.read mv nIdx
              pure $ case val of
                  Just nfc | fcType nfc ≡ River →
                      lakeSurf - fcSurface nfc > 2
                  Nothing → lakeSurf - nTerrZ > 3
                  _ → False
    n ← check lx (ly - 1)
    s ← check lx (ly + 1)
    e ← check (lx + 1) ly
    w ← check (lx - 1) ly
    pure (n ∨ s ∨ e ∨ w)
