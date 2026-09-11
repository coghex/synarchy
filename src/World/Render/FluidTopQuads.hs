{-# LANGUAGE Strict #-}
-- | Fluid SURFACE (top) quads for one chunk: Ocean, Lava, River and Lake.
--
--   Split out of "World.Render.Quads" by #2517. The flat-top contract and
--   the ice-cover eligibility rules below are the whole subject of DFL-1,
--   and 'World.Render.Quads.renderWorldQuads' needs an 'EngineEnv' — so
--   the pass lives here, where a pure headless spec can drive the real
--   production boundary over real neighbour topologies.
--
--   Vertical fluid EDGES are not built here. Every positive visible drop
--   between a fluid top and a lower neighbour is a side quad from
--   'World.Render.SideDecoQuads.waterSideFaceQuads'.
module World.Render.FluidTopQuads
    ( fluidTopQuads
    ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Scene.Types (SortableQuad(..))
import World.Chunk.Types (ChunkCoord, chunkSize)
import World.Fluid.Types (FluidCell(..), FluidType(..), IceCell)
import World.Generate (chunkToGlobal)
import World.Grid (gridToScreen, tileSideHeight)
import World.Render.QuadContext (QuadContext(..), ZSlice(..)
                                , EffectiveDepth(..))
import World.Render.TileQuads
    (oceanTileToQuad, lavaTileToQuad, freshwaterTileToQuad)
import World.Render.ViewBounds (ViewBounds, isTileVisible)

-- | Build one chunk's fluid top quads, partitioned
--   @(ocean, lava, freshwater)@ so the caller keeps its established
--   concatenation — and therefore its paint — order.
--
--   Eligibility is unchanged by #2517 and deliberately asymmetric:
--
--     * every fluid top is clipped to the @[zSlice - effDepth, zSlice]@
--       window and to 'ViewBounds';
--     * an Ocean or Lake cell under ice draws no top, because the ice
--       overlay is drawn above it instead;
--     * a River cell under ice still draws its top, and Lava is never
--       ice-covered.
--
--   __River and Lake tops are flat (#2517).__ They pass through
--   'freshwaterTileToQuad', which takes no slope id and consults no
--   neighbour, so no neighbour topology reachable from this fold can
--   select a ramp face map. Ocean and Lava tops were already flat.
fluidTopQuads ∷ QuadContext
              → ChunkCoord
              → V.Vector (Maybe FluidCell)  -- ^ this chunk's fluid map
              → V.Vector (Maybe IceCell)    -- ^ this chunk's ice map
              → ViewBounds
              → ([SortableQuad], [SortableQuad], [SortableQuad])
fluidTopQuads ctx coord fluidMap iceMap vb =
    V.ifoldl' step ([], [], []) fluidMap
  where
    lookupSlot   = qcLookupSlot ctx
    lookupFmSlot = qcLookupFmSlot ctx
    textures     = qcTextures ctx
    facing       = qcFacing ctx
    zSlice       = unZSlice (qcZSlice ctx)
    effDepth     = unEffectiveDepth (qcEffectiveDepth ctx)
    zoomAlpha    = qcTileAlpha ctx
    (wrapX, wrapY) = qcWrapOffset ctx

    step (!oAcc, !lAcc, !fAcc) idx mFluid = case mFluid of
        Nothing → (oAcc, lAcc, fAcc)
        Just fc →
            if fcSurface fc > zSlice ∨ fcSurface fc < (zSlice - effDepth)
            then (oAcc, lAcc, fAcc)
            else
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    (gx, gy) = chunkToGlobal coord lx ly
                    (rawX, rawY) = gridToScreen facing gx gy
                    relativeZ = fcSurface fc - zSlice
                    heightOffset = fromIntegral relativeZ * tileSideHeight
                    drawX = rawX + wrapX
                    drawY = rawY + wrapY - heightOffset
                    -- Skip ocean/lake rendering where ice covers the surface
                    hasIce = isJust (iceMap V.! idx)
                    ocean = oceanTileToQuad lookupSlot lookupFmSlot textures
                                facing gx gy (fcSurface fc) zSlice effDepth
                                zoomAlpha (wrapX, wrapY)
                    lava = lavaTileToQuad lookupSlot lookupFmSlot textures
                               facing gx gy (fcSurface fc) zSlice effDepth
                               zoomAlpha (wrapX, wrapY)
                    fresh ft = freshwaterTileToQuad lookupSlot lookupFmSlot
                                   textures facing gx gy (fcSurface fc) ft
                                   zSlice effDepth zoomAlpha (wrapX, wrapY)
                in if not (isTileVisible vb drawX drawY)
                   then (oAcc, lAcc, fAcc)
                   else case fcType fc of
                        Ocean
                          | hasIce    → (oAcc, lAcc, fAcc)
                          | otherwise → (ocean : oAcc, lAcc, fAcc)
                        Lava          → (oAcc, lava : lAcc, fAcc)
                        Lake
                          | hasIce    → (oAcc, lAcc, fAcc)
                          | otherwise → (oAcc, lAcc, fresh Lake : fAcc)
                        River         → (oAcc, lAcc, fresh River : fAcc)
