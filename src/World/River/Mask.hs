{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.River.Mask
    ( -- * Types (re-exported from World.River.Types)
      RiverMask
    , RiverTileInfo(..)
    , emptyRiverMask
    , riverMaskAt
      -- * Construction
    , computeRiverMask
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Base (GeoCoord(..))
import World.Hydrology.Types (RiverParams(..), RiverSegment(..))
import World.River.Types (RiverMask, RiverTileInfo(..), emptyRiverMask, riverMaskAt)
import World.Constants (seaLevel)

-- Local constant to avoid circular import with Chunk.Types
chunkSize ∷ Int
chunkSize = 16

-- * Mask computation

-- | Compute the river mask for a chunk. This mirrors the
--   geometry logic in fillRiverDirect / riverFillFromSegment
--   but stores metadata instead of placing fluid.
--
--   Each tile gets the info from the CLOSEST river segment
--   that would place water there. When two rivers overlap,
--   the one with the lower water surface wins (same as the
--   current fluid fill).
computeRiverMask ∷ [(Int, RiverParams)]  -- ^ (route index, params)
                 → Int                    -- ^ worldSize
                 → (Int, Int)             -- ^ (cx, cy) chunk coord
                 → VU.Vector Int          -- ^ terrain surface map
                 → RiverMask
computeRiverMask rivers worldSize (cx, cy) surfaceMap =
    let
        chunkGX = cx * chunkSize
        chunkGY = cy * chunkSize
        nearbyRivers = filter (riverNearChunk worldSize chunkGX chunkGY . snd) rivers
    in V.generate (chunkSize * chunkSize) $ \idx →
        let lx = idx `mod` chunkSize
            ly = idx `div` chunkSize
            gx = chunkGX + lx
            gy = chunkGY + ly
            surfZ = surfaceMap VU.! idx
        in bestRiverMask nearbyRivers worldSize gx gy surfZ

-- | Find the best river match for a tile. Returns the closest
--   river segment's info, or Nothing if no river covers this tile.
bestRiverMask ∷ [(Int, RiverParams)] → Int → Int → Int → Int
              → Maybe RiverTileInfo
bestRiverMask rivers worldSize gx gy surfZ =
    go Nothing rivers
  where
    go acc [] = acc
    go acc ((routeIdx, rp) : rest) =
        case bestSegmentMatch worldSize gx gy surfZ (rpSegments rp) of
            Nothing → go acc rest
            Just (ws, refE, perpD, chW) →
                let info = RiverTileInfo routeIdx ws refE perpD chW
                in case acc of
                    Nothing → go (Just info) rest
                    Just prev
                        -- When two rivers overlap, keep the one with
                        -- lower water surface (water seeks its level).
                        | rtiWaterSurf info < rtiWaterSurf prev →
                            go (Just info) rest
                        | otherwise → go acc rest

-- | Check the best matching segment for a tile. Returns
--   (waterSurface, refElev, perpDist, channelHalfWidth)
--   or Nothing if no segment covers this tile.
bestSegmentMatch ∷ Int → Int → Int → Int → V.Vector RiverSegment
                 → Maybe (Int, Int, Float, Float)
bestSegmentMatch worldSize gx gy surfZ segments =
    go Nothing (V.toList segments)
  where
    go acc [] = acc
    go acc (seg : rest) =
        case segmentMatch worldSize gx gy surfZ seg of
            Nothing → go acc rest
            Just result@(_, _, perpD, _) →
                case acc of
                    Nothing → go (Just result) rest
                    Just (_, _, bestD, _)
                        | perpD < bestD → go (Just result) rest
                        | otherwise     → go acc rest

-- | Check if a single segment covers a tile. Mirrors the logic
--   in riverFillFromSegmentWithDist but returns metadata instead
--   of a FluidCell.
segmentMatch ∷ Int → Int → Int → Int → RiverSegment
             → Maybe (Int, Int, Float, Float)
segmentMatch worldSize gx gy surfZ seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        (dxi, dyi) = wrappedDeltaUVFluid worldSize sx sy ex ey
        dx' = fromIntegral dxi ∷ Float
        dy' = fromIntegral dyi ∷ Float
        segLen2 = dx' * dx' + dy' * dy'
    in if segLen2 < 1.0
       then Nothing
       else
       let (pxi, pyi) = wrappedDeltaUVFluid worldSize sx sy gx gy
           px = fromIntegral pxi ∷ Float
           py = fromIntegral pyi ∷ Float
           tRaw = (px * dx' + py * dy') / segLen2
       in if tRaw < -0.05 ∨ tRaw > 1.05
          then Nothing
          else
          let signedPerp = (px * dy' - py * dx') / sqrt segLen2
              effectivePerpDist = abs signedPerp
              valleyEdge = fromIntegral (rsValleyWidth seg) / 2.0 ∷ Float
          in if effectivePerpDist > valleyEdge
             then Nothing
             else
             let tClamped = max 0.0 (min 1.0 tRaw)
                 startW = fromIntegral (rsWaterStart seg) ∷ Float
                 endW   = fromIntegral (rsWaterEnd seg) ∷ Float
                 axialWaterSurface = floor (startW + tClamped * (endW - startW)) ∷ Int

                 -- No coastal flattening in the mask — the
                 -- projection handles surface clamping.  The old
                 -- blend dropped waterSurface to seaLevel near the
                 -- mouth, causing shouldPlace to fail on tiles at
                 -- terrain > seaLevel (the river-coast gap bug).

                 startE = fromIntegral (rsStartElev seg) ∷ Float
                 endE   = fromIntegral (rsEndElev seg) ∷ Float
                 rawRefElev = floor (startE + tClamped * (endE - startE)) ∷ Int
                 -- Clamp refElev so that inChannelRelaxed can pass
                 -- for carved coastal tiles. The carving floor is
                 -- clamped to seaLevel-1, but inChannelRelaxed
                 -- requires surfZ ≤ refElev+2. For carved tiles at
                 -- terrain 1-3 near the coast, refElev must be ≥ -1
                 -- (so refElev+2 ≥ 1, covering surfZ=1). Using
                 -- seaLevel+1 ensures tiles up to terrain=3 qualify
                 -- (refElev+2 = 3). Without this, carved river
                 -- channels are left dry at ocean/lake mouths.
                 refElev = max (seaLevel + 1) rawRefElev

                 maxFillDepth = rsDepth seg + 4
                 waterSurface = axialWaterSurface
                 channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float

                 inValley = surfZ ≤ refElev
                          ∧ surfZ ≥ refElev - maxFillDepth
                 inChannel = effectivePerpDist ≤ channelHalfW + 1.0
                 inChannelRelaxed = inChannel ∧ surfZ ≤ refElev + 2
                                  ∧ surfZ ≥ refElev - maxFillDepth

                 -- Place water wherever the tile passes the
                 -- geometry + elevation checks. The inValley and
                 -- inChannelRelaxed conditions already validate
                 -- that the terrain is within the carved channel.
                 -- The old `waterSurface > surfZ` guard caused
                 -- false negatives at river mouths where the
                 -- water surface drops to seaLevel but the carved
                 -- terrain is still 1-3 above. The actual water
                 -- surface is computed by placeRiver (clamped to
                 -- at least terrZ+1), so the mask doesn't need
                 -- to enforce it.
                 shouldPlace = inValley ∨ inChannelRelaxed

             in if not shouldPlace
                then Nothing
                else Just (waterSurface, refElev, effectivePerpDist, channelHalfW)

-- * Coordinate math (local copy to avoid circular import with Fluid.Internal)

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

-- * River proximity (copied from River.hs to avoid circular import)

riverNearChunk ∷ Int → Int → Int → RiverParams → Bool
riverNearChunk worldSize chunkGX chunkGY river =
    V.any (segNear worldSize chunkGX chunkGY) (rpSegments river)
  where
    segNear ws cgx cgy seg =
        let margin = rsValleyWidth seg + chunkSize + chunkSize
            cx = cgx + chunkSize `div` 2
            cy = cgy + chunkSize `div` 2
            GeoCoord sx sy = rsStart seg
            (dxsi, dysi) = wrappedDeltaUVFluid ws sx sy cx cy
            startNear = abs dxsi < margin ∧ abs dysi < margin
            GeoCoord ex ey = rsEnd seg
            (dxei, dyei) = wrappedDeltaUVFluid ws ex ey cx cy
            endNear = abs dxei < margin ∧ abs dyei < margin
        in startNear ∨ endNear
