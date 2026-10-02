-- | The authored coordinate and bounds contract (#2699 requirement 7,
--   design D-19/D-21).
--
--   Coordinates are horizontal tile coordinates in conventional
--   Cartesian orientation (positive Y up). A finite map of @width ×
--   height@ tiles is centred on zero:
--
--   > minX = -floor(width / 2);  maxX = minX + width - 1
--   > maxY =  floor(height / 2); minY = maxY - height + 1
--
--   so 20×10 is X −10…9, Y −4…5 and 21×11 is X −10…10, Y −5…5, and the
--   rectangle holds exactly @width × height@ tiles. This contract is
--   independent of the generated world's own convention (the half-open
--   @[−half, half)@ of "World.Chunk.Types"); the runtime slices convert.
--   The camera never translates the bounds.
module Scenario.Bounds
    ( maxScenarioDimension
    , TileBounds(..)
    , mapTileBounds
    , boundsTileCount
    , inBounds
    , clipRegion
    , regionTileCount
    , footprintInside
    , tileOf
    ) where

import UPrelude
import Scenario.Types (MapDimensions(..), TileRegion(..), Footprint(..))

-- | The schema's dimension domain: each of width and height is an
--   integer in @[1, maxScenarioDimension]@. Every bound below is then
--   far inside 'Int', and the arithmetic is still done in 'Integer' and
--   checked on the way back.
maxScenarioDimension ∷ Int
maxScenarioDimension = 100000

-- | Inclusive tile rectangle: @minX minY maxX maxY@.
data TileBounds = TileBounds
    { tbMinX ∷ !Int
    , tbMinY ∷ !Int
    , tbMaxX ∷ !Int
    , tbMaxY ∷ !Int
    } deriving (Show, Eq)

-- | The exact tile rectangle of a finite map, or 'Nothing' outside the
--   dimension domain.
mapTileBounds ∷ MapDimensions → Maybe TileBounds
mapTileBounds (MapDimensions w h)
    | w < 1 ∨ h < 1 ∨ w > maxScenarioDimension ∨ h > maxScenarioDimension
        = Nothing
    | otherwise = do
        let wi = toInteger w
            hi = toInteger h
            minX = negate (wi `div` 2)
            maxX = minX + wi - 1
            maxY = hi `div` 2
            minY = maxY - hi + 1
        TileBounds <$> checked minX ⊛ checked minY ⊛ checked maxX ⊛ checked maxY
  where
    checked ∷ Integer → Maybe Int
    checked v
        | v < toInteger (minBound ∷ Int) ∨ v > toInteger (maxBound ∷ Int)
            = Nothing
        | otherwise = Just (fromInteger v)

boundsTileCount ∷ TileBounds → Integer
boundsTileCount (TileBounds x0 y0 x1 y1) =
    (toInteger x1 - toInteger x0 + 1) * (toInteger y1 - toInteger y0 + 1)

inBounds ∷ TileBounds → (Int, Int) → Bool
inBounds (TileBounds x0 y0 x1 y1) (x, y) =
    x ≥ x0 ∧ x ≤ x1 ∧ y ≥ y0 ∧ y ≤ y1

regionTileCount ∷ TileRegion → Integer
regionTileCount (RegionRect x0 y0 x1 y1) =
    (toInteger x1 - toInteger x0 + 1) * (toInteger y1 - toInteger y0 + 1)
regionTileCount (RegionTiles ts) = toInteger (length ts)

-- | Clip a patch region to the map (D-21): the in-bounds part, if any,
--   and how many tiles were dropped.
clipRegion ∷ TileBounds → TileRegion → (Maybe TileRegion, Integer)
clipRegion b@(TileBounds bx0 by0 bx1 by1) r = case r of
    RegionRect x0 y0 x1 y1 →
        let cx0 = max x0 bx0; cy0 = max y0 by0
            cx1 = min x1 bx1; cy1 = min y1 by1
        in if cx0 > cx1 ∨ cy0 > cy1
               then (Nothing, regionTileCount r)
               else let kept = RegionRect cx0 cy0 cx1 cy1
                    in (Just kept, regionTileCount r - regionTileCount kept)
    RegionTiles ts →
        let kept = filter (inBounds b) ts
            dropped = toInteger (length ts - length kept)
        in (if null kept then Nothing else Just (RegionTiles kept), dropped)

-- | Is a whole footprint, anchored at @(x, y)@, inside the map? Buildings
--   and locations are indivisible (D-21).
footprintInside ∷ TileBounds → (Int, Int) → Footprint → Bool
footprintInside b (x, y) (Footprint dx0 dy0 dx1 dy1) =
    inBounds b (x + dx0, y + dy0) ∧ inBounds b (x + dx1, y + dy1)

-- | The tile an actor or ground item at a continuous position occupies:
--   tile centres sit on integers, so the tile is the nearest integer,
--   halves rounding up.
tileOf ∷ (Float, Float) → (Int, Int)
tileOf (x, y) = (floor (x + 0.5), floor (y + 0.5))
