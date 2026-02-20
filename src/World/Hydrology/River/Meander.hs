{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River.Meander
    ( meanderSegments
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.Vector as V
import World.Base (GeoCoord(..))
import World.Hydrology.Types
import World.Geology.Hash

-----------------------------------------------------------
-- Meander Application
-----------------------------------------------------------

-- | Shift segment endpoints perpendicular to flow direction.
--   Creates natural-looking bends in the river over time.
--   Each segment shifts independently using hash-based randomness
--   so the result is deterministic.
meanderSegments ∷ Word64 → Int → Float → V.Vector RiverSegment → V.Vector RiverSegment
meanderSegments seed fidInt amount segs =
    V.imap (\segIdx seg → meanderOne seed fidInt amount segIdx seg) segs

meanderOne ∷ Word64 → Int → Float → Int → RiverSegment → RiverSegment
meanderOne seed fidInt amount segIdx seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        -- Direction of flow
        dx = fromIntegral (ex - sx) ∷ Float
        dy = fromIntegral (ey - sy) ∷ Float
        len = sqrt (dx * dx + dy * dy)
    in if len < 0.001 then seg
    else
    let -- Perpendicular direction
        perpX = -dy / len
        perpY = dx / len
        -- Hash determines shift direction and magnitude
        h1 = hashGeo seed (fidInt + segIdx) 850
        h2 = hashGeo seed (fidInt + segIdx) 851
        shiftStart = (hashToFloatGeo h1 - 0.5) * 2.0 * amount * len * 0.2
        shiftEnd   = (hashToFloatGeo h2 - 0.5) * 2.0 * amount * len * 0.2
        -- Shift the start point (but keep the very first segment source fixed)
        newSX = sx + round (perpX * shiftStart)
        newSY = sy + round (perpY * shiftStart)
        newEX = ex + round (perpX * shiftEnd)
        newEY = ey + round (perpY * shiftEnd)
    in seg { rsStart = GeoCoord newSX newSY
           , rsEnd   = GeoCoord newEX newEY
           }
