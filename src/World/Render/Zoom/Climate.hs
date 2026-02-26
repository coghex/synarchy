{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Climate data → (R, G, B) colour mapping for the various
--   zoom-map overlay modes (temperature, pressure, humidity, etc.).
module World.Render.Zoom.Climate
    ( tempToColorAt
    , pressureToColorAt
    , humidityToColorAt
    , precipToColorAt
    , precipTypeToColorAt
    , evapToColorAt
    , seaTempToColorAt
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Graphics.Camera (CameraFacing(..))
import World.Grid (worldToGrid)
import World.Types (chunkSize)
import World.Weather.Types (ClimateCoord(..), climateRegionSize, RegionClimate(..)
                           , SeasonalClimate(..), OceanCell(..))

-----------------------------------------------------------
-- Shared Helper
-----------------------------------------------------------

-- | Convert screen-space coords to a climate-region (ru, rv) pair.
toClimateCoord ∷ CameraFacing → Int → Float → Float → ClimateCoord
toClimateCoord facing worldSize x y =
    let (gx, gy) = worldToGrid facing x y
        u = gx - gy
        v = gx + gy
        w = worldSize * chunkSize
        halfW = w `div` 2
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
        halfChunks = worldSize `div` 2
        chunkU = floorDiv wrappedU chunkSize
        chunkV = floorDiv v chunkSize
        ru = (chunkU + halfChunks) `div` climateRegionSize
        rv = (chunkV + halfChunks) `div` climateRegionSize
    in ClimateCoord ru rv
  where
    floorDiv a b
      | b > 0     = if a >= 0 then a `div` b else -(((-a) + b - 1) `div` b)
      | otherwise = error "floorDiv: non-positive divisor"

-----------------------------------------------------------
-- Per-Mode Color Functions
-----------------------------------------------------------

tempToColorAt ∷ CameraFacing → Int → Float → Float
              → HM.HashMap ClimateCoord RegionClimate
              → (Float, Float, Float)
tempToColorAt facing worldSize x y cg =
    case HM.lookup (toClimateCoord facing worldSize x y) cg of
        Just region → let t = clamp01 $ (scWinter (rcAirTemp region)) / 40.0
                       in (t, 0, 1 - t)
        Nothing     → (1.0, 1.0, 1.0)

pressureToColorAt ∷ CameraFacing → Int → Float → Float
              → HM.HashMap ClimateCoord RegionClimate
              → (Float, Float, Float)
pressureToColorAt facing worldSize x y cg =
    case HM.lookup (toClimateCoord facing worldSize x y) cg of
        Just region → let t = clamp01 ((rcPressure region - 0.9) / 0.2)
                       in (t, 0, 1 - t)
        Nothing     → (1.0, 1.0, 1.0)

humidityToColorAt ∷ CameraFacing → Int → Float → Float
              → HM.HashMap ClimateCoord RegionClimate
              → (Float, Float, Float)
humidityToColorAt facing worldSize x y cg =
    case HM.lookup (toClimateCoord facing worldSize x y) cg of
        Just region → let t = clamp01 (rcHumidity region)
                       in (t, 0, 1 - t)
        Nothing     → (1.0, 1.0, 1.0)

precipToColorAt ∷ CameraFacing → Int → Float → Float
              → HM.HashMap ClimateCoord RegionClimate
              → (Float, Float, Float)
precipToColorAt facing worldSize x y cg =
    case HM.lookup (toClimateCoord facing worldSize x y) cg of
        Just region → let t = clamp01 (scWinter (rcPrecipitation region))
                       in (0, 0, t)
        Nothing     → (1.0, 1.0, 1.0)

precipTypeToColorAt ∷ CameraFacing → Int → Float → Float
              → HM.HashMap ClimateCoord RegionClimate
              → (Float, Float, Float)
precipTypeToColorAt facing worldSize x y cg =
    case HM.lookup (toClimateCoord facing worldSize x y) cg of
        Just region → let t = 1 - (clamp01 (rcPrecipType region))
                       in (t, t, t)
        Nothing     → (1.0, 1.0, 1.0)

evapToColorAt ∷ CameraFacing → Int → Float → Float
              → HM.HashMap ClimateCoord RegionClimate
              → (Float, Float, Float)
evapToColorAt facing worldSize x y cg =
    case HM.lookup (toClimateCoord facing worldSize x y) cg of
        Just region → let t = clamp01 (rcEvaporation region)
                       in (t, t, t)
        Nothing     → (1.0, 1.0, 1.0)

seaTempToColorAt ∷ CameraFacing → Int → Float → Float
              → HM.HashMap ClimateCoord OceanCell
              → (Float, Float, Float)
seaTempToColorAt facing worldSize x y oc =
    case HM.lookup (toClimateCoord facing worldSize x y) oc of
        Just region → let t = clamp01 $ (scWinter (ocTemperature region)) / 30.0
                       in (t, 0, 1 - t)
        Nothing     → (1.0, 1.0, 1.0)
