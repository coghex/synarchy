{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Save.Types
    ( SaveData(..)
    , SaveMetadata(..)
    ) where

import UPrelude
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import World.Generate.Types (WorldGenParams(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import World.Region.Types (RegionCoord(..))
import World.Weather.Types (RegionClimate(..))
import World.Base (GeoFeatureId(..))

-- | Human-readable metadata for save listing
data SaveMetadata = SaveMetadata
    { smName       ∷ !Text
    , smSeed       ∷ !Word64
    , smWorldSize  ∷ !Int
    , smPlateCount ∷ !Int
    , smTimestamp  ∷ !Text        -- ^ ISO 8601 string
    } deriving (Show, Eq, Serialize, Generic)

-- | Everything needed to reconstruct a WorldState
data SaveData = SaveData
    { sdMetadata   ∷ !SaveMetadata
    , sdGenParams  ∷ !WorldGenParams
    , sdCameraX    ∷ !Float
    , sdCameraY    ∷ !Float
    , sdTimeHour   ∷ !Int
    , sdTimeMinute ∷ !Int
    , sdDateYear   ∷ !Int
    , sdDateMonth  ∷ !Int
    , sdDateDay    ∷ !Int
    , sdTimeScale  ∷ !Float
    , sdMapMode    ∷ !ZoomMapMode
    , sdToolMode   ∷ !ToolMode
    , sdClimate    ∷ !(HM.HashMap RegionCoord RegionClimate)
    , sdRiverFlow  ∷ !(HM.HashMap GeoFeatureId Float)
    } deriving (Show, Serialize, Generic)
