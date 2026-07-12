{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Geology.Timeline.Feature
    ( FeatureShape(..)
    , featureShapeTag
    , FeatureActivity(..)
    , FeatureEvolution(..)
    , CraterParams(..)
    , VolcanicFeature(..)
    , ShieldParams(..)
    , CinderConeParams(..)
    , LavaDomeParams(..)
    , CalderaParams(..)
    , FissureParams(..)
    , LavaTubeParams(..)
    , SuperVolcanoParams(..)
    , HydrothermalParams(..)
    , LandslideParams(..)
    , FloodParams(..)
    , ErosionParams(..)
    , defaultErosionParams
    , VolcanicActivity(..)
    , PersistentFeature(..)
    ) where

import UPrelude hiding (get)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize(..))
import Data.Hashable (Hashable)
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Hydrology.Types (HydroFeature(..))

data FeatureShape
    = VolcanicShape !VolcanicFeature
    | HydroShape   !HydroFeature
    deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

-- | Compact constructor tag for a FeatureShape — the constructor name
--   only, without the payload. Use this for diagnostics/abort messages:
--   the derived Show of a RiverFeature drags in the full rpSegments
--   geometry, which would dump an entire river into a crash log.
featureShapeTag ∷ FeatureShape → String
featureShapeTag (VolcanicShape v) = case v of
    ShieldVolcano{}    → "VolcanicShape ShieldVolcano"
    CinderCone{}       → "VolcanicShape CinderCone"
    LavaDome{}         → "VolcanicShape LavaDome"
    Caldera{}          → "VolcanicShape Caldera"
    FissureVolcano{}   → "VolcanicShape FissureVolcano"
    LavaTube{}         → "VolcanicShape LavaTube"
    SuperVolcano{}     → "VolcanicShape SuperVolcano"
    HydrothermalVent{} → "VolcanicShape HydrothermalVent"
featureShapeTag (HydroShape h) = case h of
    RiverFeature{}   → "HydroShape RiverFeature"
    GlacierFeature{} → "HydroShape GlacierFeature"
    LakeFeature{}    → "HydroShape LakeFeature"

data FeatureActivity
    = FActive
    | FDormant
    | FExtinct
    | FCollapsed
    deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data FeatureEvolution
    = Reactivate
        { feHeightGain    ∷ !Int
        , feLavaExtension ∷ !Int
        , feCenter        ∷ !GeoCoord
        , feRadius        ∷ !Int
        , feCenterElev    ∷ !Int
        }
    | GoDormant
        { feCenter        ∷ !GeoCoord
        , feRadius        ∷ !Int
        }
    | GoExtinct
        { feCenter        ∷ !GeoCoord
        , feRadius        ∷ !Int
        }
    | CollapseToCaldera
        { feCollapseDepth ∷ !Int
        , feCollapseRatio ∷ !Float
        , feCenter        ∷ !GeoCoord
        , feRadius        ∷ !Int
        , feCenterElev    ∷ !Int
        }
    | ParasiticEruption
        { feChildFeature  ∷ !VolcanicFeature
        , feChildId       ∷ !GeoFeatureId
        , feCenter        ∷ !GeoCoord
        , feRadius        ∷ !Int
        }
    | FlankCollapse
        { feCollapseAngle ∷ !Float
        , feCollapseWidth ∷ !Float
        , feDebrisRadius  ∷ !Int
        , feCenter        ∷ !GeoCoord
        , feRadius        ∷ !Int
        }
    deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data CraterParams = CraterParams
    { cpCenter     ∷ !GeoCoord
    , cpRadius     ∷ !Int
    , cpDepth      ∷ !Int
    , cpRimHeight  ∷ !Int
    , cpEjectaRadius ∷ !Int
    , cpMeteorite  ∷ !(Maybe Word8)
    , cpCenterElev ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data VolcanicFeature
    = ShieldVolcano    !ShieldParams
    | CinderCone       !CinderConeParams
    | LavaDome         !LavaDomeParams
    | Caldera          !CalderaParams
    | FissureVolcano   !FissureParams
    | LavaTube         !LavaTubeParams
    | SuperVolcano     !SuperVolcanoParams
    | HydrothermalVent !HydrothermalParams
    deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data ShieldParams = ShieldParams
    { shCenter     ∷ !GeoCoord
    , shBaseRadius ∷ !Int
    , shPeakHeight ∷ !Int
    , shSummitPit  ∷ !Bool
    , shPitRadius  ∷ !Int
    , shPitDepth   ∷ !Int
    , shCenterElev ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data CinderConeParams = CinderConeParams
    { ccCenter     ∷ !GeoCoord
    , ccBaseRadius ∷ !Int
    , ccPeakHeight ∷ !Int
    , ccCraterRadius ∷ !Int
    , ccCraterDepth  ∷ !Int
    , ccCenterElev   ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data LavaDomeParams = LavaDomeParams
    { ldCenter     ∷ !GeoCoord
    , ldBaseRadius ∷ !Int
    , ldHeight     ∷ !Int
    , ldCenterElev ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data CalderaParams = CalderaParams
    { caCenter     ∷ !GeoCoord
    , caOuterRadius ∷ !Int
    , caInnerRadius ∷ !Int
    , caRimHeight   ∷ !Int
    , caFloorDepth  ∷ !Int
    , caHasLake     ∷ !Bool
    , caCenterElev  ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data FissureParams = FissureParams
    { fpStart      ∷ !GeoCoord
    , fpEnd        ∷ !GeoCoord
    , fpWidth      ∷ !Int
    , fpRidgeHeight ∷ !Int
    , fpHasMagma   ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data LavaTubeParams = LavaTubeParams
    { ltStart      ∷ !GeoCoord
    , ltEnd        ∷ !GeoCoord
    , ltWidth      ∷ !Int
    , ltRidgeHeight ∷ !Int
    , ltCollapses  ∷ !Int
    , ltCollapseSeed ∷ !Word64
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data SuperVolcanoParams = SuperVolcanoParams
    { svCenter      ∷ !GeoCoord
    , svCalderaRadius ∷ !Int
    , svRimHeight    ∷ !Int
    , svFloorDepth   ∷ !Int
    , svEjectaRadius ∷ !Int
    , svEjectaDepth  ∷ !Int
    , svCenterElev   ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data HydrothermalParams = HydrothermalParams
    { htCenter     ∷ !GeoCoord
    , htRadius     ∷ !Int
    , htChimneyHeight ∷ !Int
    , htCenterElev   ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data LandslideParams = LandslideParams
    { lsCenter     ∷ !GeoCoord
    , lsRadius     ∷ !Int
    , lsDirection  ∷ !Float
    , lsVolume     ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data FloodParams = FloodParams
    { fpCenter     ∷ !GeoCoord
    , fpRadius     ∷ !Int
    , fpDepositDepth ∷ !Int
    , fpMaterial   ∷ !Word8
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data ErosionParams = ErosionParams
    { epIntensity    ∷ !Float
    , epHydraulic    ∷ !Float
    , epThermal      ∷ !Float
    , epWind         ∷ !Float
    , epChemical     ∷ !Float
    , epSeed         ∷ !Word64
    , epTemperature  ∷ !Float
    , epPrecipitation ∷ !Float
    , epHumidity     ∷ !Float
    , epSnowFraction ∷ !Float
    , epIsLastAge    ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

defaultErosionParams ∷ ErosionParams
defaultErosionParams = ErosionParams
    { epIntensity  = 0.5
    , epHydraulic  = 0.7
    , epThermal    = 0.3
    , epWind       = 0.1
    , epChemical   = 0.2
    , epSeed       = 0
    , epTemperature = 15.0
    , epPrecipitation = 0.5
    , epHumidity   = 0.5
    , epSnowFraction = 0.0
    , epIsLastAge  = False
    }

data VolcanicActivity
    = Active
    | Dormant
    | Extinct
    | Collapsed
    deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

data PersistentFeature = PersistentFeature
    { pfId            ∷ !GeoFeatureId
    , pfFeature       ∷ !FeatureShape
    , pfActivity      ∷ !FeatureActivity
    , pfFormationPeriod ∷ !Int
    , pfLastActivePeriod ∷ !Int
    , pfEruptionCount ∷ !Int
    , pfParentId      ∷ !(Maybe GeoFeatureId)
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)
