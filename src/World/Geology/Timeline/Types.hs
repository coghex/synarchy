{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.Types
    ( GeoScale(..)
    , GeoPeriod(..)
    , GeoTimeline(..)
    , emptyTimeline
    , EventBBox(..)
    , noBBox
    , eventBBox
    , featureShapeBBox
    , volcanicFeatureBBox
    , glacierBBox
    , hydroFeatureBBox
    , hydroEvolutionBBox
    , bboxOverlapsChunk
    , tagEventsWithBBox
    , GeoEvent(..)
    , FeatureShape(..)
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
    , LavaFlow(..)
    ) where

import UPrelude
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Hydrology.Types
    ( HydroFeature(..)
    , HydroEvolution(..)
    , GlacierParams(..)
    , RiverParams(..)
    , RiverSegment(..)
    , LakeParams(..)
    )

data GeoScale
    = Eon       -- ^ Billions of years — major crustal formation
    | Era       -- ^ Hundreds of millions — large-scale events
    | Period    -- ^ Tens of millions — mountain building, rifting
    | Epoch     -- ^ Millions — climate shifts, glaciation
    | Age       -- ^ Hundreds of thousands — local events, erosion detail
    deriving (Show, Eq, Ord)

data GeoPeriod = GeoPeriod
    { gpName       ∷ !Text
    , gpScale      ∷ !GeoScale
    , gpDuration   ∷ !Int          -- ^ Relative duration (arbitrary units)
    , gpDate       ∷ !Float        -- ^ Date of period start
    , gpEvents     ∷ ![GeoEvent]
    , gpErosion    ∷ !ErosionParams
    , gpTaggedEvents ∷ ![(GeoEvent, EventBBox)]
    } deriving (Show, Eq)

data GeoTimeline = GeoTimeline
    { gtSeed       ∷ !Word64
    , gtWorldSize  ∷ !Int
    , gtPeriods    ∷ ![GeoPeriod]
    , gtFeatures   ∷ ![PersistentFeature]
    } deriving (Show, Eq)

emptyTimeline ∷ GeoTimeline
emptyTimeline = GeoTimeline
    { gtSeed = 0
    , gtWorldSize = 128
    , gtPeriods = []
    , gtFeatures = []
    }

data EventBBox = EventBBox
    { bbMinX ∷ !Int
    , bbMinY ∷ !Int
    , bbMaxX ∷ !Int
    , bbMaxY ∷ !Int
    } deriving (Show, Eq)

noBBox ∷ EventBBox
noBBox = EventBBox minBound minBound maxBound maxBound

eventBBox ∷ GeoEvent → Int → EventBBox
eventBBox (CraterEvent cp) _ws =
    let GeoCoord cx cy = cpCenter cp
        r = cpRadius cp + cpEjectaRadius cp
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
eventBBox (VolcanicEvent shape) ws = featureShapeBBox shape ws
eventBBox (VolcanicModify _fid evo) _ws =
    let GeoCoord cx cy = feCenter evo
        r = case evo of
                FlankCollapse { feDebrisRadius = dr } → max (feRadius evo) dr
                _                                     → feRadius evo
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
eventBBox (EruptionEvent _ flow) _ws =
    let r = lfRadius flow
    in EventBBox (lfSourceX flow - r) (lfSourceY flow - r)
                 (lfSourceX flow + r) (lfSourceY flow + r)
eventBBox (LandslideEvent ls) _ws =
    let GeoCoord cx cy = lsCenter ls
        r = lsRadius ls
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
eventBBox (FloodEvent fp) _ws =
    let GeoCoord cx cy = fpCenter fp
        r = fpRadius fp
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
eventBBox (GlaciationEvent glacier) _ws =
    glacierBBox glacier
eventBBox (HydroEvent hf) ws = hydroFeatureBBox hf ws
eventBBox (HydroModify _fid evo) _ws =
    hydroEvolutionBBox evo

featureShapeBBox ∷ FeatureShape → Int → EventBBox
featureShapeBBox (VolcanicShape vf) ws = volcanicFeatureBBox vf ws
featureShapeBBox (HydroShape hf) ws   = hydroFeatureBBox hf ws

volcanicFeatureBBox ∷ VolcanicFeature → Int → EventBBox
volcanicFeatureBBox (ShieldVolcano p) _ws =
    let GeoCoord cx cy = shCenter p
        r = shBaseRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (CinderCone p) _ws =
    let GeoCoord cx cy = ccCenter p
        r = ccBaseRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (LavaDome p) _ws =
    let GeoCoord cx cy = ldCenter p
        r = ldBaseRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (Caldera p) _ws =
    let GeoCoord cx cy = caCenter p
        r = caOuterRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (SuperVolcano p) _ws =
    let GeoCoord cx cy = svCenter p
        r = svEjectaRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (HydrothermalVent p) _ws =
    let GeoCoord cx cy = htCenter p
        r = htRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (FissureVolcano p) _ws =
    let GeoCoord sx sy = fpStart p
        GeoCoord ex ey = fpEnd p
        w = fpWidth p
    in EventBBox (min sx ex - w) (min sy ey - w)
                 (max sx ex + w) (max sy ey + w)
volcanicFeatureBBox (LavaTube p) _ws =
    let GeoCoord sx sy = ltStart p
        GeoCoord ex ey = ltEnd p
        w = ltWidth p
    in EventBBox (min sx ex - w) (min sy ey - w)
                 (max sx ex + w) (max sy ey + w)

glacierBBox ∷ GlacierParams → EventBBox
glacierBBox glacier =
    let GeoCoord cx cy = glCenter glacier
        len = glLength glacier
        w = glWidth glacier
        moraine = glMoraineSize glacier
        r = len + moraine + w
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)

hydroFeatureBBox ∷ HydroFeature → Int → EventBBox
hydroFeatureBBox (RiverFeature river) _ws =
    let allCoords = rpSourceRegion river : rpMouthRegion river
                  : concatMap (\seg → [rsStart seg, rsEnd seg]) (rpSegments river)
        xs = map (\(GeoCoord x _) → x) allCoords
        ys = map (\(GeoCoord _ y) → y) allCoords
        maxValley = case rpSegments river of
            []   → 8
            segs → maximum (map rsValleyWidth segs)
        pad = maxValley
    in EventBBox (minimum xs - pad) (minimum ys - pad)
                 (maximum xs + pad) (maximum ys + pad)
hydroFeatureBBox (GlacierFeature glacier) _ws =
    glacierBBox glacier
hydroFeatureBBox (LakeFeature lake) _ws =
    let GeoCoord cx cy = lkCenter lake
        r = lkRadius lake
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)

hydroEvolutionBBox ∷ HydroEvolution → EventBBox
hydroEvolutionBBox (RiverBranch branchPt _angle len _childId) =
    let GeoCoord bx by = branchPt
    in EventBBox (bx - len) (by - len) (bx + len) (by + len)
hydroEvolutionBBox (RiverMeander _ _) =
    noBBox
hydroEvolutionBBox (RiverCapture _capturedId capturePoint) =
    let GeoCoord cx cy = capturePoint
        r = 30
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
hydroEvolutionBBox (RiverDam damPt _lakeId damH) =
    let GeoCoord dx dy = damPt
        r = max 10 damH
    in EventBBox (dx - r) (dy - r) (dx + r) (dy + r)
hydroEvolutionBBox RiverDryUp =
    noBBox
hydroEvolutionBBox (GlacierAdvance _advLen _advWid) =
    noBBox
hydroEvolutionBBox (GlacierRetreat _retreatLen _moraineDep) =
    noBBox
hydroEvolutionBBox (GlacierMelt _moraineDep) =
    noBBox
hydroEvolutionBBox (GlacierBranch branchPt _angle len _childId) =
    let GeoCoord bx by = branchPt
    in EventBBox (bx - len) (by - len) (bx + len) (by + len)
hydroEvolutionBBox (LakeDrain _) =
    noBBox
hydroEvolutionBBox (LakeExpand newRadius _) =
    noBBox

bboxOverlapsChunk ∷ Int → EventBBox → Int → Int → Int → Int → Bool
bboxOverlapsChunk _worldSize bb cMinX cMinY cMaxX cMaxY =
    not (bbMaxX bb < cMinX ∨ bbMinX bb > cMaxX
       ∨ bbMaxY bb < cMinY ∨ bbMinY bb > cMaxY)

tagEventsWithBBox ∷ Int → [GeoEvent] → [(GeoEvent, EventBBox)]
tagEventsWithBBox worldSize events =
    map (\evt → (evt, eventBBox evt worldSize)) events

data GeoEvent
    = CraterEvent !CraterParams
    | VolcanicEvent !FeatureShape
    | VolcanicModify !GeoFeatureId !FeatureEvolution
    | EruptionEvent !GeoFeatureId !LavaFlow
    | LandslideEvent !LandslideParams
    | GlaciationEvent !GlacierParams
    | FloodEvent !FloodParams
    | HydroEvent !HydroFeature
    | HydroModify !GeoFeatureId !HydroEvolution
    deriving (Show, Eq)

data FeatureShape
    = VolcanicShape !VolcanicFeature
    | HydroShape   !HydroFeature
    deriving (Show, Eq)

data FeatureActivity
    = FActive
    | FDormant
    | FExtinct
    | FCollapsed
    deriving (Show, Eq)

data FeatureEvolution
    = Reactivate
        { feHeightGain    ∷ !Int
        , feLavaExtension ∷ !Int
        , feCenter        ∷ !GeoCoord
        , feRadius        ∷ !Int
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
    deriving (Show, Eq)

data CraterParams = CraterParams
    { cpCenter     ∷ !GeoCoord
    , cpRadius     ∷ !Int
    , cpDepth      ∷ !Int
    , cpRimHeight  ∷ !Int
    , cpEjectaRadius ∷ !Int
    , cpMeteorite  ∷ !(Maybe Word8)
    } deriving (Show, Eq)

data VolcanicFeature
    = ShieldVolcano    !ShieldParams
    | CinderCone       !CinderConeParams
    | LavaDome         !LavaDomeParams
    | Caldera          !CalderaParams
    | FissureVolcano   !FissureParams
    | LavaTube         !LavaTubeParams
    | SuperVolcano     !SuperVolcanoParams
    | HydrothermalVent !HydrothermalParams
    deriving (Show, Eq)

data ShieldParams = ShieldParams
    { shCenter     ∷ !GeoCoord
    , shBaseRadius ∷ !Int
    , shPeakHeight ∷ !Int
    , shSummitPit  ∷ !Bool
    , shPitRadius  ∷ !Int
    , shPitDepth   ∷ !Int
    } deriving (Show, Eq)

data CinderConeParams = CinderConeParams
    { ccCenter     ∷ !GeoCoord
    , ccBaseRadius ∷ !Int
    , ccPeakHeight ∷ !Int
    , ccCraterRadius ∷ !Int
    , ccCraterDepth  ∷ !Int
    } deriving (Show, Eq)

data LavaDomeParams = LavaDomeParams
    { ldCenter     ∷ !GeoCoord
    , ldBaseRadius ∷ !Int
    , ldHeight     ∷ !Int
    } deriving (Show, Eq)

data CalderaParams = CalderaParams
    { caCenter     ∷ !GeoCoord
    , caOuterRadius ∷ !Int
    , caInnerRadius ∷ !Int
    , caRimHeight   ∷ !Int
    , caFloorDepth  ∷ !Int
    , caHasLake     ∷ !Bool
    } deriving (Show, Eq)

data FissureParams = FissureParams
    { fpStart      ∷ !GeoCoord
    , fpEnd        ∷ !GeoCoord
    , fpWidth      ∷ !Int
    , fpRidgeHeight ∷ !Int
    , fpHasMagma   ∷ !Bool
    } deriving (Show, Eq)

data LavaTubeParams = LavaTubeParams
    { ltStart      ∷ !GeoCoord
    , ltEnd        ∷ !GeoCoord
    , ltWidth      ∷ !Int
    , ltRidgeHeight ∷ !Int
    , ltCollapses  ∷ !Int
    , ltCollapseSeed ∷ !Word64
    } deriving (Show, Eq)

data SuperVolcanoParams = SuperVolcanoParams
    { svCenter      ∷ !GeoCoord
    , svCalderaRadius ∷ !Int
    , svRimHeight    ∷ !Int
    , svFloorDepth   ∷ !Int
    , svEjectaRadius ∷ !Int
    , svEjectaDepth  ∷ !Int
    } deriving (Show, Eq)

data HydrothermalParams = HydrothermalParams
    { htCenter     ∷ !GeoCoord
    , htRadius     ∷ !Int
    , htChimneyHeight ∷ !Int
    } deriving (Show, Eq)

data LandslideParams = LandslideParams
    { lsCenter     ∷ !GeoCoord
    , lsRadius     ∷ !Int
    , lsDirection  ∷ !Float
    , lsVolume     ∷ !Int
    } deriving (Show, Eq)

data FloodParams = FloodParams
    { fpCenter     ∷ !GeoCoord
    , fpRadius     ∷ !Int
    , fpDepositDepth ∷ !Int
    , fpMaterial   ∷ !Word8
    } deriving (Show, Eq)

data ErosionParams = ErosionParams
    { epIntensity    ∷ !Float
    , epHydraulic    ∷ !Float
    , epThermal      ∷ !Float
    , epWind         ∷ !Float
    , epChemical     ∷ !Float
    , epSeed         ∷ !Word64
    } deriving (Show, Eq)

defaultErosionParams ∷ ErosionParams
defaultErosionParams = ErosionParams
    { epIntensity  = 0.5
    , epHydraulic  = 0.7
    , epThermal    = 0.3
    , epWind       = 0.1
    , epChemical   = 0.2
    , epSeed       = 0
    }

data VolcanicActivity
    = Active
    | Dormant
    | Extinct
    | Collapsed
    deriving (Show, Eq)

data PersistentFeature = PersistentFeature
    { pfId            ∷ !GeoFeatureId
    , pfFeature       ∷ !FeatureShape
    , pfActivity      ∷ !FeatureActivity
    , pfFormationPeriod ∷ !Int
    , pfLastActivePeriod ∷ !Int
    , pfEruptionCount ∷ !Int
    , pfParentId      ∷ !(Maybe GeoFeatureId)
    } deriving (Show, Eq)

data LavaFlow = LavaFlow
    { lfSourceX   ∷ !Int
    , lfSourceY   ∷ !Int
    , lfRadius    ∷ !Int
    , lfElevation ∷ !Int
    , lfVolume    ∷ !Int
    , lfMaterial  ∷ !Word8
    , lfViscosity ∷ !Int
    } deriving (Show, Eq)
