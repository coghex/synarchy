{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
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
    , tileInBBoxWrapped
    , bboxOverlapsChunk
    , tagEventsWithBBox
    , GeoEvent(..)
    , RiverSegmentCarve(..)
    , RiverDeltaParams(..)
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
    , explodeRiverEvent
    ) where

import UPrelude hiding (get)
import GHC.Generics (Generic)
import Data.Serialize (Serialize(..))
import Data.Hashable (Hashable)
import qualified Data.Vector as V
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
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
    deriving (Show, Eq, Ord, Generic, Serialize)

data GeoPeriod = GeoPeriod
    { gpName       ∷ !Text
    , gpScale      ∷ !GeoScale
    , gpDuration   ∷ !Int          -- ^ Relative duration (arbitrary units)
    , gpDate       ∷ !Float        -- ^ Date of period start
    , gpEvents     ∷ ![GeoEvent]
    , gpErosion    ∷ !ErosionParams
    , gpTaggedEvents ∷ ![(GeoEvent, EventBBox)]
    , gpExplodedEvents ∷ !(V.Vector (GeoEvent, EventBBox))
    , gpPeriodBBox    ∷ !EventBBox      -- ^ Bounding box of all events in this period
    } deriving (Show, Eq, Generic, Serialize)

data GeoTimeline = GeoTimeline
    { gtSeed       ∷ !Word64
    , gtWorldSize  ∷ !Int
    , gtPeriods    ∷ ![GeoPeriod]
    , gtFeatures   ∷ ![PersistentFeature]
    } deriving (Show, Eq, Generic, Serialize)

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
    } deriving (Show, Eq, Generic, Serialize, Hashable)

noBBox ∷ EventBBox
noBBox = EventBBox minBound minBound maxBound maxBound

-----------------------------------------------------------
-- Per-segment river carving event data
-----------------------------------------------------------

-- | A single river segment's carving parameters.
--   Replaces the old whole-river HydroEvent (RiverFeature river)
--   at the event level. Each segment gets its own tight bbox.
data RiverSegmentCarve = RiverSegmentCarve
    { rscSegment     ∷ !RiverSegment
    , rscMeanderSeed ∷ !Word64
    } deriving (Show, Eq, Generic, Serialize, Hashable)

-- | Delta deposit at the river mouth. Extracted from the last
--   segment so it gets its own small bbox around the mouth.
data RiverDeltaParams = RiverDeltaParams
    { rdpLastSegment ∷ !RiverSegment  -- ^ Last segment (for mouth position + direction)
    , rdpFlowRate    ∷ !Float         -- ^ Total river flow (drives delta size)
    } deriving (Show, Eq, Generic, Serialize, Hashable)

-----------------------------------------------------------
-- GeoEvent — now with per-segment river events
-----------------------------------------------------------

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
    | RiverSegmentEvent !RiverSegmentCarve
    | RiverDeltaEvent   !RiverDeltaParams
    deriving (Show, Eq, Generic, Serialize, Hashable)

-----------------------------------------------------------
-- Explode a river HydroEvent into per-segment events
-----------------------------------------------------------

-- | Split a single HydroEvent (RiverFeature river) into N+1 events:
--   one RiverSegmentEvent per segment, plus one RiverDeltaEvent
--   for the mouth deposit. Non-river HydroEvents pass through unchanged.
explodeRiverEvent ∷ GeoEvent → [GeoEvent]
explodeRiverEvent (HydroEvent (RiverFeature river)) =
    let segs   = rpSegments river
        mSeed  = rpMeanderSeed river
        segEvts = V.toList $ V.map (\seg → RiverSegmentEvent
                        (RiverSegmentCarve seg mSeed)) segs
        deltaEvt = if V.null segs
            then []
            else [RiverDeltaEvent (RiverDeltaParams (V.last segs)
                                                     (rpFlowRate river))]
    in segEvts ++ deltaEvt
explodeRiverEvent evt = [evt]

-----------------------------------------------------------
-- Bounding boxes
-----------------------------------------------------------

-- | Wrap a GeoCoord into canonical u-space.
--   Duplicates the logic of wrapGlobalU to avoid circular imports.
{-# INLINE wrapCoordU #-}
wrapCoordU ∷ Int → Int → Int → (Int, Int)
wrapCoordU worldSize gx gy =
    let w = worldSize * 16  -- worldWidthTiles
        halfW = w `div` 2
        u = gx - gy
        v = gx + gy
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
    in ((wrappedU + v) `div` 2, (v - wrappedU) `div` 2)

eventBBox ∷ GeoEvent → Int → EventBBox
eventBBox (CraterEvent cp) ws =                                    -- was _ws
    let GeoCoord cx0 cy0 = cpCenter cp
        (cx, cy) = wrapCoordU ws cx0 cy0                          -- ADDED
        r = cpRadius cp + cpEjectaRadius cp
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
eventBBox (VolcanicEvent shape) ws = featureShapeBBox shape ws
eventBBox (VolcanicModify _fid evo) ws =                           -- was _ws
    let GeoCoord cx0 cy0 = feCenter evo
        (cx, cy) = wrapCoordU ws cx0 cy0                          -- ADDED
        r = case evo of
                FlankCollapse { feDebrisRadius = dr } → max (feRadius evo) dr
                _                                     → feRadius evo
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
eventBBox (EruptionEvent _ flow) ws =                              -- was _ws
    let (sx, sy) = wrapCoordU ws (lfSourceX flow) (lfSourceY flow) -- ADDED
        r = lfRadius flow
    in EventBBox (sx - r) (sy - r) (sx + r) (sy + r)
eventBBox (LandslideEvent ls) ws =                                 -- was _ws
    let GeoCoord cx0 cy0 = lsCenter ls
        (cx, cy) = wrapCoordU ws cx0 cy0                          -- ADDED
        r = lsRadius ls
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
eventBBox (FloodEvent fp) ws =                                     -- was _ws
    let GeoCoord cx0 cy0 = World.Geology.Timeline.Types.fpCenter fp
        (cx, cy) = wrapCoordU ws cx0 cy0                          -- ADDED
        r = World.Geology.Timeline.Types.fpRadius fp
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
eventBBox (GlaciationEvent glacier) ws =                           -- was _ws
    glacierBBoxW ws glacier                                        -- CHANGED
eventBBox (HydroEvent hf) ws = hydroFeatureBBox hf ws
eventBBox (HydroModify _fid evo) ws =                              -- was _ws
    hydroEvolutionBBoxW ws evo                                     -- CHANGED

-- tight bbox around just this one segment
eventBBox (RiverSegmentEvent rsc) ws =                             -- was _ws
    let seg = rscSegment rsc
        GeoCoord sx0 sy0 = rsStart seg
        GeoCoord ex0 ey0 = rsEnd seg
        (sx, sy) = wrapCoordU ws sx0 sy0                          -- ADDED
        (ex, ey) = wrapCoordU ws ex0 ey0                          -- ADDED
        pad = rsValleyWidth seg
    in EventBBox (min sx ex - pad) (min sy ey - pad)
                 (max sx ex + pad) (max sy ey + pad)

-- tight bbox around the river mouth delta
eventBBox (RiverDeltaEvent rdp) ws =                               -- was _ws
    let seg = rdpLastSegment rdp
        GeoCoord mx0 my0 = rsEnd seg
        (mx, my) = wrapCoordU ws mx0 my0                          -- ADDED
        totalFlow = rdpFlowRate rdp
        deltaRadius = round (totalFlow * 25.0 + 8.0) ∷ Int
    in EventBBox (mx - deltaRadius) (my - deltaRadius)
                 (mx + deltaRadius) (my + deltaRadius)

featureShapeBBox ∷ FeatureShape → Int → EventBBox
featureShapeBBox (VolcanicShape vf) ws = volcanicFeatureBBox vf ws
featureShapeBBox (HydroShape hf) ws   = hydroFeatureBBox hf ws

volcanicFeatureBBox ∷ VolcanicFeature → Int → EventBBox
volcanicFeatureBBox (ShieldVolcano p) ws =                         -- was _ws
    let GeoCoord cx0 cy0 = shCenter p
        (cx, cy) = wrapCoordU ws cx0 cy0                          -- ADDED
        r = shBaseRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (CinderCone p) ws =                            -- was _ws
    let GeoCoord cx0 cy0 = ccCenter p
        (cx, cy) = wrapCoordU ws cx0 cy0                          -- ADDED
        r = ccBaseRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (LavaDome p) ws =                              -- was _ws
    let GeoCoord cx0 cy0 = ldCenter p
        (cx, cy) = wrapCoordU ws cx0 cy0                          -- ADDED
        r = ldBaseRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (Caldera p) ws =                               -- was _ws
    let GeoCoord cx0 cy0 = caCenter p
        (cx, cy) = wrapCoordU ws cx0 cy0                          -- ADDED
        r = caOuterRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (SuperVolcano p) ws =                          -- was _ws
    let GeoCoord cx0 cy0 = svCenter p
        (cx, cy) = wrapCoordU ws cx0 cy0                          -- ADDED
        r = svEjectaRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (HydrothermalVent p) ws =                      -- was _ws
    let GeoCoord cx0 cy0 = htCenter p
        (cx, cy) = wrapCoordU ws cx0 cy0                          -- ADDED
        r = htRadius p
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
volcanicFeatureBBox (FissureVolcano p) ws =                        -- was _ws
    let GeoCoord sx0 sy0 = fpStart p
        GeoCoord ex0 ey0 = fpEnd p
        (sx, sy) = wrapCoordU ws sx0 sy0                          -- ADDED
        (ex, ey) = wrapCoordU ws ex0 ey0                          -- ADDED
        w = World.Geology.Timeline.Types.fpWidth p
    in EventBBox (min sx ex - w) (min sy ey - w)
                 (max sx ex + w) (max sy ey + w)
volcanicFeatureBBox (LavaTube p) ws =                              -- was _ws
    let GeoCoord sx0 sy0 = ltStart p
        GeoCoord ex0 ey0 = ltEnd p
        (sx, sy) = wrapCoordU ws sx0 sy0                          -- ADDED
        (ex, ey) = wrapCoordU ws ex0 ey0                          -- ADDED
        w = ltWidth p
    in EventBBox (min sx ex - w) (min sy ey - w)
                 (max sx ex + w) (max sy ey + w)

-- | Glacier bbox, now with wrapping
glacierBBoxW ∷ Int → GlacierParams → EventBBox
glacierBBoxW ws glacier =
    let GeoCoord cx0 cy0 = glCenter glacier
        (cx, cy) = wrapCoordU ws cx0 cy0                          -- ADDED
        len = glLength glacier
        w = glWidth glacier
        moraine = glMoraineSize glacier
        r = len + moraine + w
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)

-- | Keep the old glacierBBox for callers that don't have worldSize
glacierBBox ∷ GlacierParams → EventBBox
glacierBBox glacier =
    let GeoCoord cx cy = glCenter glacier
        len = glLength glacier
        w = glWidth glacier
        moraine = glMoraineSize glacier
        r = len + moraine + w
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)

hydroFeatureBBox ∷ HydroFeature → Int → EventBBox
hydroFeatureBBox (RiverFeature river) ws =
    let wrapC (GeoCoord x y) = wrapCoordU ws x y
        (srcX, srcY) = wrapC (rpSourceRegion river)
        (mthX, mthY) = wrapC (rpMouthRegion river)
        segBounds = V.foldl' (\(xlo, ylo, xhi, yhi) seg →
            let (sx, sy) = wrapC (rsStart seg)
                (ex, ey) = wrapC (rsEnd seg)
            in ( min xlo (min sx ex)
               , min ylo (min sy ey)
               , max xhi (max sx ex)
               , max yhi (max sy ey)
               )
            ) (srcX, srcY, srcX, srcY) (rpSegments river)
        (xlo0, ylo0, xhi0, yhi0) = segBounds
        xlo = min xlo0 (min srcX mthX)
        ylo = min ylo0 (min srcY mthY)
        xhi = max xhi0 (max srcX mthX)
        yhi = max yhi0 (max srcY mthY)
        maxValley = if V.null (rpSegments river)
                    then 8
                    else V.maximum (V.map rsValleyWidth (rpSegments river))
    in EventBBox (xlo - maxValley) (ylo - maxValley)
                 (xhi + maxValley) (yhi + maxValley)
hydroFeatureBBox (GlacierFeature glacier) ws = glacierBBoxW ws glacier
hydroFeatureBBox (LakeFeature lake) ws =
    let GeoCoord cx0 cy0 = lkCenter lake
        (cx, cy) = wrapCoordU ws cx0 cy0
        r = lkRadius lake
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)

-- | Hydro evolution bbox with wrapping
hydroEvolutionBBoxW ∷ Int → HydroEvolution → EventBBox
hydroEvolutionBBoxW ws (RiverBranch branchPt _angle len _childId) =
    let GeoCoord bx0 by0 = branchPt
        (bx, by) = wrapCoordU ws bx0 by0
    in EventBBox (bx - len) (by - len) (bx + len) (by + len)
hydroEvolutionBBoxW _ws (RiverMeander _ _) = noBBox
hydroEvolutionBBoxW ws (RiverCapture _capturedId capturePoint) =
    let GeoCoord cx0 cy0 = capturePoint
        (cx, cy) = wrapCoordU ws cx0 cy0
        r = 30
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
hydroEvolutionBBoxW ws (RiverDam damPt _lakeId damH) =
    let GeoCoord dx0 dy0 = damPt
        (dx, dy) = wrapCoordU ws dx0 dy0
        r = max 10 damH
    in EventBBox (dx - r) (dy - r) (dx + r) (dy + r)
hydroEvolutionBBoxW _ws RiverDryUp = noBBox
hydroEvolutionBBoxW _ws (GlacierAdvance _ _) = noBBox
hydroEvolutionBBoxW _ws (GlacierRetreat _ _) = noBBox
hydroEvolutionBBoxW _ws (GlacierMelt _) = noBBox
hydroEvolutionBBoxW ws (GlacierBranch branchPt _angle len _childId) =
    let GeoCoord bx0 by0 = branchPt
        (bx, by) = wrapCoordU ws bx0 by0
    in EventBBox (bx - len) (by - len) (bx + len) (by + len)
hydroEvolutionBBoxW _ws (LakeDrain _) = noBBox
hydroEvolutionBBoxW _ws (LakeExpand _ _) = noBBox

-- | Keep old unwrapped versions if anything still references them
hydroEvolutionBBox ∷ HydroEvolution → EventBBox
hydroEvolutionBBox (RiverBranch branchPt _angle len _childId) =
    let GeoCoord bx by = branchPt
    in EventBBox (bx - len) (by - len) (bx + len) (by + len)
hydroEvolutionBBox (RiverMeander _ _) = noBBox
hydroEvolutionBBox (RiverCapture _capturedId capturePoint) =
    let GeoCoord cx cy = capturePoint
        r = 30
    in EventBBox (cx - r) (cy - r) (cx + r) (cy + r)
hydroEvolutionBBox (RiverDam damPt _lakeId damH) =
    let GeoCoord dx dy = damPt
        r = max 10 damH
    in EventBBox (dx - r) (dy - r) (dx + r) (dy + r)
hydroEvolutionBBox RiverDryUp = noBBox
hydroEvolutionBBox (GlacierAdvance _ _) = noBBox
hydroEvolutionBBox (GlacierRetreat _ _) = noBBox
hydroEvolutionBBox (GlacierMelt _) = noBBox
hydroEvolutionBBox (GlacierBranch branchPt _angle len _childId) =
    let GeoCoord bx by = branchPt
    in EventBBox (bx - len) (by - len) (bx + len) (by + len)
hydroEvolutionBBox (LakeDrain _) = noBBox
hydroEvolutionBBox (LakeExpand _ _) = noBBox

-- | Check if a wrapped tile coordinate falls within an event bbox,
--   accounting for u-axis wrapping.
{-# INLINE tileInBBoxWrapped #-}
tileInBBoxWrapped ∷ Int → Int → Int → EventBBox → Bool
tileInBBoxWrapped worldSize gx gy bb =
    let w = worldSize * 16
        halfW = w `div` 2
        -- Bbox center and half-extents
        bMidX = (bbMinX bb + bbMaxX bb) `div` 2
        bMidY = (bbMinY bb + bbMaxY bb) `div` 2
        bHalfX = (bbMaxX bb - bbMinX bb) `div` 2
        bHalfY = (bbMaxY bb - bbMinY bb) `div` 2
        -- Wrapped delta between tile and bbox center in u-space
        du = (gx - gy) - (bMidX - bMidY)
        dv = (gx + gy) - (bMidX + bMidY)
        wdu = ((du + halfW) `mod` w + w) `mod` w - halfW
        dxi = (wdu + dv) `div` 2
        dyi = (dv - wdu) `div` 2
    in abs dxi ≤ bHalfX ∧ abs dyi ≤ bHalfY

bboxOverlapsChunk ∷ Int → EventBBox → Int → Int → Int → Int → Bool
bboxOverlapsChunk worldSize bb cMinX cMinY cMaxX cMaxY =
    let w = worldSize * 16
        halfW = w `div` 2
        -- Chunk center and half-extents
        cMidX = (cMinX + cMaxX) `div` 2
        cMidY = (cMinY + cMaxY) `div` 2
        cHalfX = (cMaxX - cMinX) `div` 2
        cHalfY = (cMaxY - cMinY) `div` 2
        -- Bbox center and half-extents
        bMidX = (bbMinX bb + bbMaxX bb) `div` 2
        bMidY = (bbMinY bb + bbMaxY bb) `div` 2
        bHalfX = (bbMaxX bb - bbMinX bb) `div` 2
        bHalfY = (bbMaxY bb - bbMinY bb) `div` 2
        -- Wrapped distance between centers in u-space
        du = (cMidX - cMidY) - (bMidX - bMidY)
        dv = (cMidX + cMidY) - (bMidX + bMidY)
        wdu = ((du + halfW) `mod` w + w) `mod` w - halfW
        dxi = (wdu + dv) `div` 2
        dyi = (dv - wdu) `div` 2
    in abs dxi ≤ (cHalfX + bHalfX) ∧ abs dyi ≤ (cHalfY + bHalfY)

-- | Tag events with bounding boxes. River HydroEvents are exploded
--   into per-segment events BEFORE tagging, so each segment gets
--   its own tight bbox.
tagEventsWithBBox ∷ Int → [GeoEvent] → [(GeoEvent, EventBBox)]
tagEventsWithBBox worldSize events =
    map (\evt → (evt, eventBBox evt worldSize)) events

data FeatureShape
    = VolcanicShape !VolcanicFeature
    | HydroShape   !HydroFeature
    deriving (Show, Eq, Generic, Serialize, Hashable)

data FeatureActivity
    = FActive
    | FDormant
    | FExtinct
    | FCollapsed
    deriving (Show, Eq, Generic, Serialize, Hashable)

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
    deriving (Show, Eq, Generic, Serialize, Hashable)

data CraterParams = CraterParams
    { cpCenter     ∷ !GeoCoord
    , cpRadius     ∷ !Int
    , cpDepth      ∷ !Int
    , cpRimHeight  ∷ !Int
    , cpEjectaRadius ∷ !Int
    , cpMeteorite  ∷ !(Maybe Word8)
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data VolcanicFeature
    = ShieldVolcano    !ShieldParams
    | CinderCone       !CinderConeParams
    | LavaDome         !LavaDomeParams
    | Caldera          !CalderaParams
    | FissureVolcano   !FissureParams
    | LavaTube         !LavaTubeParams
    | SuperVolcano     !SuperVolcanoParams
    | HydrothermalVent !HydrothermalParams
    deriving (Show, Eq, Generic, Serialize, Hashable)

data ShieldParams = ShieldParams
    { shCenter     ∷ !GeoCoord
    , shBaseRadius ∷ !Int
    , shPeakHeight ∷ !Int
    , shSummitPit  ∷ !Bool
    , shPitRadius  ∷ !Int
    , shPitDepth   ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data CinderConeParams = CinderConeParams
    { ccCenter     ∷ !GeoCoord
    , ccBaseRadius ∷ !Int
    , ccPeakHeight ∷ !Int
    , ccCraterRadius ∷ !Int
    , ccCraterDepth  ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data LavaDomeParams = LavaDomeParams
    { ldCenter     ∷ !GeoCoord
    , ldBaseRadius ∷ !Int
    , ldHeight     ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data CalderaParams = CalderaParams
    { caCenter     ∷ !GeoCoord
    , caOuterRadius ∷ !Int
    , caInnerRadius ∷ !Int
    , caRimHeight   ∷ !Int
    , caFloorDepth  ∷ !Int
    , caHasLake     ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data FissureParams = FissureParams
    { fpStart      ∷ !GeoCoord
    , fpEnd        ∷ !GeoCoord
    , fpWidth      ∷ !Int
    , fpRidgeHeight ∷ !Int
    , fpHasMagma   ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data LavaTubeParams = LavaTubeParams
    { ltStart      ∷ !GeoCoord
    , ltEnd        ∷ !GeoCoord
    , ltWidth      ∷ !Int
    , ltRidgeHeight ∷ !Int
    , ltCollapses  ∷ !Int
    , ltCollapseSeed ∷ !Word64
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data SuperVolcanoParams = SuperVolcanoParams
    { svCenter      ∷ !GeoCoord
    , svCalderaRadius ∷ !Int
    , svRimHeight    ∷ !Int
    , svFloorDepth   ∷ !Int
    , svEjectaRadius ∷ !Int
    , svEjectaDepth  ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data HydrothermalParams = HydrothermalParams
    { htCenter     ∷ !GeoCoord
    , htRadius     ∷ !Int
    , htChimneyHeight ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data LandslideParams = LandslideParams
    { lsCenter     ∷ !GeoCoord
    , lsRadius     ∷ !Int
    , lsDirection  ∷ !Float
    , lsVolume     ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data FloodParams = FloodParams
    { fpCenter     ∷ !GeoCoord
    , fpRadius     ∷ !Int
    , fpDepositDepth ∷ !Int
    , fpMaterial   ∷ !Word8
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data ErosionParams = ErosionParams
    { epIntensity    ∷ !Float
    , epHydraulic    ∷ !Float
    , epThermal      ∷ !Float
    , epWind         ∷ !Float
    , epChemical     ∷ !Float
    , epSeed         ∷ !Word64
    } deriving (Show, Eq, Generic, Serialize, Hashable)

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
    deriving (Show, Eq, Generic, Serialize, Hashable)

data PersistentFeature = PersistentFeature
    { pfId            ∷ !GeoFeatureId
    , pfFeature       ∷ !FeatureShape
    , pfActivity      ∷ !FeatureActivity
    , pfFormationPeriod ∷ !Int
    , pfLastActivePeriod ∷ !Int
    , pfEruptionCount ∷ !Int
    , pfParentId      ∷ !(Maybe GeoFeatureId)
    } deriving (Show, Eq, Generic, Serialize, Hashable)

data LavaFlow = LavaFlow
    { lfSourceX   ∷ !Int
    , lfSourceY   ∷ !Int
    , lfRadius    ∷ !Int
    , lfElevation ∷ !Int
    , lfVolume    ∷ !Int
    , lfMaterial  ∷ !Word8
    , lfViscosity ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, Hashable)
