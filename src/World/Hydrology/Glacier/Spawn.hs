{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Glacier.Spawn
    ( spawnMeltwaterRiver
    , spawnMoraineLake
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.Vector as V
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Hydrology.Types
import World.Geology.Types
import World.Geology.Hash
import World.Hydrology.Glacier.Common (getGlacierParams)

-----------------------------------------------------------
-- Helper: spawn a meltwater river at glacier terminus
-----------------------------------------------------------

-- | Creates a new river feature originating at the glacier's
--   terminal point. The river flows in the same general direction
--   as the glacier but diverges. This is how glaciers seed the
--   river system — glaciers come first geologically, then as
--   they retreat, their meltwater carves river valleys into
--   the U-shaped glacial valleys.
spawnMeltwaterRiver ∷ Word64 → Int → GeoFeatureId → PersistentFeature
                    → ([GeoEvent], TimelineBuildState)
                    → ([GeoEvent], TimelineBuildState)
spawnMeltwaterRiver seed periodIdx parentFid pf (events, tbs) =
    let GeoFeatureId fidInt = parentFid
        (childId, tbs') = allocFeatureId tbs
        glacier = getGlacierParams pf
        GeoCoord cx cy = glCenter glacier
        flowDir = glFlowDir glacier
        glacierLen = fromIntegral (glLength glacier) ∷ Float

        -- River starts at glacier terminus
        termX = cx + round (glacierLen * cos flowDir)
        termY = cy + round (glacierLen * sin flowDir)

        -- River continues roughly in the glacier's flow direction
        -- but with some deviation
        h2 = hashGeo seed fidInt 970
        h3 = hashGeo seed fidInt 971
        h4 = hashGeo seed fidInt 972
        riverAngle = flowDir + (hashToFloatGeo h2 - 0.5) * 0.6
        riverLen = hashToRangeGeo h3 20 60
        endX = termX + round (fromIntegral riverLen * cos riverAngle)
        endY = termY + round (fromIntegral riverLen * sin riverAngle)

        -- Simple single-segment river for now
        -- River generation will add more complexity later
        seg = RiverSegment
            { rsStart      = GeoCoord termX termY
            , rsEnd        = GeoCoord endX endY
            , rsWidth      = hashToRangeGeo h4 2 5
            , rsValleyWidth = hashToRangeGeo (hashGeo seed fidInt 973) 8 20
            , rsDepth      = hashToRangeGeo (hashGeo seed fidInt 974) 5 15
            , rsFlowRate   = 0.4  -- moderate meltwater
            , rsStartElev  = 0
            , rsEndElev    = 0
            }

        riverParams = RiverParams
            { rpSourceRegion = GeoCoord termX termY
            , rpMouthRegion  = GeoCoord endX endY
            , rpSegments     = V.singleton seg
            , rpFlowRate     = 0.4
            , rpMeanderSeed  = fromIntegral (hashGeo seed fidInt 975)
            }

        childPf = PersistentFeature
            { pfId               = childId
            , pfFeature          = HydroShape $ RiverFeature riverParams
            , pfActivity         = FActive
            , pfFormationPeriod   = periodIdx
            , pfLastActivePeriod  = periodIdx
            , pfEruptionCount     = 1
            , pfParentId          = Just parentFid
            }

        evt = HydroEvent (RiverFeature riverParams)
        tbs'' = registerFeature childPf tbs'
    in (evt : events, tbs'')

-----------------------------------------------------------
-- Helper: spawn a moraine-dammed lake
-----------------------------------------------------------

-- | When a glacier retreats, its terminal moraine can dam
--   the valley, creating a lake behind it. Classic examples:
--   the Great Lakes, Lake Geneva, Lake Como.
--
--   The lake forms at the glacier terminus. Its surface level
--   equals the moraine height. The basin depth is the glacier's
--   carve depth below that.
spawnMoraineLake ∷ Word64 → Int → GeoFeatureId → PersistentFeature
                 → ([GeoEvent], TimelineBuildState)
                 → ([GeoEvent], TimelineBuildState)
spawnMoraineLake seed periodIdx parentFid pf (events, tbs) =
    let GeoFeatureId fidInt = parentFid
        (childId, tbs') = allocFeatureId tbs
        glacier = getGlacierParams pf
        GeoCoord cx cy = glCenter glacier
        flowDir = glFlowDir glacier
        glacierLen = fromIntegral (glLength glacier) ∷ Float

        -- Lake sits just behind the terminal moraine
        -- (upstream of the terminus)
        lakeDistFromCenter = glacierLen * 0.75
        lakeX = cx + round (lakeDistFromCenter * cos flowDir)
        lakeY = cy + round (lakeDistFromCenter * sin flowDir)

        h2 = hashGeo seed fidInt 980
        h3 = hashGeo seed fidInt 981
        h4 = hashGeo seed fidInt 982

        lakeRadius = hashToRangeGeo h2 8 25
        -- Lake surface = moraine height (relative, will be
        -- combined with base elevation at chunk gen time)
        lakeSurface = glMoraineSize glacier
        lakeDepth = hashToRangeGeo h3 5 (glCarveDepth glacier)

        lakeParams = LakeParams
            { lkCenter  = GeoCoord lakeX lakeY
            , lkRadius  = lakeRadius
            , lkSurface = lakeSurface
            , lkDepth   = lakeDepth
            , lkSource  = GlacialBasin parentFid
            }

        childPf = PersistentFeature
            { pfId               = childId
            , pfFeature          = HydroShape $ LakeFeature lakeParams
            , pfActivity         = FActive
            , pfFormationPeriod   = periodIdx
            , pfLastActivePeriod  = periodIdx
            , pfEruptionCount     = 0
            , pfParentId          = Just parentFid
            }

        evt = HydroEvent (LakeFeature lakeParams)
        tbs'' = registerFeature childPf tbs'
    in (evt : events, tbs'')
