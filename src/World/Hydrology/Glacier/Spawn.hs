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

-- * Meltwater river spawning

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

        -- Build 4 segments with perpendicular noise for natural look.
        -- Without this, glacier rivers are ruler-straight from terminus
        -- to endpoint — visually jarring and unrealistic.
        numSegs = 4 ∷ Int
        segWidth = hashToRangeGeo h4 2 5
        segDepth = hashToRangeGeo (hashGeo seed fidInt 974) 3 8
        segValleyW = hashToRangeGeo (hashGeo seed fidInt 973) 8 16
        dx = fromIntegral (endX - termX) ∷ Float
        dy = fromIntegral (endY - termY) ∷ Float
        rLen = sqrt (dx * dx + dy * dy)
        -- Perpendicular direction for noise offset
        (perpX, perpY) = if rLen > 0.001
            then (negate dy / rLen, dx / rLen)
            else (0.0, 1.0)
        -- Build waypoints with noise
        waypoints = [ let t = fromIntegral i / fromIntegral numSegs ∷ Float
                          baseX = fromIntegral termX + t * fromIntegral (endX - termX)
                          baseY = fromIntegral termY + t * fromIntegral (endY - termY)
                          -- Taper noise at endpoints
                          taper = sin (t * 3.14159)
                          noiseH = hashGeo seed (fidInt * 13 + i) 976
                          noiseVal = (hashToFloatGeo noiseH - 0.5) * rLen * 0.25 * taper
                          wx = round (baseX + perpX * noiseVal)
                          wy = round (baseY + perpY * noiseVal)
                      in GeoCoord wx wy
                    | i ← [0 .. numSegs] ]
        segments = V.fromList $ zipWith (\i (s, e) →
            let t = fromIntegral (i + 1) / fromIntegral numSegs ∷ Float
                flow = 0.2 + t * 0.3
            in RiverSegment
                { rsStart      = s
                , rsEnd        = e
                , rsWidth      = segWidth
                , rsValleyWidth = segValleyW
                , rsDepth      = segDepth
                , rsFlowRate   = flow
                , rsStartElev  = 0
                , rsEndElev    = 0
                , rsWaterStart = 0
                , rsWaterEnd   = 0
                }
            ) [0∷Int ..] (zip waypoints (drop 1 waypoints))

        riverParams = RiverParams
            { rpSourceRegion = GeoCoord termX termY
            , rpMouthRegion  = GeoCoord endX endY
            , rpSegments     = segments
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

-- * Moraine-dammed lake spawning

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
