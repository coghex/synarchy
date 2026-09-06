{-# LANGUAGE Strict #-}
module World.Geology.Log
    ( logTimeline
    , logTimelineSummary
    , formatPlatesSummary
    ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import World.Types
import World.Material (getMaterialProps, MaterialProps(..), MaterialRegistry)
import World.Plate (generatePlates)

-- * Timeline Summary

data TimelineSummary = TimelineSummary
    { tsNumPeriods      ∷ !Int
    , tsTotalEvents     ∷ !Int
    , tsTotalDuration   ∷ !Int
    , tsEventCounts     ∷ !(Map.Map Text Int)
    , tsScaleCounts     ∷ !(Map.Map GeoScale Int)
    , tsFeatureCount    ∷ !Int
    , tsActiveFeatures  ∷ !Int
    , tsDormantFeatures ∷ !Int
    , tsExtinctFeatures ∷ !Int
    , tsTotalCraters    ∷ !Int
    } deriving (Show)

summarizeTimeline ∷ GeoTimeline → TimelineSummary
summarizeTimeline tl =
    let periods = gtPeriods tl
        features = gtFeatures tl

        allEvents = concatMap gpEvents periods

        eventCounts = foldl' countEvent Map.empty allEvents
        scaleCounts = foldl' (\m p → Map.insertWith (+) (gpScale p) 1 m)
                             Map.empty periods

        (active, dormant, extinct) = foldl' countActivity (0, 0, 0) features

        craterCount = length [ () | CraterEvent _ ← allEvents ]

    in TimelineSummary
        { tsNumPeriods      = length periods
        , tsTotalEvents     = length allEvents
        , tsTotalDuration   = sum (map gpDuration periods)
        , tsEventCounts     = eventCounts
        , tsScaleCounts     = scaleCounts
        , tsFeatureCount    = length features
        , tsActiveFeatures  = active
        , tsDormantFeatures = dormant
        , tsExtinctFeatures = extinct
        , tsTotalCraters    = craterCount
        }

countEvent ∷ Map.Map Text Int → GeoEvent → Map.Map Text Int
countEvent m (CraterEvent _)         = Map.insertWith (+) "Craters" 1 m
countEvent m (VolcanicEvent f)       = Map.insertWith (+) (featureTypeName f) 1 m
countEvent m (VolcanicModify _ evo)  = Map.insertWith (+) (evolutionName evo) 1 m
countEvent m (LandslideEvent _)      = Map.insertWith (+) "Landslides" 1 m
countEvent m (GlaciationEvent _)     = Map.insertWith (+) "Glaciations" 1 m
countEvent m (FloodEvent _)          = Map.insertWith (+) "Floods" 1 m
countEvent m (HydroEvent _)          = Map.insertWith (+) "Hydro Events" 1 m
countEvent m (HydroModify _ _)       = Map.insertWith (+) "Hydro Modifications" 1 m
countEvent m (RiverSegmentEvent _)   = Map.insertWith (+) "River Segments" 1 m
countEvent m (RiverDeltaEvent _)     = Map.insertWith (+) "River Deltas" 1 m
countEvent m (OreSheetEvent _)       = Map.insertWith (+) "Ore Sheets" 1 m
countEvent m (GlacierMoraineEvent _) = Map.insertWith (+) "Glacier Moraines" 1 m

featureTypeName ∷ FeatureShape → Text
featureTypeName (VolcanicShape (ShieldVolcano _))    = "Shield Volcanoes"
featureTypeName (VolcanicShape (CinderCone _))       = "Cinder Cones"
featureTypeName (VolcanicShape (LavaDome _))         = "Lava Domes"
featureTypeName (VolcanicShape (Caldera _))          = "Calderas"
featureTypeName (VolcanicShape (FissureVolcano _))   = "Fissures"
featureTypeName (VolcanicShape (LavaTube _))         = "Lava Tubes"
featureTypeName (VolcanicShape (SuperVolcano _))     = "Super Volcanoes"
featureTypeName (VolcanicShape (HydrothermalVent _)) = "Hydrothermal Vents"
featureTypeName _                     = "Other Features"

evolutionName ∷ FeatureEvolution → Text
evolutionName (Reactivate _ _ _ _ _)      = "Reactivations"
evolutionName (GoDormant _ _)             = "Went Dormant"
evolutionName (GoExtinct _ _)             = "Went Extinct"
evolutionName (CollapseToCaldera _ _ _ _ _) = "Caldera Collapses"
evolutionName (ParasiticEruption _ _ _ _) = "Parasitic Eruptions"
evolutionName (FlankCollapse _ _ _ _ _)   = "Flank Collapses"

countActivity ∷ (Int, Int, Int) → PersistentFeature → (Int, Int, Int)
countActivity (a, d, e) pf = case pfActivity pf of
    FActive    → (a + 1, d, e)
    FDormant   → (a, d + 1, e)
    FExtinct   → (a, d, e + 1)
    FCollapsed → (a, d, e + 1)

-- * Scale Display

showScale ∷ GeoScale → Text
showScale Eon    = "Eon"
showScale Era    = "Era"
showScale Period = "Period"
showScale Epoch  = "Epoch"
showScale Age    = "Age"

-- * Plate Formatting

-- | Format the tectonic plates as a summary section.
--   Called separately since plates are generated from seed/size,
--   not stored in the timeline.
formatPlatesSummary ∷ Word64 → Int → Int → MaterialRegistry → [Text]
formatPlatesSummary seed worldSize plateCount registry =
    let plates = generatePlates seed worldSize plateCount
        header = "═══ Tectonic Plates (" <> tshow plateCount <> ") ═══"
        plateMsgs = zipWith (formatOnePlate registry) [0..] plates
    in header : plateMsgs

formatOnePlate ∷ MaterialRegistry → Int → TectonicPlate → Text
formatOnePlate registry idx plate =
    let landType = if plateIsLand plate then "Continental" else "Oceanic"
        matName' = mpName (getMaterialProps registry (plateMaterial plate))
        GeoCoord cx cy = plateCoord plate
    in "  Plate #" <> tshow idx <> ": "
       <> padR 14 landType
       <> matName' <> " "
       <> "(" <> tshow cx <> ", " <> tshow cy <> ") "
       <> "elev=" <> tshow (plateBaseElev plate)

-- | Helper to extract a GeoCoord from a plate
plateCoord ∷ TectonicPlate → GeoCoord
plateCoord p = GeoCoord (plateCenterX p) (plateCenterY p)

-- * Full Timeline Formatting

-- | Format the entire timeline as a list of text lines.
--   Returns a [Text] rather than logging directly, so the caller
--   chooses the sink: 'logTimeline' below takes an arbitrary
--   @Text → m ()@ callback, and a caller may fan one list out to
--   several sinks without re-parsing. It names no sink of its own
--   (#1933).
formatTimeline ∷ GeoTimeline → [Text]
formatTimeline tl =
    let summary = summarizeTimeline tl
        headerLines = formatSummaryLines summary
        -- Periods are already in chronological order (reversed during build)
        periodLines = concatMap formatPeriodChronological (gtPeriods tl)
        featureLines = formatFeatureLines (gtFeatures tl)
    in headerLines <> periodLines <> featureLines

-- | Format the summary header as lines.
formatSummaryLines ∷ TimelineSummary → [Text]
formatSummaryLines ts =
    [ "╔══════════════════════════════════════════════════════════╗"
    , "║              GEOLOGICAL TIMELINE SUMMARY                ║"
    , "╠══════════════════════════════════════════════════════════╣"
    , "║  Total geological time: " <> padR 31 (tshow (tsTotalDuration ts) <> " MY") <> "║"
    , "║  Timeline periods:      " <> padR 31 (tshow (tsNumPeriods ts)) <> "║"
    , "║  Total events:          " <> padR 31 (tshow (tsTotalEvents ts)) <> "║"
    , "║  Total craters:         " <> padR 31 (tshow (tsTotalCraters ts)) <> "║"
    , "║  Persistent features:   " <> padR 31 (tshow (tsFeatureCount ts)) <> "║"
    , "║    Active:              " <> padR 31 (tshow (tsActiveFeatures ts)) <> "║"
    , "║    Dormant:             " <> padR 31 (tshow (tsDormantFeatures ts)) <> "║"
    , "║    Extinct/Collapsed:   " <> padR 31 (tshow (tsExtinctFeatures ts)) <> "║"
    , "╠══════════════════════════════════════════════════════════╣"
    , "║  Event breakdown:                                       ║"
    ] <> formatEventCountLines (tsEventCounts ts)
    <> [ "╚══════════════════════════════════════════════════════════╝"
       , ""
       , "═══ Chronological Event Log ═══"
       ]

padR ∷ Int → Text → Text
padR n t = t <> T.replicate (max 0 (n - T.length t)) " "

formatEventCountLines ∷ Map.Map Text Int → [Text]
formatEventCountLines m =
    [ "║    " <> padR 22 name <> padR 32 (tshow c) <> "║"
    | (name, c) ← Map.toAscList m
    ]

-- * Chronological Period Formatting

-- | Format a single period with its date and all events expanded.
--   Each event gets its own line with coordinates.
formatPeriodChronological ∷ GeoPeriod → [Text]
formatPeriodChronological period =
    let dateStr = formatMyDate (gpDate period)
        scaleStr = showScale (gpScale period)
        header = dateStr <> " [" <> padR 6 scaleStr <> "] "
              <> gpName period <> " (" <> tshow (gpDuration period) <> " MY)"
        events = gpEvents period
        eventLines = if null events
            then ["  └─ (erosion only)"]
            else map (\e → "  ├─ " <> formatEventDetailed e) events
    in header : eventLines

-- | Format a date in MY for display.
--   Shows as "  123.0 MY" left-padded for alignment.
formatMyDate ∷ Float → Text
formatMyDate my =
    let raw = T.pack (showFFloat1 my) <> " MY"
    in padL 10 raw

-- | Left-pad a text to a given width.
padL ∷ Int → Text → Text
padL n t = T.replicate (max 0 (n - T.length t)) " " <> t

-- | Show a float with 1 decimal place.
showFFloat1 ∷ Float → String
showFFloat1 f =
    let sign  = if f < 0 then "-" else ""
        af    = abs f
        whole = floor af ∷ Int
        frac  = round ((af - fromIntegral whole) * 10.0) ∷ Int
    in sign <> show whole <> "." <> show frac

-- | Format a single event with full detail including coordinates.
formatEventDetailed ∷ GeoEvent → Text
formatEventDetailed (CraterEvent cp) =
    let GeoCoord cx cy = cpCenter cp
    in "Crater r=" <> tshow (cpRadius cp)
       <> " depth=" <> tshow (cpDepth cp)
       <> " (" <> tshow cx <> ", " <> tshow cy <> ")"
       <> case cpMeteorite cp of
            Just _mat → " *meteorite*"
            Nothing  → ""

formatEventDetailed (VolcanicEvent feature) =
    formatFeatureEvent feature

formatEventDetailed (VolcanicModify (GeoFeatureId fid) evo) =
    "Feature #" <> tshow fid <> " " <> formatEvolution evo

formatEventDetailed (LandslideEvent _) = "Landslide"
formatEventDetailed (GlaciationEvent _) = "Glaciation"
formatEventDetailed (FloodEvent _) = "Flood"
formatEventDetailed (HydroEvent _) = "Hydro Event"
formatEventDetailed (HydroModify (GeoFeatureId fid) desc) =
    "Hydro modification at Feature #" <> tshow fid <> ": " <> tshow desc
formatEventDetailed (RiverSegmentEvent rsc) =
    let seg = rscSegment rsc
        GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
    in "River Segment w=" <> tshow (rsWidth seg)
       <> " (" <> tshow sx <> ", " <> tshow sy <> ")"
       <> "→(" <> tshow ex <> ", " <> tshow ey <> ")"
formatEventDetailed (RiverDeltaEvent rdp) =
    let seg = rdpLastSegment rdp
        GeoCoord mx my = rsEnd seg
    in "River Delta flow=" <> tshow (rdpFlowRate rdp)
       <> " (" <> tshow mx <> ", " <> tshow my <> ")"
formatEventDetailed (OreSheetEvent _) = "Ore Sheet"
formatEventDetailed (GlacierMoraineEvent _) = "Glacier Moraine"

-- | Format a volcanic feature event with type and coordinates.
formatFeatureEvent ∷ FeatureShape → Text
formatFeatureEvent (VolcanicShape (ShieldVolcano p)) =
    let GeoCoord cx cy = shCenter p
    in "Shield Volcano baseR=" <> tshow (shBaseRadius p)
       <> " height=" <> tshow (shPeakHeight p)
       <> " (" <> tshow cx <> ", " <> tshow cy <> ")"

formatFeatureEvent (VolcanicShape (CinderCone p)) =
    let GeoCoord cx cy = ccCenter p
    in "Cinder Cone baseR=" <> tshow (ccBaseRadius p)
       <> " height=" <> tshow (ccPeakHeight p)
       <> " (" <> tshow cx <> ", " <> tshow cy <> ")"

formatFeatureEvent (VolcanicShape (LavaDome p)) =
    let GeoCoord cx cy = ldCenter p
    in "Lava Dome baseR=" <> tshow (ldBaseRadius p)
       <> " height=" <> tshow (ldHeight p)
       <> " (" <> tshow cx <> ", " <> tshow cy <> ")"

formatFeatureEvent (VolcanicShape (Caldera p)) =
    let GeoCoord cx cy = caCenter p
    in "Caldera outerR=" <> tshow (caOuterRadius p)
       <> " rimH=" <> tshow (caRimHeight p)
       <> " (" <> tshow cx <> ", " <> tshow cy <> ")"

formatFeatureEvent (VolcanicShape (FissureVolcano p)) =
    let GeoCoord sx sy = fpStart p
        GeoCoord ex ey = fpEnd p
    in "Fissure width=" <> tshow (fpWidth p)
       <> " ridgeH=" <> tshow (fpRidgeHeight p)
       <> " (" <> tshow sx <> ", " <> tshow sy <> ")"
       <> "→(" <> tshow ex <> ", " <> tshow ey <> ")"

formatFeatureEvent (VolcanicShape (LavaTube p)) =
    let GeoCoord sx sy = ltStart p
        GeoCoord ex ey = ltEnd p
    in "Lava Tube width=" <> tshow (ltWidth p)
       <> " collapses=" <> tshow (ltCollapses p)
       <> " (" <> tshow sx <> ", " <> tshow sy <> ")"
       <> "→(" <> tshow ex <> ", " <> tshow ey <> ")"

formatFeatureEvent (VolcanicShape (SuperVolcano p)) =
    let GeoCoord cx cy = svCenter p
    in "★ SUPERVOLCANO calderaR=" <> tshow (svCalderaRadius p)
       <> " ejectaR=" <> tshow (svEjectaRadius p)
       <> " (" <> tshow cx <> ", " <> tshow cy <> ")"

formatFeatureEvent (VolcanicShape (HydrothermalVent p)) =
    let GeoCoord cx cy = htCenter p
    in "Hydrothermal Vent r=" <> tshow (htRadius p)
       <> " chimneyH=" <> tshow (htChimneyHeight p)
       <> " (" <> tshow cx <> ", " <> tshow cy <> ")"
-- This volcanic formatter only ever sees 'VolcanicShape' (from
-- 'VolcanicEvent'). The catch-all keeps the match total without a
-- crash if that ever changes.
formatFeatureEvent (HydroShape _) = "Hydro feature"

-- | Format an evolution event.
formatEvolution ∷ FeatureEvolution → Text
formatEvolution (Reactivate hGain _ (GeoCoord cx cy) _ _) =
    "Reactivated +" <> tshow hGain <> "m"
    <> " (" <> tshow cx <> ", " <> tshow cy <> ")"
formatEvolution (GoDormant (GeoCoord cx cy) _) =
    "Went Dormant (" <> tshow cx <> ", " <> tshow cy <> ")"
formatEvolution (GoExtinct (GeoCoord cx cy) _) =
    "Went Extinct (" <> tshow cx <> ", " <> tshow cy <> ")"
formatEvolution (CollapseToCaldera depth _ (GeoCoord cx cy) _ _) =
    "Collapsed to Caldera depth=" <> tshow depth
    <> " (" <> tshow cx <> ", " <> tshow cy <> ")"
formatEvolution (ParasiticEruption _ _ (GeoCoord cx cy) _) =
    "Parasitic Eruption (" <> tshow cx <> ", " <> tshow cy <> ")"
formatEvolution (FlankCollapse _ _ _ (GeoCoord cx cy) _) =
    "Flank Collapse (" <> tshow cx <> ", " <> tshow cy <> ")"

-- * Feature List

formatFeatureLines ∷ [PersistentFeature] → [Text]
formatFeatureLines features =
    let header = ""
        header2 = "═══ Persistent Features (" <> tshow (length features) <> ") ═══"
    in [header, header2] <> map formatOneFeature features

formatOneFeature ∷ PersistentFeature → Text
formatOneFeature pf =
    let GeoFeatureId fid = pfId pf
        (name, coord, details) = describeFeature' (pfFeature pf)
        GeoCoord fx fy = coord
        activity = case pfActivity pf of
            FActive    → "[ACTIVE]  "
            FDormant   → "[DORMANT] "
            FExtinct   → "[EXTINCT] "
            FCollapsed → "[COLLAPS] "
        parent = case pfParentId pf of
            Just (GeoFeatureId pid) → " parent=#" <> tshow pid
            Nothing → ""
        eruptions = if pfEruptionCount pf > 1
            then " eruptions=" <> tshow (pfEruptionCount pf)
            else ""
    in "  #" <> padR 4 (tshow fid)
       <> activity <> padR 20 name
       <> " (" <> tshow fx <> ", " <> tshow fy <> ") "
       <> details <> parent <> eruptions

describeFeature' ∷ FeatureShape → (Text, GeoCoord, Text)
describeFeature' (VolcanicShape (ShieldVolcano p)) =
    ("Shield Volcano", shCenter p,
     "baseR=" <> tshow (shBaseRadius p)
     <> " height=" <> tshow (shPeakHeight p))
describeFeature' (VolcanicShape (CinderCone p)) =
    ("Cinder Cone", ccCenter p,
     "baseR=" <> tshow (ccBaseRadius p)
     <> " height=" <> tshow (ccPeakHeight p))
describeFeature' (VolcanicShape (LavaDome p)) =
    ("Lava Dome", ldCenter p,
     "baseR=" <> tshow (ldBaseRadius p)
     <> " height=" <> tshow (ldHeight p))
describeFeature' (VolcanicShape (Caldera p)) =
    ("Caldera", caCenter p,
     "outerR=" <> tshow (caOuterRadius p)
     <> " rimH=" <> tshow (caRimHeight p))
describeFeature' (VolcanicShape (FissureVolcano p)) =
    ("Fissure", fpStart p,
     "width=" <> tshow (fpWidth p)
     <> " ridgeH=" <> tshow (fpRidgeHeight p))
describeFeature' (VolcanicShape (LavaTube p)) =
    ("Lava Tube", ltStart p,
     "width=" <> tshow (ltWidth p)
     <> " collapses=" <> tshow (ltCollapses p))
describeFeature' (VolcanicShape (SuperVolcano p)) =
    ("SUPERVOLCANO", svCenter p,
     "calderaR=" <> tshow (svCalderaRadius p)
     <> " ejectaR=" <> tshow (svEjectaRadius p))
describeFeature' (VolcanicShape (HydrothermalVent p)) =
    ("Hydrothermal Vent", htCenter p,
     "radius=" <> tshow (htRadius p)
     <> " chimneyH=" <> tshow (htChimneyHeight p))
describeFeature' (HydroShape (RiverFeature p)) =
    ("River", rpSourceRegion p,
     "mouth=" <> tshow (rpMouthRegion p)
     <> " flow=" <> tshow (rpFlowRate p))
describeFeature' (HydroShape (GlacierFeature p)) =
    ("Glacier", glCenter p,
     "length=" <> tshow (glLength p)
     <> " width=" <> tshow (glWidth p)
     <> " thickness=" <> tshow (glThickness p))
describeFeature' (HydroShape (LakeFeature p)) =
    ("Lake", lkCenter p,
     "radius=" <> tshow (lkRadius p)
     <> " surface=" <> tshow (lkSurface p)
     <> " depth=" <> tshow (lkDepth p))

-- * IO Logging Functions

-- | Log the full timeline using a provided log function.
--   Returns the formatted lines so the caller can also
--   send them elsewhere (e.g., to the Lua panel).
logTimeline ∷ MonadIO m ⇒ (Text → m ()) → GeoTimeline → m ()
logTimeline logFn tl =
    let logLines = formatTimeline tl
    in mapM_ logFn logLines

-- | Log just the summary (shorter output).
logTimelineSummary ∷ MonadIO m ⇒ (Text → m ()) → GeoTimeline → m ()
logTimelineSummary logFn tl =
    let summary = summarizeTimeline tl
        logLines = formatSummaryLines summary
    in mapM_ logFn logLines
