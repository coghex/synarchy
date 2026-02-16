{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Log
    ( logTimeline
    , logTimelineSummary
    , formatTimeline
    , formatPlatesSummary
    ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import World.Types
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..))
import World.Plate (TectonicPlate(..), generatePlates)
import World.Geology.Types

-----------------------------------------------------------
-- Timeline Summary
-----------------------------------------------------------

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
countEvent m (EruptionEvent _ _)     = Map.insertWith (+) "Eruptions" 1 m
countEvent m (LandslideEvent _)      = Map.insertWith (+) "Landslides" 1 m
countEvent m (GlaciationEvent _)     = Map.insertWith (+) "Glaciations" 1 m
countEvent m (FloodEvent _)          = Map.insertWith (+) "Floods" 1 m

featureTypeName ∷ VolcanicFeature → Text
featureTypeName (ShieldVolcano _)    = "Shield Volcanoes"
featureTypeName (CinderCone _)       = "Cinder Cones"
featureTypeName (LavaDome _)         = "Lava Domes"
featureTypeName (Caldera _)          = "Calderas"
featureTypeName (FissureVolcano _)   = "Fissures"
featureTypeName (LavaTube _)         = "Lava Tubes"
featureTypeName (SuperVolcano _)     = "Super Volcanoes"
featureTypeName (HydrothermalVent _) = "Hydrothermal Vents"

evolutionName ∷ FeatureEvolution → Text
evolutionName (Reactivate _ _ _ _)        = "Reactivations"
evolutionName (GoDormant _ _)             = "Went Dormant"
evolutionName (GoExtinct _ _)             = "Went Extinct"
evolutionName (CollapseToCaldera _ _ _ _) = "Caldera Collapses"
evolutionName (ParasiticEruption _ _ _ _) = "Parasitic Eruptions"
evolutionName (FlankCollapse _ _ _ _ _)   = "Flank Collapses"

countActivity ∷ (Int, Int, Int) → PersistentFeature → (Int, Int, Int)
countActivity (a, d, e) pf = case pfActivity pf of
    Active    → (a + 1, d, e)
    Dormant   → (a, d + 1, e)
    Extinct   → (a, d, e + 1)
    Collapsed → (a, d, e + 1)

-----------------------------------------------------------
-- Scale Display
-----------------------------------------------------------

showScale ∷ GeoScale → Text
showScale Eon    = "Eon"
showScale Era    = "Era"
showScale Period = "Period"
showScale Epoch  = "Epoch"
showScale Age    = "Age"

-----------------------------------------------------------
-- Plate Formatting
-----------------------------------------------------------

-- | Format the tectonic plates as a summary section.
--   Called separately since plates are generated from seed/size,
--   not stored in the timeline.
formatPlatesSummary ∷ Word64 → Int → Int → [Text]
formatPlatesSummary seed worldSize plateCount =
    let plates = generatePlates seed worldSize plateCount
        header = "═══ Tectonic Plates (" <> T.pack (show plateCount) <> ") ═══"
        plateMsgs = zipWith formatOnePlate [0..] plates
    in header : plateMsgs

formatOnePlate ∷ Int → TectonicPlate → Text
formatOnePlate idx plate =
    let landType = if plateIsLand plate then "Continental" else "Oceanic"
        matName' = matName (getMaterialProps (plateMaterial plate))
        GeoCoord cx cy = plateCoord plate
    in "  Plate #" <> T.pack (show idx) <> ": "
       <> padR 14 landType
       <> matName' <> " "
       <> "(" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ") "
       <> "elev=" <> T.pack (show (plateBaseElev plate))

-- | Helper to extract a GeoCoord from a plate
plateCoord ∷ TectonicPlate → GeoCoord
plateCoord p = GeoCoord (plateCenterX p) (plateCenterY p)

-----------------------------------------------------------
-- Full Timeline Formatting
-----------------------------------------------------------

-- | Format the entire timeline as a list of text lines.
--   Returns a [Text] so callers can send each line to
--   both logInfo and sendGenLog without re-parsing.
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
    , "║  Total geological time: " <> padR 31 (T.pack (show (tsTotalDuration ts)) <> " MY") <> "║"
    , "║  Timeline periods:      " <> padR 31 (T.pack (show (tsNumPeriods ts))) <> "║"
    , "║  Total events:          " <> padR 31 (T.pack (show (tsTotalEvents ts))) <> "║"
    , "║  Total craters:         " <> padR 31 (T.pack (show (tsTotalCraters ts))) <> "║"
    , "║  Persistent features:   " <> padR 31 (T.pack (show (tsFeatureCount ts))) <> "║"
    , "║    Active:              " <> padR 31 (T.pack (show (tsActiveFeatures ts))) <> "║"
    , "║    Dormant:             " <> padR 31 (T.pack (show (tsDormantFeatures ts))) <> "║"
    , "║    Extinct/Collapsed:   " <> padR 31 (T.pack (show (tsExtinctFeatures ts))) <> "║"
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
    [ "║    " <> padR 22 name <> padR 32 (T.pack (show c)) <> "║"
    | (name, c) ← Map.toAscList m
    ]

-----------------------------------------------------------
-- Chronological Period Formatting
-----------------------------------------------------------

-- | Format a single period with its date and all events expanded.
--   Each event gets its own line with coordinates.
formatPeriodChronological ∷ GeoPeriod → [Text]
formatPeriodChronological period =
    let dateStr = formatMyDate (gpDate period)
        scaleStr = showScale (gpScale period)
        header = dateStr <> " [" <> padR 6 scaleStr <> "] "
              <> gpName period <> " (" <> T.pack (show (gpDuration period)) <> " MY)"
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
    let whole = floor f ∷ Int
        frac  = round ((f - fromIntegral whole) * 10.0) ∷ Int
    in show whole <> "." <> show (abs frac)

-- | Format a single event with full detail including coordinates.
formatEventDetailed ∷ GeoEvent → Text
formatEventDetailed (CraterEvent cp) =
    let GeoCoord cx cy = cpCenter cp
    in "Crater r=" <> T.pack (show (cpRadius cp))
       <> " depth=" <> T.pack (show (cpDepth cp))
       <> " (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"
       <> case cpMeteorite cp of
            Just mat → " meteorite=" <> matName (getMaterialProps (MaterialId mat))
            Nothing  → ""

formatEventDetailed (VolcanicEvent feature) =
    formatFeatureEvent feature

formatEventDetailed (VolcanicModify (GeoFeatureId fid) evo) =
    "Feature #" <> T.pack (show fid) <> " " <> formatEvolution evo

formatEventDetailed (EruptionEvent (GeoFeatureId fid) strength) =
    "Eruption at Feature #" <> T.pack (show fid) <> " strength=" <> T.pack (show strength)
formatEventDetailed (LandslideEvent _) = "Landslide"
formatEventDetailed (GlaciationEvent _) = "Glaciation"
formatEventDetailed (FloodEvent _) = "Flood"

-- | Format a volcanic feature event with type and coordinates.
formatFeatureEvent ∷ VolcanicFeature → Text
formatFeatureEvent (ShieldVolcano p) =
    let GeoCoord cx cy = shCenter p
    in "Shield Volcano baseR=" <> T.pack (show (shBaseRadius p))
       <> " height=" <> T.pack (show (shPeakHeight p))
       <> " (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"

formatFeatureEvent (CinderCone p) =
    let GeoCoord cx cy = ccCenter p
    in "Cinder Cone baseR=" <> T.pack (show (ccBaseRadius p))
       <> " height=" <> T.pack (show (ccPeakHeight p))
       <> " (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"

formatFeatureEvent (LavaDome p) =
    let GeoCoord cx cy = ldCenter p
    in "Lava Dome baseR=" <> T.pack (show (ldBaseRadius p))
       <> " height=" <> T.pack (show (ldHeight p))
       <> " (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"

formatFeatureEvent (Caldera p) =
    let GeoCoord cx cy = caCenter p
    in "Caldera outerR=" <> T.pack (show (caOuterRadius p))
       <> " rimH=" <> T.pack (show (caRimHeight p))
       <> " (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"

formatFeatureEvent (FissureVolcano p) =
    let GeoCoord sx sy = fpStart p
        GeoCoord ex ey = fpEnd p
    in "Fissure width=" <> T.pack (show (fpWidth p))
       <> " ridgeH=" <> T.pack (show (fpRidgeHeight p))
       <> " (" <> T.pack (show sx) <> ", " <> T.pack (show sy) <> ")"
       <> "→(" <> T.pack (show ex) <> ", " <> T.pack (show ey) <> ")"

formatFeatureEvent (LavaTube p) =
    let GeoCoord sx sy = ltStart p
        GeoCoord ex ey = ltEnd p
    in "Lava Tube width=" <> T.pack (show (ltWidth p))
       <> " collapses=" <> T.pack (show (ltCollapses p))
       <> " (" <> T.pack (show sx) <> ", " <> T.pack (show sy) <> ")"
       <> "→(" <> T.pack (show ex) <> ", " <> T.pack (show ey) <> ")"

formatFeatureEvent (SuperVolcano p) =
    let GeoCoord cx cy = svCenter p
    in "★ SUPERVOLCANO calderaR=" <> T.pack (show (svCalderaRadius p))
       <> " ejectaR=" <> T.pack (show (svEjectaRadius p))
       <> " (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"

formatFeatureEvent (HydrothermalVent p) =
    let GeoCoord cx cy = htCenter p
    in "Hydrothermal Vent r=" <> T.pack (show (htRadius p))
       <> " chimneyH=" <> T.pack (show (htChimneyHeight p))
       <> " (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"

-- | Format an evolution event.
formatEvolution ∷ FeatureEvolution → Text
formatEvolution (Reactivate hGain _ (GeoCoord cx cy) _) =
    "Reactivated +" <> T.pack (show hGain) <> "m"
    <> " (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"
formatEvolution (GoDormant (GeoCoord cx cy) _) =
    "Went Dormant (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"
formatEvolution (GoExtinct (GeoCoord cx cy) _) =
    "Went Extinct (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"
formatEvolution (CollapseToCaldera depth _ (GeoCoord cx cy) _) =
    "Collapsed to Caldera depth=" <> T.pack (show depth)
    <> " (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"
formatEvolution (ParasiticEruption _ _ (GeoCoord cx cy) _) =
    "Parasitic Eruption (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"
formatEvolution (FlankCollapse _ _ _ (GeoCoord cx cy) _) =
    "Flank Collapse (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"

-----------------------------------------------------------
-- Feature List
-----------------------------------------------------------

formatFeatureLines ∷ [PersistentFeature] → [Text]
formatFeatureLines features =
    let header = ""
        header2 = "═══ Persistent Features (" <> T.pack (show (length features)) <> ") ═══"
    in [header, header2] <> map formatOneFeature features

formatOneFeature ∷ PersistentFeature → Text
formatOneFeature pf =
    let GeoFeatureId fid = pfId pf
        (name, coord, details) = describeFeature' (pfFeature pf)
        GeoCoord fx fy = coord
        activity = case pfActivity pf of
            Active    → "[ACTIVE]  "
            Dormant   → "[DORMANT] "
            Extinct   → "[EXTINCT] "
            Collapsed → "[COLLAPS] "
        parent = case pfParentId pf of
            Just (GeoFeatureId pid) → " parent=#" <> T.pack (show pid)
            Nothing → ""
        eruptions = if pfEruptionCount pf > 1
            then " eruptions=" <> T.pack (show (pfEruptionCount pf))
            else ""
    in "  #" <> padR 4 (T.pack (show fid))
       <> activity <> padR 20 name
       <> " (" <> T.pack (show fx) <> ", " <> T.pack (show fy) <> ") "
       <> details <> parent <> eruptions

describeFeature' ∷ VolcanicFeature → (Text, GeoCoord, Text)
describeFeature' (ShieldVolcano p) =
    ("Shield Volcano", shCenter p,
     "baseR=" <> T.pack (show (shBaseRadius p))
     <> " height=" <> T.pack (show (shPeakHeight p)))
describeFeature' (CinderCone p) =
    ("Cinder Cone", ccCenter p,
     "baseR=" <> T.pack (show (ccBaseRadius p))
     <> " height=" <> T.pack (show (ccPeakHeight p)))
describeFeature' (LavaDome p) =
    ("Lava Dome", ldCenter p,
     "baseR=" <> T.pack (show (ldBaseRadius p))
     <> " height=" <> T.pack (show (ldHeight p)))
describeFeature' (Caldera p) =
    ("Caldera", caCenter p,
     "outerR=" <> T.pack (show (caOuterRadius p))
     <> " rimH=" <> T.pack (show (caRimHeight p)))
describeFeature' (FissureVolcano p) =
    ("Fissure", fpStart p,
     "width=" <> T.pack (show (fpWidth p))
     <> " ridgeH=" <> T.pack (show (fpRidgeHeight p)))
describeFeature' (LavaTube p) =
    ("Lava Tube", ltStart p,
     "width=" <> T.pack (show (ltWidth p))
     <> " collapses=" <> T.pack (show (ltCollapses p)))
describeFeature' (SuperVolcano p) =
    ("SUPERVOLCANO", svCenter p,
     "calderaR=" <> T.pack (show (svCalderaRadius p))
     <> " ejectaR=" <> T.pack (show (svEjectaRadius p)))
describeFeature' (HydrothermalVent p) =
    ("Hydrothermal Vent", htCenter p,
     "radius=" <> T.pack (show (htRadius p))
     <> " chimneyH=" <> T.pack (show (htChimneyHeight p)))

-----------------------------------------------------------
-- IO Logging Functions
-----------------------------------------------------------

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
