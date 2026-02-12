{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Log
    ( logTimeline
    , logTimelineSummary
    , formatTimeline
    ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import World.Types
import World.Geology.Types

-----------------------------------------------------------
-- Timeline Summary
-----------------------------------------------------------

-- | Summary statistics for a geological timeline.
data TimelineSummary = TimelineSummary
    { tsNumPeriods      ∷ !Int
    , tsTotalEvents     ∷ !Int
    , tsTotalDuration   ∷ !Int       -- ^ Sum of all period durations (MY)
    , tsEventCounts     ∷ !(Map.Map Text Int)
    , tsScaleCounts     ∷ !(Map.Map GeoScale Int)
    , tsFeatureCount    ∷ !Int
    , tsActiveFeatures  ∷ !Int
    , tsDormantFeatures ∷ !Int
    , tsExtinctFeatures ∷ !Int
    } deriving (Show)

-- | Build a summary from a timeline.
summarizeTimeline ∷ GeoTimeline → TimelineSummary
summarizeTimeline tl =
    let periods = gtPeriods tl
        features = gtFeatures tl

        allEvents = concatMap gpEvents periods

        eventCounts = foldl' countEvent Map.empty allEvents
        scaleCounts = foldl' (\m p → Map.insertWith (+) (gpScale p) 1 m)
                             Map.empty periods

        (active, dormant, extinct) = foldl' countActivity (0, 0, 0) features

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
        }

countEvent ∷ Map.Map Text Int → GeoEvent → Map.Map Text Int
countEvent m (CraterEvent _)         = Map.insertWith (+) "Craters" 1 m
countEvent m (VolcanicEvent f)       = Map.insertWith (+) (featureTypeName f) 1 m
countEvent m (VolcanicModify _ evo)  = Map.insertWith (+) (evolutionName evo) 1 m
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
evolutionName (Reactivate _ _)        = "Reactivations"
evolutionName GoDormant               = "Went Dormant"
evolutionName GoExtinct               = "Went Extinct"
evolutionName (CollapseToCaldera _ _) = "Caldera Collapses"
evolutionName (ParasiticEruption _ _) = "Parasitic Eruptions"
evolutionName (FlankCollapse _ _ _)   = "Flank Collapses"

countActivity ∷ (Int, Int, Int) → PersistentFeature → (Int, Int, Int)
countActivity (a, d, e) pf = case pfActivity pf of
    Active    → (a + 1, d, e)
    Dormant   → (a, d + 1, e)
    Extinct   → (a, d, e + 1)
    Collapsed → (a, d, e + 1)  -- collapsed counts as extinct for summary

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
-- Full Timeline Formatting
-----------------------------------------------------------

-- | Format the entire timeline as a structured text log.
--   Shows the summary header, then each period with its events
--   grouped by scale.
formatTimeline ∷ GeoTimeline → Text
formatTimeline tl =
    let summary = summarizeTimeline tl
        header = formatSummary summary
        periods = gtPeriods tl
        body = T.intercalate "\n" (zipWith formatPeriod [0..] periods)
        features = formatFeatureList (gtFeatures tl)
    in header <> "\n" <> body <> "\n" <> features

-- | Format the summary header.
formatSummary ∷ TimelineSummary → Text
formatSummary ts = T.unlines
    [ "╔══════════════════════════════════════════════════════════╗"
    , "║              GEOLOGICAL TIMELINE SUMMARY                ║"
    , "╠══════════════════════════════════════════════════════════╣"
    , "║  Total geological time: " <> padR 31 (T.pack (show (tsTotalDuration ts)) <> " MY") <> ""
    , "║  Timeline periods:      " <> padR 31 (T.pack (show (tsNumPeriods ts))) <> "║"
    , "║  Total events:          " <> padR 31 (T.pack (show (tsTotalEvents ts))) <> "║"
    , "║  Persistent features:   " <> padR 31 (T.pack (show (tsFeatureCount ts))) <> "║"
    , "║    Active:              " <> padR 31 (T.pack (show (tsActiveFeatures ts))) <> "║"
    , "║    Dormant:             " <> padR 31 (T.pack (show (tsDormantFeatures ts))) <> "║"
    , "║    Extinct/Collapsed:   " <> padR 31 (T.pack (show (tsExtinctFeatures ts))) <> "║"
    , "╠══════════════════════════════════════════════════════════╣"
    , "║  Scale breakdown:                                       ║"
    , formatScaleCounts (tsScaleCounts ts)
    , "╠═════════════════════════════════════════════════════════╣"
    , "║  Event breakdown:                                       ║"
    , formatEventCounts (tsEventCounts ts)
    , "╚══════════════════════════════════════════════════════════╝"
    ]

padR ∷ Int → Text → Text
padR n t = t <> T.replicate (max 0 (n - T.length t)) " "

formatScaleCounts ∷ Map.Map GeoScale Int → Text
formatScaleCounts m =
    T.intercalate "\n"
        [ "║    " <> padR 22 (showScale s) <> padR 32 (T.pack (show c)) <> "║"
        | (s, c) ← Map.toAscList m
        ]

formatEventCounts ∷ Map.Map Text Int → Text
formatEventCounts m =
    T.intercalate "\n"
        [ "║    " <> padR 22 name <> padR 32 (T.pack (show c)) <> "║"
        | (name, c) ← Map.toAscList m
        ]

-----------------------------------------------------------
-- Period Formatting
-----------------------------------------------------------

-- | Format a single period. Shows scale indicator, name,
--   duration, and a compact event list.
formatPeriod ∷ Int → GeoPeriod → Text
formatPeriod idx period =
    let scaleBar = case gpScale period of
            Eon    → "████"
            Era    → " ███"
            Period → "  ██"
            Epoch  → "   █"
            Age    → "    "
        events = gpEvents period
        eventSummary = if null events
            then "  (erosion only)"
            else "  " <> formatEventList events
        durationStr = T.pack (show (gpDuration period)) <> " MY"
    in scaleBar <> " ["
       <> padR 3 (T.pack (show idx))
       <> "] "
       <> padR 28 (gpName period)
       <> padR 8 durationStr
       <> eventSummary

-- | Compact event list for a single period.
formatEventList ∷ [GeoEvent] → Text
formatEventList events =
    let counts = foldl' countEvent Map.empty events
        parts = [ T.pack (show c) <> "× " <> name
                | (name, c) ← Map.toAscList counts
                ]
    in T.intercalate ", " parts

-----------------------------------------------------------
-- Feature List
-----------------------------------------------------------

formatFeatureList ∷ [PersistentFeature] → Text
formatFeatureList features =
    let header = "─── Persistent Features (" <> T.pack (show (length features)) <> ") ───"
        lines' = map formatOneFeature features
    in T.unlines (header : lines')

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

-- | Describe feature for logging (same as Thread.hs version
--   but pure Text, no IO dependency).
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

-- | Log the full timeline using the engine's logger.
--   Splits the formatted text into lines and logs each one.
logTimeline ∷ MonadIO m ⇒ (Text → m ()) → GeoTimeline → m ()
logTimeline logFn tl =
    let formatted = formatTimeline tl
        logLines = T.lines formatted
    in mapM_ logFn logLines

-- | Log just the summary (shorter output).
logTimelineSummary ∷ MonadIO m ⇒ (Text → m ()) → GeoTimeline → m ()
logTimelineSummary logFn tl =
    let summary = summarizeTimeline tl
        formatted = formatSummary summary
        logLines = T.lines formatted
    in mapM_ logFn logLines
