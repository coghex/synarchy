{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Log
    ( -- * Pure formatters (return [Text])
      formatHydroSummary
    , formatHydroNormal
    , formatHydroVerbose
    ) where

import UPrelude
import qualified Data.Text as T
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Hydrology.Types

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

padR ∷ Int → Text → Text
padR n t = t <> T.replicate (max 0 (n - T.length t)) " "

-----------------------------------------------------------
-- Feature Counting
-----------------------------------------------------------

data HydroSummaryStats = HydroSummaryStats
    { hsRivers    ∷ !Int
    , hsGlaciers  ∷ !Int
    , hsLakes     ∷ !Int
    , hsOther     ∷ !Int
    , hsTotalFlow ∷ !Float
    } deriving (Show)

gatherStats ∷ [PersistentFeature] → HydroSummaryStats
gatherStats features =
    let hydroFeatures = [ f | f ← features
                        , isHydroFeature (pfFeature f) ]
        rivers   = length [ () | f ← hydroFeatures
                           , isRiver (pfFeature f) ]
        glaciers = length [ () | f ← hydroFeatures
                           , isGlacier (pfFeature f) ]
        lakes    = length [ () | f ← hydroFeatures
                           , isLake (pfFeature f) ]
        other    = length hydroFeatures - rivers - glaciers - lakes
        totalFlow = sum [ rpFlowRate p
                        | f ← hydroFeatures
                        , HydroShape (RiverFeature p) ← [pfFeature f] ]
    in HydroSummaryStats rivers glaciers lakes other totalFlow

isHydroFeature ∷ FeatureShape → Bool
isHydroFeature (HydroShape _) = True
isHydroFeature _              = False

isRiver ∷ FeatureShape → Bool
isRiver (HydroShape (RiverFeature _)) = True
isRiver _                             = False

isGlacier ∷ FeatureShape → Bool
isGlacier (HydroShape (GlacierFeature _)) = True
isGlacier _                               = False

isLake ∷ FeatureShape → Bool
isLake (HydroShape (LakeFeature _)) = True
isLake _                            = False

-----------------------------------------------------------
-- Summary (one box, counts only)
-----------------------------------------------------------

-- | One-box summary: feature counts and total river flow.
formatHydroSummary ∷ [PersistentFeature] → [Text]
formatHydroSummary features =
    let stats = gatherStats features
    in [ "╔══════════════════════════════════════════════════════════╗"
       , "║              HYDROLOGICAL SUMMARY                      ║"
       , "╠══════════════════════════════════════════════════════════╣"
       , "║  Rivers:                " <> padR 31 (T.pack (show (hsRivers stats))) <> "║"
       , "║  Glaciers:              " <> padR 31 (T.pack (show (hsGlaciers stats))) <> "║"
       , "║  Lakes:                 " <> padR 31 (T.pack (show (hsLakes stats))) <> "║"
       , "║  Other hydro features:  " <> padR 31 (T.pack (show (hsOther stats))) <> "║"
       , "║  Total river flow:      " <> padR 31 (T.pack (showFF1 (hsTotalFlow stats))) <> "║"
       , "╚══════════════════════════════════════════════════════════╝"
       , ""
       ]

-----------------------------------------------------------
-- Normal (summary + per-feature one-liners)
-----------------------------------------------------------

-- | Summary box + a compact one-liner per hydrological feature.
formatHydroNormal ∷ [PersistentFeature] → [Text]
formatHydroNormal features =
    let summaryLines = formatHydroSummary features
        hydroFeats   = filter (isHydroFeature . pfFeature) features
        header       = "═══ Hydrological Features (" <> T.pack (show (length hydroFeats)) <> ") ═══"
        featureLines = map formatOneHydroFeature hydroFeats
    in summaryLines <> [header] <> featureLines <> [""]

-----------------------------------------------------------
-- Verbose (summary + full detail per feature)
-----------------------------------------------------------

-- | Summary box + full detail including activity, parent, evolution counts.
formatHydroVerbose ∷ [PersistentFeature] → [Text]
formatHydroVerbose features =
    let summaryLines = formatHydroSummary features
        hydroFeats   = filter (isHydroFeature . pfFeature) features
        header       = "═══ Hydrological Features (verbose) ═══"
        featureLines = concatMap formatOneHydroFeatureVerbose hydroFeats
    in summaryLines <> ["", header] <> featureLines

-----------------------------------------------------------
-- Single-feature formatters
-----------------------------------------------------------

formatOneHydroFeature ∷ PersistentFeature → Text
formatOneHydroFeature pf =
    let GeoFeatureId fid = pfId pf
        (tag, coord, detail) = describeHydroFeature (pfFeature pf)
        GeoCoord fx fy = coord
        actTag = case pfActivity pf of
            FActive    → "[ACTIVE]  "
            FDormant   → "[DORMANT] "
            FExtinct   → "[EXTINCT] "
            FCollapsed → "[COLLAPS] "
    in "  #" <> padR 4 (T.pack (show fid))
       <> actTag <> padR 20 tag
       <> " (" <> T.pack (show fx) <> ", " <> T.pack (show fy) <> ") "
       <> detail

formatOneHydroFeatureVerbose ∷ PersistentFeature → [Text]
formatOneHydroFeatureVerbose pf =
    let GeoFeatureId fid = pfId pf
        (tag, coord, detail) = describeHydroFeature (pfFeature pf)
        GeoCoord fx fy = coord
        actLine = "    Activity: " <> T.pack (show (pfActivity pf))
        parentLine = case pfParentId pf of
            Just (GeoFeatureId pid) → "    Parent feature: #" <> T.pack (show pid)
            Nothing                 → "    Parent feature: (none)"
        eruptLine = "    Eruption/activation count: " <> T.pack (show (pfEruptionCount pf))
    in [ "  ── " <> tag <> " #" <> T.pack (show fid)
         <> " (" <> T.pack (show fx) <> ", " <> T.pack (show fy) <> ")"
       , "    " <> detail
       , actLine
       , parentLine
       , eruptLine
       ]

-- | Describe a hydrological feature shape.
--   Returns (label, representative coord, detail string).
describeHydroFeature ∷ FeatureShape → (Text, GeoCoord, Text)
describeHydroFeature (HydroShape (RiverFeature p)) =
    ( "River"
    , rpSourceRegion p
    , "mouth=" <> T.pack (show (rpMouthRegion p))
      <> " flow=" <> T.pack (showFF1 (rpFlowRate p))
      <> " segs=" <> T.pack (show (length (rpSegments p)))
    )
describeHydroFeature (HydroShape (GlacierFeature p)) =
    ( "Glacier"
    , glCenter p
    , "len=" <> T.pack (show (glLength p))
      <> " width=" <> T.pack (show (glWidth p))
      <> " thick=" <> T.pack (show (glThickness p))
    )
describeHydroFeature (HydroShape (LakeFeature p)) =
    ( "Lake"
    , lkCenter p
    , "r=" <> T.pack (show (lkRadius p))
      <> " surf=" <> T.pack (show (lkSurface p))
      <> " depth=" <> T.pack (show (lkDepth p))
    )
describeHydroFeature other =
    -- Non-hydro shapes shouldn't reach here, but handled gracefully
    ( "Non-hydro feature"
    , GeoCoord 0 0
    , T.pack (show other)
    )

-----------------------------------------------------------
-- Float formatting
-----------------------------------------------------------

showFF1 ∷ Float → String
showFF1 f =
    let whole = floor f ∷ Int
        frac  = round ((f - fromIntegral whole) * 10.0) ∷ Int
    in show whole <> "." <> show (abs frac)
