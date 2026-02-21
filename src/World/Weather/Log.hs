{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Weather.Log
    ( formatWeather
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import World.Weather.Types

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

padR ∷ Int → Text → Text
padR n t = t <> T.replicate (max 0 (n - T.length t)) " "

showF1 ∷ Float → Text
showF1 f =
    let whole = floor f ∷ Int
        frac  = abs (round ((f - fromIntegral whole) * 10.0) ∷ Int)
    in T.pack (show whole) <> "." <> T.pack (show frac)

showSeasonal ∷ SeasonalClimate → Text
showSeasonal (SeasonalClimate s w) =
    "summer=" <> showF1 s <> " winter=" <> showF1 w

-----------------------------------------------------------
-- Public API
-----------------------------------------------------------

-- | Format the entire climate state as a list of log lines.
--   Mirrors the style of formatTimeline in World.Geology.Log.
formatWeather ∷ ClimateState → [Text]
formatWeather cs =
    let climateLines  = formatClimateGrid (csClimate cs)
        oceanLines    = formatOceanGrid (csOcean cs)
        atmoLines     = formatAtmoGrid (csAtmo cs)
        surfaceLines  = formatSurfaceBudgets (csSurface cs)
        globalLines   = formatGlobals cs
    in globalLines <> oceanLines <> atmoLines <> climateLines <> surfaceLines

-----------------------------------------------------------
-- Global Summary
-----------------------------------------------------------

formatGlobals ∷ ClimateState → [Text]
formatGlobals cs =
    [ "╔══════════════════════════════════════════════════════════╗"
    , "║                CLIMATE STATE SUMMARY                    ║"
    , "╠══════════════════════════════════════════════════════════╣"
    , "║  Global CO₂:            " <> padR 31 (showF1 (csGlobalCO2 cs)) <> "║"
    , "║  Global temp offset:    " <> padR 31 (showF1 (csGlobalTemp cs) <> " °C") <> "║"
    , "║  Solar constant:        " <> padR 31 (showF1 (csSolarConst cs)) <> "║"
    , "╚══════════════════════════════════════════════════════════╝"
    , ""
    ]

-----------------------------------------------------------
-- Ocean Grid Summary
-----------------------------------------------------------

formatOceanGrid ∷ OceanGrid → [Text]
formatOceanGrid og =
    let cellCount = HM.size (ogCells og)
        deepCount = HM.size (ogDeepWater og)
        currentCount = length (ogCurrents og)
        thcCount = length (ogThcCells og)
        header = "═══ Ocean Grid ═══"
        stats  = "  Cells: " <> T.pack (show cellCount)
              <> "  Deep water masses: " <> T.pack (show deepCount)
              <> "  Named currents: " <> T.pack (show currentCount)
              <> "  THC loops: " <> T.pack (show thcCount)
        -- Show a sample of ocean cells (first 5)
        sampleCells = take 5 $ HM.toList (ogCells og)
        cellLines = map formatOceanCellSample sampleCells
    in [header, stats] <> cellLines
       <> (if cellCount > 5 then ["  ... (" <> T.pack (show (cellCount - 5)) <> " more)"] else [])
       <> [""]

formatOceanCellSample ∷ (ClimateCoord, OceanCell) → Text
formatOceanCellSample (ClimateCoord cx cy, oc) =
    "  (" <> T.pack (show cx) <> "," <> T.pack (show cy) <> ")"
    <> " SST=" <> showSeasonal (ocTemperature oc)
    <> " sal=" <> showF1 (ocSalinity oc)
    <> " depth=" <> T.pack (show (ocDepth oc))
    <> " ice=" <> showF1 (ocIceCover oc)

-----------------------------------------------------------
-- Atmosphere Grid Summary
-----------------------------------------------------------

formatAtmoGrid ∷ AtmoGrid → [Text]
formatAtmoGrid ag =
    let windCount = HM.size (agWind ag)
        moistCount = HM.size (agMoisture ag)
        sysCount = length (agSystems ag)
        header = "═══ Atmospheric Circulation ═══"
        stats  = "  Wind cells: " <> T.pack (show windCount)
              <> "  Moisture cells: " <> T.pack (show moistCount)
              <> "  Pressure systems: " <> T.pack (show sysCount)
    in [header, stats, ""]

-----------------------------------------------------------
-- Climate Grid Summary
-----------------------------------------------------------

formatClimateGrid ∷ ClimateGrid → [Text]
formatClimateGrid cg =
    let regionCount = HM.size (cgRegions cg)
        header = "═══ Climate Regions (" <> T.pack (show regionCount) <> ") ═══"
        -- Show a sample of regions
        sampleRegions = take 8 $ HM.toList (cgRegions cg)
        regionLines = map formatRegionClimate sampleRegions
    in [header] <> regionLines
       <> (if regionCount > 8 then ["  ... (" <> T.pack (show (regionCount - 8)) <> " more)"] else [])
       <> [""]

formatRegionClimate ∷ (ClimateCoord, RegionClimate) → Text
formatRegionClimate (ClimateCoord cx cy, rc) =
    "  (" <> T.pack (show cx) <> "," <> T.pack (show cy) <> ")"
    <> " temp=" <> showSeasonal (rcAirTemp rc)
    <> " humid=" <> showF1 (rcHumidity rc)
    <> " precip=" <> showSeasonal (rcPrecipitation rc)
    <> " cloud=" <> showF1 (rcCloudCover rc)
    <> " elev=" <> T.pack (show (rcElevAvg rc))

-----------------------------------------------------------
-- Surface Budgets Summary
-----------------------------------------------------------

formatSurfaceBudgets ∷ HM.HashMap ClimateCoord SurfaceBudget → [Text]
formatSurfaceBudgets sb =
    let count = HM.size sb
        header = "═══ Surface Budgets (" <> T.pack (show count) <> ") ═══"
    in if count ≡ 0
       then [header, "  (empty — not yet computed)", ""]
       else let samples = take 5 $ HM.toList sb
                sampleLines = map formatBudgetSample samples
            in [header] <> sampleLines
               <> (if count > 5 then ["  ... (" <> T.pack (show (count - 5)) <> " more)"] else [])
               <> [""]

formatBudgetSample ∷ (ClimateCoord, SurfaceBudget) → Text
formatBudgetSample (ClimateCoord cx cy, sb) =
    "  (" <> T.pack (show cx) <> "," <> T.pack (show cy) <> ")"
    <> " " <> T.pack (show (sbSurfaceType sb))
    <> " albedo=" <> showF1 (sbAlbedo sb)
    <> " net=" <> showF1 (sbNetMoisture sb)
    <> " runoff=" <> showF1 (sbRunoff sb)
