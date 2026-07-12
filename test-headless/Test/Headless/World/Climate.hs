{-# LANGUAGE UnicodeSyntax #-}
-- | Final climate refinement (#785): the completed timeline's evolved
--   CO2 / solar forcing must drive the FINAL regional climate grid,
--   not a hardcoded baseline (1.0 CO2 / 0.0 offset / 1.0 solar) with
--   only the csGlobalCO2/csGlobalTemp/csSolarConst summary fields
--   patched from the timeline afterward. That old wiring left the
--   regional grid (what every tile actually reads, via
--   World.Weather.Lookup.lookupLocalClimate) computed from baseline
--   forcing while the summary fields reported evolved forcing.
module Test.Headless.World.Climate (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Engine.Core.State (EngineEnv)
import Test.Headless.Harness (sharedWorld, getWorldGenParams)
import World.Types (WorldGenParams(..))
import World.Weather.Types
    ( ClimateState(..), ClimateGrid(..), RegionClimate(..)
    , SeasonalClimate(..), ClimateCoord(..) )
import World.Weather.Generate (initEarlyClimate)
import World.Weather.Generate.ClimateBuilder (buildClimateFromOceanSet)

-- | A small synthetic world: one ocean region at the origin, the rest
--   land, no freshwater sources. Big enough (climateRegionCount for a
--   16-chunk world) to exercise the maritime BFS without real worldgen.
synthWorldSize ∷ Int
synthWorldSize = 16

synthOcean ∷ HS.HashSet ClimateCoord
synthOcean = HS.singleton (ClimateCoord 0 0)

synthFreshwater ∷ HM.HashMap ClimateCoord Float
synthFreshwater = HM.empty

-- | Mean annual temperature over every region, computed directly from
--   the per-region seasonal split — the same quantity
--   'buildClimateFromOceanSet' averages into 'csGlobalTemp'
--   (rcAirTemp's summer/winter mean), NOT a bilinear per-tile lookup.
regionMeanTemp ∷ ClimateState → Float
regionMeanTemp cs =
    let regions = HM.elems (cgRegions (csClimate cs))
        annualMean rc = let SeasonalClimate summer winter = rcAirTemp rc
                        in (summer + winter) / 2.0
    in if null regions then 0.0
       else sum (map annualMean regions) / fromIntegral (length regions)

-- | Both floats are Float (single precision) means over ~dozens to a
--   few hundred regions, so allow a small tolerance rather than exact
--   equality.
closeTo ∷ Float → Float → Float → Bool
closeTo tol a b = abs (a - b) < tol

spec ∷ SpecWith EngineEnv
spec = describe "Final climate refinement" $ do

    it "forcing sensitivity: higher CO2 produces warmer regional temperatures" $ \_env → do
        let climateLowCO2  = buildClimateFromOceanSet synthWorldSize synthOcean
                                synthFreshwater 1.0 0.0 1.0
            climateHighCO2 = buildClimateFromOceanSet synthWorldSize synthOcean
                                synthFreshwater 1.5 0.0 1.0
            -- co2TempBoost = (globalCO2 - 1.0) * 6.0 is added uniformly
            -- to every region before the per-latitude falloff, so the
            -- global mean should shift by exactly 0.5 * 6.0 = 3.0 °C.
            delta = csGlobalTemp climateHighCO2 - csGlobalTemp climateLowCO2
        delta `shouldSatisfy` closeTo 0.01 3.0
        csGlobalTemp climateHighCO2 `shouldSatisfy` (> csGlobalTemp climateLowCO2)

    it "global/regional consistency: csGlobalTemp is the mean of the region grid it was built with" $ \_env → do
        let climate = buildClimateFromOceanSet synthWorldSize synthOcean
                        synthFreshwater 1.2 0.0 1.0
        csGlobalTemp climate `shouldSatisfy`
            closeTo 0.01 (regionMeanTemp climate)

    it "determinism: identical inputs produce an identical ClimateState" $ \_env → do
        let a = buildClimateFromOceanSet synthWorldSize synthOcean
                    synthFreshwater 1.2 0.0 1.0
            b = buildClimateFromOceanSet synthWorldSize synthOcean
                    synthFreshwater 1.2 0.0 1.0
        a `shouldBe` b

    it "integration wiring: a completed world stores regional climate rebuilt from the timeline's final forcing, not baseline" $ \env → do
        -- Reuses the canonical shared world other worldgen specs
        -- already pay for — no extra generation cost.
        ws ← sharedWorld env 42 64 3
        mParams ← getWorldGenParams ws
        case mParams of
            Nothing → expectationFailure "expected generated world params"
            Just params → do
                let stored = wgpClimateState params
                    worldSize = wgpWorldSize params
                    oceanMap = wgpOceanMap params
                    timeline = wgpGeoTimeline params

                -- (1) The stored summary forcing genuinely evolved away
                -- from baseline (CO2 1.0) — rules out a regression
                -- where BOTH the regional grid and the summary fields
                -- are built from hardcoded baseline forcing (which
                -- would otherwise look self-consistent).
                csGlobalCO2 stored `shouldNotSatisfy` closeTo 0.05 1.0

                -- (2) Rebuilding via the same public final-refinement
                -- function, fed the SAME ocean map / completed
                -- timeline plus the STORED final forcing, reproduces
                -- the exact stored climate. This proves the stored
                -- regional grid — not just its mean — was actually
                -- built from the final ocean/freshwater inputs and
                -- final CO2/solar forcing, rather than some other
                -- grid with a coincidentally-matching mean.
                let rebuiltFromFinal = initEarlyClimate worldSize oceanMap
                        timeline (csGlobalCO2 stored) (csSolarConst stored)
                rebuiltFromFinal `shouldBe` stored

                -- (3) Explicitly: that same rebuild recipe, fed
                -- hardcoded BASELINE forcing instead, produces a
                -- DIFFERENT climate — so (2) is not a vacuous check
                -- that would pass regardless of which forcing was
                -- used.
                let rebuiltFromBaseline = initEarlyClimate worldSize
                        oceanMap timeline 1.0 1.0
                rebuiltFromBaseline `shouldNotBe` stored
