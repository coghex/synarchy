-- | Final climate refinement (#785): the completed timeline's evolved
--   CO2 / solar forcing must drive the FINAL regional climate grid,
--   not a hardcoded baseline (1.0 CO2 / 0.0 offset / 1.0 solar) with
--   only the csGlobalCO2/csGlobalTemp/csSolarConst summary fields
--   patched from the timeline afterward. That old wiring left the
--   regional grid (what every tile actually reads, via
--   World.Weather.Lookup.lookupLocalClimate) computed from baseline
--   forcing while the summary fields reported evolved forcing.
--
--   The synthetic-fixture examples below hold the same standard for
--   the builder in isolation (#1379): every float it produces is
--   finite, and its absolute temperature scale is anchored to stated
--   values rather than to a second call of the function under test.
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
    , SeasonalClimate(..), ClimateCoord(..), OceanGrid(..)
    , OceanCell(..), AtmoGrid(..) )
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

-- | Finite = neither NaN nor an infinity.
--
--   This is STRICTLY stronger than the guard the deleted
--   \"determinism\" example carried by accident. That example compared
--   two bindings of one pure construction, so the only real property
--   it had was IEEE equality's refusal to equate NaN with itself — and
--   IEEE equality happily equates two same-sign infinities, so an
--   infinite field passed it. Both are rejected here, on purpose.
isFiniteFloat ∷ Float → Bool
isFiniteFloat x = not (isNaN x) ∧ not (isInfinite x)

-- | Every 'Float' one 'RegionClimate' carries, labelled by field.
--   'rcElevAvg' is an 'Int' and so can be neither NaN nor infinite.
regionFloats ∷ RegionClimate → [(String, Float)]
regionFloats rc =
    [ ("rcAirTemp.summer",       scSummer (rcAirTemp rc))
    , ("rcAirTemp.winter",       scWinter (rcAirTemp rc))
    , ("rcHumidity",             rcHumidity rc)
    , ("rcPrecipitation.summer", scSummer (rcPrecipitation rc))
    , ("rcPrecipitation.winter", scWinter (rcPrecipitation rc))
    , ("rcPrecipType",           rcPrecipType rc)
    , ("rcEvaporation",          rcEvaporation rc)
    , ("rcCloudCover",           rcCloudCover rc)
    , ("rcPressure",             rcPressure rc)
    , ("rcWindDir",              rcWindDir rc)
    , ("rcWindSpeed",            rcWindSpeed rc)
    , ("rcOrographicLift",       rcOrographicLift rc)
    , ("rcContinentality",       rcContinentality rc)
    , ("rcAlbedo",               rcAlbedo rc)
    , ("rcWaterTable.summer",    scSummer (rcWaterTable rc))
    , ("rcWaterTable.winter",    scWinter (rcWaterTable rc))
    ]

-- | Every 'Float' one 'OceanCell' carries. 'ocDepth' is an 'Int'.
oceanFloats ∷ OceanCell → [(String, Float)]
oceanFloats oc =
    [ ("ocTemperature.summer", scSummer (ocTemperature oc))
    , ("ocTemperature.winter", scWinter (ocTemperature oc))
    , ("ocSalinity",           ocSalinity oc)
    , ("ocCurrentDir",         ocCurrentDir oc)
    , ("ocCurrentSpd",         ocCurrentSpd oc)
    , ("ocUpwelling",          ocUpwelling oc)
    , ("ocIceCover",           ocIceCover oc)
    ]

-- | Every 'Float' reachable in a 'ClimateState' that
--   'buildClimateFromOceanSet' returns, each labelled by where it came
--   from so a failure names the offending field instead of reporting a
--   bare False. The collections this omits are the ones that builder
--   leaves empty; the example below asserts they really are empty, so
--   this cannot silently under-report.
climateFloats ∷ ClimateState → [(String, Float)]
climateFloats cs =
    [ ("csGlobalCO2",  csGlobalCO2 cs)
    , ("csGlobalTemp", csGlobalTemp cs)
    , ("csSolarConst", csSolarConst cs)
    ]
    ⧺ [ (show coord ⧺ "." ⧺ field, value)
      | (coord, rc) ← HM.toList (cgRegions (csClimate cs))
      , (field, value) ← regionFloats rc
      ]
    ⧺ [ (show coord ⧺ "." ⧺ field, value)
      | (coord, oc) ← HM.toList (ogCells (csOcean cs))
      , (field, value) ← oceanFloats oc
      ]

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

    it "finiteness: every float the synthetic climate produces is finite — no NaN, no infinity" $ \_env → do
        let climate = buildClimateFromOceanSet synthWorldSize synthOcean
                        synthFreshwater 1.2 0.0 1.0
        -- Coverage guard. 'climateFloats' walks the region grid and the
        -- ocean cells because those are the only float-bearing
        -- collections this builder path populates. Pin that here so a
        -- builder that starts filling one of the others fails loudly
        -- instead of leaving the finiteness sweep quietly incomplete.
        HM.size (cgRegions (csClimate climate)) `shouldBe` 16
        HM.keys (ogCells (csOcean climate)) `shouldBe` [ClimateCoord 0 0]
        HM.size (ogDeepWater (csOcean climate)) `shouldBe` 0
        length (ogCurrents (csOcean climate)) `shouldBe` 0
        length (ogThcCells (csOcean climate)) `shouldBe` 0
        HM.size (agWind (csAtmo climate)) `shouldBe` 0
        HM.size (agMoisture (csAtmo climate)) `shouldBe` 0
        length (agSystems (csAtmo climate)) `shouldBe` 0
        HM.size (csSurface climate) `shouldBe` 0
        -- The property the deleted self-comparison provided by
        -- accident, now provided on purpose and named for what it is.
        map fst (filter (not ∘ isFiniteFloat ∘ snd) (climateFloats climate))
            `shouldBe` []

    it "absolute scale: the synthetic fixture's temperatures match the pinned climate-model values" $ \_env → do
        let climate = buildClimateFromOceanSet synthWorldSize synthOcean
                        synthFreshwater 1.2 0.0 1.0
        -- The numbers below are ANCHORS, not observations: they are
        -- stated here and never obtained by calling the builder a
        -- second time, so a deterministic-but-wrong model cannot move
        -- the expectation along with the result. The other examples in
        -- this module pin a CO2 delta, global/regional agreement, and
        -- "stored forcing differs from baseline" — a grid uniformly
        -- 50 °C too hot satisfies all three and fails this one.
        --
        -- Changing either anchor is a deliberate climate-model
        -- decision, not a number to re-bless until the suite goes
        -- green: re-derive it the way it is derived here, and if the
        -- derivation no longer matches, the model changed.
        --
        -- Derivation for this fixture (worldSize 16 → 4 climate
        -- regions per side; CO2 1.2, temp offset 0.0, solar 1.0), from
        -- the constants in World.Weather.Generate.ClimateBuilder:
        --
        --   tEquator = 34 + (1.2 - 1) * 6 + 0             = 35.2 °C
        --   tMean    = tEquator - 52 * latRatio ** 1.25
        --                       + 2 * maritimeIndex
        --   seasonal = 3 + 14 * latRatio ** 1.2
        --                + 12 * (1 - maritimeIndex)
        --
        -- Region (0,0) is the fixture's sole ocean, at rv = 0, so
        -- latRatio = 1 and maritimeIndex = 1:
        --   tMean    = 35.2 - 52 + 2         = -14.8 °C
        --   seasonal = 3 + 14 + 0            =  17.0 °C
        --   summer   = -14.8 + 17.0          =   2.2 °C
        --   winter   = -14.8 - 17.0          = -31.8 °C
        --
        -- csGlobalTemp is the mean of tMean over all 16 regions. Only
        -- latitude and ocean distance vary. Before maritime warming,
        -- the eight polar-row regions (latRatio 1) sit at -16.8 °C and
        -- the eight mid-row ones (latRatio 1/3, so
        -- 52 * (1/3) ** 1.25 = 13.170) at 22.030 °C, a mean of
        -- (8 * -16.8 + 8 * 22.030) / 16 = 2.615 °C. Maritime warming
        -- adds 2 * exp(-5 * d) for BFS distance d from the ocean,
        -- which is 2 at region (0,0) itself and already under 0.014 at
        -- d = 1; over the whole grid it totals 2.041, i.e. +0.128 on
        -- the mean. 2.615 + 0.128 = 2.742 °C, and the builder's own
        -- single-precision arithmetic lands on 2.7423.
        csGlobalTemp climate `shouldSatisfy` closeTo 0.01 2.7423
        -- The anchor is load-bearing rather than merely true: a grid
        -- offset uniformly in temperature misses it. globalTempOffset
        -- shifts tEquator, and so every region's mean, by the same
        -- amount before the per-latitude falloff — which is exactly
        -- the uniform regional error the other three examples in this
        -- module cannot see.
        let uniformlyOffset = buildClimateFromOceanSet synthWorldSize
                                synthOcean synthFreshwater 1.2 5.0 1.0
        csGlobalTemp uniformlyOffset `shouldNotSatisfy` closeTo 0.01 2.7423
        -- One named region as well, so a regression that redistributes
        -- heat between regions while preserving the global mean is
        -- caught too. Deliberately ONE region: the grid is not frozen.
        case HM.lookup (ClimateCoord 0 0) (cgRegions (csClimate climate)) of
            Nothing → expectationFailure
                "expected a region at the synthetic ocean coord (0,0)"
            Just oceanRegion → do
                scSummer (rcAirTemp oceanRegion) `shouldSatisfy` closeTo 0.01 2.2
                scWinter (rcAirTemp oceanRegion) `shouldSatisfy` closeTo 0.01 (-31.8)

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
