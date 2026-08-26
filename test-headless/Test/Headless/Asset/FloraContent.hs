-- | Production flora-content coverage. Unlike the pure growth fixtures, this
--   loads the shipped YAML so an approved texture family cannot remain
--   unregistered without a focused test noticing.
module Test.Headless.Asset.FloraContent (spec) where

import UPrelude
import Test.Hspec
import Engine.Asset.YamlFlora
import Engine.Asset.YamlMaterials (loadPopulatedMaterialRegistry)
import Engine.Core.Log
    (LogBackend(..), LogConfig(..), defaultLogConfig, initLogger)
import World.Flora.Placement (speciesFitnessDetail)
import World.Flora.Types (FloraWorldGen(..))
import World.Material
    (MaterialId(..), MaterialRegistry, materialIdByName)

spec ∷ Spec
spec = describe "saguaro flora content" $ do
    it "registers the approved texture family as decorative desert flora" $ do
        logger ← initLogger defaultLogConfig
            { lcBackend = LogToCallback (\_ → pure ()) }
        defs ← loadFloraYaml logger "data/flora/saguaro.yaml"
        registry ← loadPopulatedMaterialRegistry logger "data/materials"
        case defs of
            [def] → assertSaguaro registry def
            _ → expectationFailure $
                "expected exactly one saguaro definition, got " ⧺ show (length defs)

assertSaguaro ∷ MaterialRegistry → FloraYamlDef → Expectation
assertSaguaro registry def = do
    fydName def `shouldBe` "saguaro"
    fydType def `shouldBe` "cactus"
    fydTexDir def `shouldBe` "assets/textures/flora/saguaro"
    fydLifecycle def `shouldBe` "perennial"
    fydMinLife def `shouldBe` Just 18000
    fydMaxLife def `shouldBe` Just 54000
    fydDeathChance def `shouldBe` Just 0.01
    fydHarvest def `shouldBe` Nothing

    map (\p → (fypTag p, fypTexture p, fypAge p)) (fydPhases def)
        `shouldBe`
            [ ("sprout", "sprout.png", 0)
            , ("matured", "matured.png", 1800)
            , ("dead", "dead.png", 54000)
            ]

    map (\c → (fycsTag c, fycsStartDay c, fycsTexture c))
            (fydAnnualCycle def)
        `shouldBe`
            [ ("dormant", 0, "matured.png")
            , ("budding", 90, "matured.png")
            , ("flowering", 120, "matured_flowering.png")
            , ("fruiting", 150, "matured_fruiting.png")
            , ("senescing", 240, "matured.png")
            ]

    map (\o → (fycoPhase o, fycoCycle o, fycoTexture o))
            (fydCycleOverrides def)
        `shouldBe`
            [ (phase, cycle, texture)
            | phase ← ["sprout", "dead"]
            , cycle ← ["dormant", "budding", "flowering", "fruiting", "senescing"]
            , let texture = if phase ≡ "sprout" then "sprout.png" else "dead.png"
            ]

    let wg = fydWorldGen def
    fywCategory wg `shouldBe` "cactus"
    (fywMinTemp wg, fywIdealTemp wg, fywMaxTemp wg)
        `shouldBe` (18, 30, 45)
    (fywMinPrecip wg, fywIdealPrecip wg, fywMaxPrecip wg)
        `shouldBe` (0.02, 0.22, 0.30)
    (fywMinAlt wg, fywIdealAlt wg, fywMaxAlt wg)
        `shouldBe` (Just (-20), Just 120, Just 500)
    (fywMinHumidity wg, fywIdealHumidity wg, fywMaxHumidity wg)
        `shouldBe` (Just 0.02, Just 0.18, Just 0.55)
    fywMaxSlope wg `shouldBe` Just 3
    fywDensity wg `shouldBe` Just 0.08
    fywFootprint wg `shouldBe` Just 14
    fywSoils wg `shouldBe`
        ["sandy_loam", "sandy_clay_loam", "loamy_sand"]

    let resolvedSoils = map (materialIdByName registry) (fywSoils wg)
    resolvedSoils `shouldSatisfy` all isJust
    case [unMaterialId mid | Just mid ← resolvedSoils] of
        [sandyLoam, sandyClayLoam, loamySand] → do
            let runtimeWG = toRuntimeWorldGen wg
                    [sandyLoam, sandyClayLoam, loamySand]
                score soil precip = fst $ speciesFitnessDetail runtimeWG
                    soil 0 30 precip 0.18 120
                primaryScore = score sandyLoam 0.22
                clayScore = score sandyClayLoam 0.27
                sandScore = score loamySand 0.18
            clayScore `shouldSatisfy` (> 0)
            sandScore `shouldSatisfy` (> 0)
            primaryScore `shouldSatisfy` (> clayScore)
            primaryScore `shouldSatisfy` (> sandScore)
        _ → expectationFailure "expected all three saguaro soils to resolve"

toRuntimeWorldGen ∷ FloraYamlWorldGen → [Word8] → FloraWorldGen
toRuntimeWorldGen wg soilIds =
    let minAlt = fromMaybe (-100) (fywMinAlt wg)
        maxAlt = fromMaybe 800 (fywMaxAlt wg)
        minHumidity = fromMaybe 0 (fywMinHumidity wg)
        maxHumidity = fromMaybe 1 (fywMaxHumidity wg)
    in FloraWorldGen
        { fwCategory = fywCategory wg
        , fwMinTemp = fywMinTemp wg
        , fwMaxTemp = fywMaxTemp wg
        , fwIdealTemp = fywIdealTemp wg
        , fwMinPrecip = fywMinPrecip wg
        , fwMaxPrecip = fywMaxPrecip wg
        , fwIdealPrecip = fywIdealPrecip wg
        , fwMinAlt = minAlt
        , fwMaxAlt = maxAlt
        , fwIdealAlt = fromMaybe ((minAlt + maxAlt) `div` 2) (fywIdealAlt wg)
        , fwMinHumidity = minHumidity
        , fwMaxHumidity = maxHumidity
        , fwIdealHumidity = fromMaybe ((minHumidity + maxHumidity) / 2)
                              (fywIdealHumidity wg)
        , fwMaxSlope = maybe 15 fromIntegral (fywMaxSlope wg)
        , fwDensity = fromMaybe 0.1 (fywDensity wg)
        , fwSoils = soilIds
        , fwFootprint = fromMaybe 0 (fywFootprint wg)
        }
