-- | Production flora-content coverage. Unlike the pure growth fixtures, this
--   loads the shipped YAML so an approved texture family cannot remain
--   unregistered without a focused test noticing.
module Test.Headless.Asset.FloraContent (spec) where

import UPrelude
import Test.Hspec
import Engine.Asset.YamlFlora
import Engine.Core.Log
    (LogBackend(..), LogConfig(..), defaultLogConfig, initLogger)

spec ∷ Spec
spec = describe "saguaro flora content" $ do
    it "registers the approved texture family as decorative desert flora" $ do
        logger ← initLogger defaultLogConfig
            { lcBackend = LogToCallback (\_ → pure ()) }
        defs ← loadFloraYaml logger "data/flora/saguaro.yaml"
        case defs of
            [def] → assertSaguaro def
            _ → expectationFailure $
                "expected exactly one saguaro definition, got " ⧺ show (length defs)

assertSaguaro ∷ FloraYamlDef → Expectation
assertSaguaro def = do
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
        `shouldBe` (0.02, 0.10, 0.30)
    (fywMinAlt wg, fywIdealAlt wg, fywMaxAlt wg)
        `shouldBe` (Just (-20), Just 120, Just 500)
    (fywMinHumidity wg, fywIdealHumidity wg, fywMaxHumidity wg)
        `shouldBe` (Just 0.02, Just 0.18, Just 0.55)
    fywMaxSlope wg `shouldBe` Just 3
    fywDensity wg `shouldBe` Just 0.08
    fywFootprint wg `shouldBe` Just 14
    fywSoils wg `shouldBe`
        ["sand", "loamy_sand", "sandy_loam", "light_gravel", "heavy_gravel"]
