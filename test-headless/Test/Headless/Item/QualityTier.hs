{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Pure tests for the quality→label tier resolution (#345): the
--   default band table's boundaries, an item def's own
--   `quality_tiers:` override taking precedence, and the YAML schema
--   for that override list.
module Test.Headless.Item.QualityTier (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import Item.Types (ItemDef(..), QualityTier(..), qualityTierLabel)
import Engine.Asset.YamlItems (ItemYamlQualityTier(..))
import Engine.Asset.Handle (TextureHandle(..))

-- | A minimal ItemDef stand-in — only idQualityTiers matters here.
blankDef ∷ [QualityTier] → ItemDef
blankDef tiers = ItemDef
    { idName = "test_item", idDisplayName = "Test Item"
    , idTexture = TextureHandle 0, idWeight = 0, idWeightSpec = Nothing
    , idKind = "misc", idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Just (0, 100), idConditionSpec = Nothing
    , idQualityTiers = tiers
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = [], idInsulation = 0
    }

decode ∷ BS.ByteString → Either String ItemYamlQualityTier
decode = either (Left . show) Right . Yaml.decodeEither'

spec ∷ Spec
spec = do
    describe "qualityTierLabel (default table)" $ do
        let def = blankDef []
        it "labels 100 as excellent" $
            qualityTierLabel def 100 `shouldBe` Just "excellent"
        it "labels the 90 boundary as excellent" $
            qualityTierLabel def 90 `shouldBe` Just "excellent"
        it "labels just under 90 as good" $
            qualityTierLabel def 89.9 `shouldBe` Just "good"
        it "labels the 75 boundary as good" $
            qualityTierLabel def 75 `shouldBe` Just "good"
        it "labels just under 75 as average" $
            qualityTierLabel def 74.9 `shouldBe` Just "average"
        it "labels the 50 boundary as average" $
            qualityTierLabel def 50 `shouldBe` Just "average"
        it "labels just under 50 as bad" $
            qualityTierLabel def 49.9 `shouldBe` Just "bad"
        it "labels the 25 boundary as bad" $
            qualityTierLabel def 25 `shouldBe` Just "bad"
        it "labels just under 25 as atrocious" $
            qualityTierLabel def 24.9 `shouldBe` Just "atrocious"
        it "labels 0 as atrocious" $
            qualityTierLabel def 0 `shouldBe` Just "atrocious"

    describe "qualityTierLabel (per-def override)" $ do
        let custom = [ QualityTier 80 "masterwork", QualityTier 0 "crude" ]
            def    = blankDef custom
        it "uses the def's own table instead of the default" $
            qualityTierLabel def 95 `shouldBe` Just "masterwork"
        it "falls through the override table's own bands" $
            qualityTierLabel def 10 `shouldBe` Just "crude"
        it "ignores an unrelated quality with an empty override" $
            -- An empty override list still falls back to the default —
            -- there's always a 0-floor band to land on.
            qualityTierLabel (blankDef []) 60 `shouldBe` Just "average"

    describe "ItemYamlQualityTier YAML parsing" $ do
        it "parses a { min, label } entry" $
            decode "{ min: 90, label: excellent }"
              `shouldBe` Right ItemYamlQualityTier
                  { iyqtMin = 90, iyqtLabel = "excellent" }
        it "requires both min and label" $
            decode "{ min: 90 }"
              `shouldSatisfy` either (const True) (const False)
