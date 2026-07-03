{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Pure tests for the item-buff YAML schema (#392): the `percent`
--   field parses (fractional, matching the unit-level modifiers
--   block), `amount` became optional so a buff can declare either or
--   both, and existing amount-only YAML keeps parsing unchanged.
module Test.Headless.Item.BuffYaml (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import Engine.Asset.YamlItems (ItemYamlBuff(..))

decode ∷ BS.ByteString → Either String ItemYamlBuff
decode = either (Left . show) Right . Yaml.decodeEither'

spec ∷ Spec
spec = do
    describe "ItemYamlBuff percent parsing (#392)" $ do
        it "parses a buff declaring amount, percent and scaling" $
            decode "{ stat: perception, amount: 1, percent: 0.1,\
                    \ scales_with_condition: true }"
              `shouldBe` Right ItemYamlBuff
                  { iybStat = "perception", iybAmount = 1
                  , iybPercent = 0.1, iybScalesWithCondition = True }
        it "percent defaults to 0 (existing amount-only YAML unchanged)" $
            -- The technogoggles shape.
            decode "{ stat: perception, amount: 1,\
                    \ scales_with_condition: true }"
              `shouldBe` Right ItemYamlBuff
                  { iybStat = "perception", iybAmount = 1
                  , iybPercent = 0, iybScalesWithCondition = True }
        it "amount is optional: a percent-only buff parses" $
            decode "{ stat: strength, percent: 0.25 }"
              `shouldBe` Right ItemYamlBuff
                  { iybStat = "strength", iybAmount = 0
                  , iybPercent = 0.25, iybScalesWithCondition = False }
        it "stat stays required" $
            decode "{ amount: 1, percent: 0.1 }"
              `shouldSatisfy` either (const True) (const False)
